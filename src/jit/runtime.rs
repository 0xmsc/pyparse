use std::collections::HashMap;
use std::ptr;
use std::sync::Arc;

use anyhow::{Result, bail};
use cranelift_jit::JITBuilder;

use crate::builtins::BuiltinFunction;
use crate::runtime::error::RuntimeError;
use crate::runtime::object::CallContext;
use crate::runtime::value::Value;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(super) enum RuntimeFunctionId {
    MakeInt,
    MakeBool,
    MakeString,
    MakeNone,
    MakeFunction,
    MakeList,
    DefineClass,
    Add,
    Sub,
    LessThan,
    LessThanTruthy,
    IsTruthy,
    Call,
    LoadName,
    StoreName,
    LoadAttr,
    StoreAttr,
    LoadIndex,
    StoreIndexValue,
    StoreIndexName,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(super) enum RuntimeFunctionSignature {
    CtxI64ToValue,
    CtxI8ToValue,
    CtxPtrI64ToValue,
    CtxToValue,
    CtxPtrPtrI64ToValue,
    CtxDefineClassToValue,
    CtxValueValueToValue,
    CtxValueValueToI8,
    ValueToI8,
    CtxPtrI64ValueToVoid,
    CtxValuePtrI64ToValue,
    CtxValuePtrI64ValueToVoid,
    CtxValueValueValueToVoid,
    CtxPtrI64ValueValueToVoid,
}

#[derive(Clone, Copy, Debug)]
pub(super) struct RuntimeFunctionSpec {
    pub(super) id: RuntimeFunctionId,
    pub(super) symbol: &'static str,
    pub(super) function: *const u8,
    pub(super) signature: RuntimeFunctionSignature,
}

const MIN_INTERNED_INT: i64 = -4096;
const MAX_INTERNED_INT: i64 = 16384;

pub(super) type RuntimeValue = Value;
pub(super) type EntryFunction =
    extern "C" fn(*mut Runtime, *const *mut RuntimeValue) -> *mut RuntimeValue;

#[derive(Clone, Copy)]
pub(super) struct CompiledFunctionPointer {
    pub(super) entry: EntryFunction,
    pub(super) arity: usize,
}

pub(super) struct Runtime {
    globals: HashMap<String, RuntimeValue>,
    output: Vec<String>,
    #[allow(clippy::vec_box)]
    values: Vec<Box<RuntimeValue>>,
    #[allow(clippy::vec_box)]
    interned_values: Vec<Box<RuntimeValue>>,
    int_intern: HashMap<i64, usize>,
    functions: Arc<HashMap<String, CompiledFunctionPointer>>,
    symbol_cache: HashMap<(usize, i64), String>,
    error: Option<String>,
    runtime_error: Option<RuntimeError>,
}

impl Runtime {
    fn new(functions: Arc<HashMap<String, CompiledFunctionPointer>>) -> Self {
        let interned_values = vec![
            Box::new(Value::bool_object(false)),
            Box::new(Value::bool_object(true)),
            Box::new(Value::none_object()),
        ];
        Self {
            globals: HashMap::new(),
            output: Vec::new(),
            values: Vec::new(),
            interned_values,
            int_intern: HashMap::new(),
            functions,
            symbol_cache: HashMap::new(),
            error: None,
            runtime_error: None,
        }
    }

    fn alloc_value(&mut self, value: RuntimeValue) -> *mut RuntimeValue {
        let mut boxed = Box::new(value);
        let ptr = &mut *boxed as *mut RuntimeValue;
        self.values.push(boxed);
        ptr
    }

    fn interned_ptr(&mut self, index: usize) -> *mut RuntimeValue {
        &mut *self.interned_values[index] as *mut RuntimeValue
    }

    fn bool_ptr(&mut self, value: bool) -> *mut RuntimeValue {
        self.interned_ptr(if value { 1 } else { 0 })
    }

    fn none_ptr(&mut self) -> *mut RuntimeValue {
        self.interned_ptr(2)
    }

    fn int_ptr(&mut self, value: i64) -> *mut RuntimeValue {
        if !(MIN_INTERNED_INT..=MAX_INTERNED_INT).contains(&value) {
            return self.alloc_value(Value::int_object(value));
        }
        if let Some(index) = self.int_intern.get(&value).copied() {
            return self.interned_ptr(index);
        }
        let index = self.interned_values.len();
        self.interned_values
            .push(Box::new(Value::int_object(value)));
        self.int_intern.insert(value, index);
        self.interned_ptr(index)
    }

    fn alloc_or_intern_value(&mut self, value: RuntimeValue) -> *mut RuntimeValue {
        if let Some(integer) = value.as_int() {
            return self.int_ptr(integer);
        }
        if let Some(boolean) = value.as_bool() {
            return self.bool_ptr(boolean);
        }
        if value.is_none() {
            return self.none_ptr();
        }
        self.alloc_value(value)
    }

    fn set_error_message(&mut self, message: String) {
        if self.error.is_none() {
            self.error = Some(message);
        }
    }

    fn set_runtime_error(&mut self, error: RuntimeError) {
        if self.runtime_error.is_none() {
            self.runtime_error = Some(error.clone());
        }
        self.set_error_message(error.to_string());
    }

    fn decode_symbol(&mut self, ptr: *const u8, len: i64) -> String {
        let key = (ptr as usize, len);
        if let Some(symbol) = self.symbol_cache.get(&key) {
            return symbol.clone();
        }
        let symbol = decode_runtime_string(ptr, len);
        self.symbol_cache.insert(key, symbol.clone());
        symbol
    }
}

impl CallContext for Runtime {
    fn call_builtin(
        &mut self,
        builtin: BuiltinFunction,
        args: Vec<RuntimeValue>,
    ) -> std::result::Result<RuntimeValue, RuntimeError> {
        match builtin {
            BuiltinFunction::Print => {
                let rendered = args.iter().map(Value::to_output).collect::<Vec<_>>();
                self.output.push(rendered.join(" "));
                Ok(Value::none_object())
            }
            BuiltinFunction::Len => {
                RuntimeError::expect_function_arity("len", 1, args.len())?;
                args[0].len()
            }
        }
    }

    fn call_function_named(
        &mut self,
        name: &str,
        args: Vec<RuntimeValue>,
    ) -> std::result::Result<RuntimeValue, RuntimeError> {
        let function =
            self.functions
                .get(name)
                .copied()
                .ok_or_else(|| RuntimeError::UndefinedFunction {
                    name: name.to_string(),
                })?;
        RuntimeError::expect_function_arity(name, function.arity, args.len())?;

        let mut arg_ptrs = Vec::with_capacity(args.len());
        for arg in args {
            arg_ptrs.push(self.alloc_value(arg));
        }

        let runtime_ptr = self as *mut Runtime;
        let result = (function.entry)(runtime_ptr, arg_ptrs.as_ptr());
        if result.is_null() {
            if let Some(error) = self.runtime_error.clone() {
                return Err(error);
            }
            return Err(RuntimeError::UnsupportedOperation {
                operation: "call".to_string(),
                type_name: "function".to_string(),
            });
        }
        Ok(unsafe { (&*result).clone() })
    }
}

fn decode_runtime_string(ptr: *const u8, len: i64) -> String {
    let slice = unsafe { std::slice::from_raw_parts(ptr, len as usize) };
    String::from_utf8_lossy(slice).to_string()
}

unsafe extern "C" fn runtime_make_int(ctx: *mut Runtime, value: i64) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    runtime.int_ptr(value)
}

unsafe extern "C" fn runtime_make_bool(ctx: *mut Runtime, value: u8) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    runtime.bool_ptr(value != 0)
}

unsafe extern "C" fn runtime_make_string(
    ctx: *mut Runtime,
    ptr: *const u8,
    len: i64,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    runtime.alloc_value(Value::string_object(decode_runtime_string(ptr, len)))
}

unsafe extern "C" fn runtime_make_none(ctx: *mut Runtime) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    runtime.none_ptr()
}

unsafe extern "C" fn runtime_make_function(
    ctx: *mut Runtime,
    ptr: *const u8,
    len: i64,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    let symbol = runtime.decode_symbol(ptr, len);
    runtime.alloc_value(Value::function_object(symbol))
}

unsafe extern "C" fn runtime_make_list(
    ctx: *mut Runtime,
    values: *const *mut RuntimeValue,
    count: i64,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    if count < 0 {
        runtime.set_error_message("List element count cannot be negative".to_string());
        return ptr::null_mut();
    }
    let count = count as usize;
    if count > 0 && values.is_null() {
        runtime.set_error_message("List elements cannot be null".to_string());
        return ptr::null_mut();
    }

    let mut elements = Vec::with_capacity(count);
    for index in 0..count {
        let value_ptr = unsafe { *values.add(index) };
        if value_ptr.is_null() {
            runtime.set_error_message("List element cannot be null".to_string());
            return ptr::null_mut();
        }
        elements.push(unsafe { (&*value_ptr).clone() });
    }
    runtime.alloc_value(Value::list_object(elements))
}

unsafe extern "C" fn runtime_define_class(
    ctx: *mut Runtime,
    class_name_ptr: *const u8,
    class_name_len: i64,
    method_name_ptrs: *const *const u8,
    method_name_lens: *const i64,
    method_symbol_ptrs: *const *const u8,
    method_symbol_lens: *const i64,
    count: i64,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    if count < 0 {
        runtime.set_error_message("Class method count cannot be negative".to_string());
        return ptr::null_mut();
    }
    let count = count as usize;
    if count > 0
        && (method_name_ptrs.is_null()
            || method_name_lens.is_null()
            || method_symbol_ptrs.is_null()
            || method_symbol_lens.is_null())
    {
        runtime.set_error_message("Class methods cannot be null".to_string());
        return ptr::null_mut();
    }

    let class_name = runtime.decode_symbol(class_name_ptr, class_name_len);
    let mut methods = HashMap::with_capacity(count);
    for index in 0..count {
        let method_name_ptr = unsafe { *method_name_ptrs.add(index) };
        let method_name_len = unsafe { *method_name_lens.add(index) };
        let method_symbol_ptr = unsafe { *method_symbol_ptrs.add(index) };
        let method_symbol_len = unsafe { *method_symbol_lens.add(index) };

        if method_name_ptr.is_null() || method_symbol_ptr.is_null() {
            runtime.set_error_message("Class method entries cannot be null".to_string());
            return ptr::null_mut();
        }

        let method_name = runtime.decode_symbol(method_name_ptr, method_name_len);
        let method_symbol = runtime.decode_symbol(method_symbol_ptr, method_symbol_len);
        methods.insert(method_name, Value::function_object(method_symbol));
    }
    runtime.alloc_value(Value::class_object(class_name, methods))
}

unsafe extern "C" fn runtime_add(
    ctx: *mut Runtime,
    left: *mut RuntimeValue,
    right: *mut RuntimeValue,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    let left = unsafe { &*left };
    let right = unsafe { &*right };
    if let (Some(left_int), Some(right_int)) = (left.as_int(), right.as_int()) {
        return runtime.int_ptr(left_int + right_int);
    }
    let left = left.clone();
    let right = right.clone();
    match left.add(runtime, right) {
        Ok(value) => runtime.alloc_or_intern_value(value),
        Err(error) => {
            runtime.set_runtime_error(error);
            ptr::null_mut()
        }
    }
}

unsafe extern "C" fn runtime_sub(
    ctx: *mut Runtime,
    left: *mut RuntimeValue,
    right: *mut RuntimeValue,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    let left = unsafe { &*left };
    let right = unsafe { &*right };
    if let (Some(left_int), Some(right_int)) = (left.as_int(), right.as_int()) {
        return runtime.int_ptr(left_int - right_int);
    }
    let left = left.clone();
    let right = right.clone();
    match left.sub(runtime, right) {
        Ok(value) => runtime.alloc_or_intern_value(value),
        Err(error) => {
            runtime.set_runtime_error(error);
            ptr::null_mut()
        }
    }
}

unsafe extern "C" fn runtime_less_than(
    ctx: *mut Runtime,
    left: *mut RuntimeValue,
    right: *mut RuntimeValue,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    let left = unsafe { &*left };
    let right = unsafe { &*right };
    if let (Some(left_int), Some(right_int)) = (left.as_int(), right.as_int()) {
        return runtime.bool_ptr(left_int < right_int);
    }
    let left = left.clone();
    let right = right.clone();
    match left.less_than(runtime, right) {
        Ok(value) => runtime.alloc_or_intern_value(value),
        Err(error) => {
            runtime.set_runtime_error(error);
            ptr::null_mut()
        }
    }
}

unsafe extern "C" fn runtime_less_than_truthy(
    ctx: *mut Runtime,
    left: *mut RuntimeValue,
    right: *mut RuntimeValue,
) -> u8 {
    let runtime = unsafe { &mut *ctx };
    let left = unsafe { &*left };
    let right = unsafe { &*right };
    if let (Some(left_int), Some(right_int)) = (left.as_int(), right.as_int()) {
        return if left_int < right_int { 1 } else { 0 };
    }

    let left = left.clone();
    let right = right.clone();
    match left.less_than(runtime, right) {
        Ok(value) => {
            if value.is_truthy() {
                1
            } else {
                0
            }
        }
        Err(error) => {
            runtime.set_runtime_error(error);
            2
        }
    }
}

unsafe extern "C" fn runtime_is_truthy(value: *mut RuntimeValue) -> u8 {
    if value.is_null() {
        return 0;
    }
    let value = unsafe { &*value };
    if value.is_truthy() { 1 } else { 0 }
}

unsafe extern "C" fn runtime_call(
    ctx: *mut Runtime,
    callee: *mut RuntimeValue,
    args: *const *mut RuntimeValue,
    count: i64,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    if count < 0 {
        runtime.set_error_message("Call argument count cannot be negative".to_string());
        return ptr::null_mut();
    }
    if callee.is_null() {
        runtime.set_error_message("Call target cannot be null".to_string());
        return ptr::null_mut();
    }

    let argc = count as usize;
    if argc > 0 && args.is_null() {
        runtime.set_error_message("Call arguments cannot be null".to_string());
        return ptr::null_mut();
    }

    let mut evaluated_args = Vec::with_capacity(argc);
    for index in 0..argc {
        let value_ptr = unsafe { *args.add(index) };
        if value_ptr.is_null() {
            runtime.set_error_message("Call argument cannot be null".to_string());
            return ptr::null_mut();
        }
        evaluated_args.push(unsafe { (&*value_ptr).clone() });
    }

    let callee_value = unsafe { (&*callee).clone() };
    match callee_value.call(runtime, evaluated_args) {
        Ok(value) => runtime.alloc_or_intern_value(value),
        Err(error) => {
            runtime.set_runtime_error(error);
            ptr::null_mut()
        }
    }
}

unsafe extern "C" fn runtime_load_name(
    ctx: *mut Runtime,
    ptr: *const u8,
    len: i64,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    let name = runtime.decode_symbol(ptr, len);
    if let Some(value) = runtime.globals.get(&name).cloned() {
        return runtime.alloc_or_intern_value(value);
    }
    if let Some(builtin) = BuiltinFunction::from_name(&name) {
        return runtime.alloc_value(Value::builtin_function_object(builtin));
    }
    runtime.set_runtime_error(RuntimeError::UndefinedVariable { name });
    ptr::null_mut()
}

unsafe extern "C" fn runtime_store_name(
    ctx: *mut Runtime,
    ptr: *const u8,
    len: i64,
    value: *mut RuntimeValue,
) {
    let runtime = unsafe { &mut *ctx };
    if value.is_null() {
        runtime.set_error_message("Cannot store null value".to_string());
        return;
    }
    let name = runtime.decode_symbol(ptr, len);
    runtime.globals.insert(name, unsafe { (&*value).clone() });
}

unsafe extern "C" fn runtime_load_attr(
    ctx: *mut Runtime,
    object: *mut RuntimeValue,
    ptr: *const u8,
    len: i64,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    if object.is_null() {
        runtime.set_error_message("Cannot load attribute from null value".to_string());
        return ptr::null_mut();
    }
    let attribute = runtime.decode_symbol(ptr, len);
    let object = unsafe { (&*object).clone() };
    match object.get_attribute(&attribute) {
        Ok(value) => runtime.alloc_or_intern_value(value),
        Err(error) => {
            runtime.set_runtime_error(error);
            ptr::null_mut()
        }
    }
}

unsafe extern "C" fn runtime_store_attr(
    ctx: *mut Runtime,
    object: *mut RuntimeValue,
    ptr: *const u8,
    len: i64,
    value: *mut RuntimeValue,
) {
    let runtime = unsafe { &mut *ctx };
    if object.is_null() {
        runtime.set_error_message("Cannot store attribute on null value".to_string());
        return;
    }
    if value.is_null() {
        runtime.set_error_message("Cannot store null attribute value".to_string());
        return;
    }
    let attribute = runtime.decode_symbol(ptr, len);
    let object = unsafe { (&*object).clone() };
    let value = unsafe { (&*value).clone() };
    if let Err(error) = object.set_attribute(&attribute, value) {
        runtime.set_runtime_error(error);
    }
}

unsafe extern "C" fn runtime_load_index(
    ctx: *mut Runtime,
    object: *mut RuntimeValue,
    index: *mut RuntimeValue,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    if object.is_null() {
        runtime.set_error_message("Cannot index null value".to_string());
        return ptr::null_mut();
    }
    if index.is_null() {
        runtime.set_error_message("List index cannot be null".to_string());
        return ptr::null_mut();
    }

    let object = unsafe { (&*object).clone() };
    let index = unsafe { (&*index).clone() };
    match object.get_item_with_context(runtime, index) {
        Ok(value) => runtime.alloc_or_intern_value(value),
        Err(error) => {
            runtime.set_runtime_error(error);
            ptr::null_mut()
        }
    }
}

unsafe extern "C" fn runtime_store_index_value(
    ctx: *mut Runtime,
    target: *mut RuntimeValue,
    index: *mut RuntimeValue,
    value: *mut RuntimeValue,
) {
    let runtime = unsafe { &mut *ctx };
    if target.is_null() {
        runtime.set_error_message("Cannot assign index on null value".to_string());
        return;
    }
    if index.is_null() {
        runtime.set_error_message("Cannot assign null index".to_string());
        return;
    }
    if value.is_null() {
        runtime.set_error_message("Cannot assign null value to index".to_string());
        return;
    }

    let target = unsafe { (&*target).clone() };
    let index = unsafe { (&*index).clone() };
    let value = unsafe { (&*value).clone() };
    if let Err(error) = target.set_item_with_context(runtime, index, value) {
        runtime.set_runtime_error(error);
    }
}

unsafe extern "C" fn runtime_store_index_name(
    ctx: *mut Runtime,
    ptr: *const u8,
    len: i64,
    index: *mut RuntimeValue,
    value: *mut RuntimeValue,
) {
    let runtime = unsafe { &mut *ctx };
    if index.is_null() {
        runtime.set_error_message("Cannot assign null index".to_string());
        return;
    }
    if value.is_null() {
        runtime.set_error_message("Cannot assign null value to index".to_string());
        return;
    }

    let name = runtime.decode_symbol(ptr, len);
    let Some(target) = runtime.globals.get(&name).cloned() else {
        runtime.set_runtime_error(RuntimeError::UndefinedVariable { name });
        return;
    };
    let index = unsafe { (&*index).clone() };
    let value = unsafe { (&*value).clone() };
    if let Err(error) = target.set_item_with_context(runtime, index, value) {
        runtime.set_runtime_error(error);
    }
}

pub(super) fn runtime_function_specs() -> [RuntimeFunctionSpec; 20] {
    [
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::MakeInt,
            symbol: "runtime_make_int",
            function: runtime_make_int as *const u8,
            signature: RuntimeFunctionSignature::CtxI64ToValue,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::MakeBool,
            symbol: "runtime_make_bool",
            function: runtime_make_bool as *const u8,
            signature: RuntimeFunctionSignature::CtxI8ToValue,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::MakeString,
            symbol: "runtime_make_string",
            function: runtime_make_string as *const u8,
            signature: RuntimeFunctionSignature::CtxPtrI64ToValue,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::MakeNone,
            symbol: "runtime_make_none",
            function: runtime_make_none as *const u8,
            signature: RuntimeFunctionSignature::CtxToValue,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::MakeFunction,
            symbol: "runtime_make_function",
            function: runtime_make_function as *const u8,
            signature: RuntimeFunctionSignature::CtxPtrI64ToValue,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::MakeList,
            symbol: "runtime_make_list",
            function: runtime_make_list as *const u8,
            signature: RuntimeFunctionSignature::CtxPtrI64ToValue,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::DefineClass,
            symbol: "runtime_define_class",
            function: runtime_define_class as *const u8,
            signature: RuntimeFunctionSignature::CtxDefineClassToValue,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::Add,
            symbol: "runtime_add",
            function: runtime_add as *const u8,
            signature: RuntimeFunctionSignature::CtxValueValueToValue,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::Sub,
            symbol: "runtime_sub",
            function: runtime_sub as *const u8,
            signature: RuntimeFunctionSignature::CtxValueValueToValue,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::LessThan,
            symbol: "runtime_less_than",
            function: runtime_less_than as *const u8,
            signature: RuntimeFunctionSignature::CtxValueValueToValue,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::LessThanTruthy,
            symbol: "runtime_less_than_truthy",
            function: runtime_less_than_truthy as *const u8,
            signature: RuntimeFunctionSignature::CtxValueValueToI8,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::IsTruthy,
            symbol: "runtime_is_truthy",
            function: runtime_is_truthy as *const u8,
            signature: RuntimeFunctionSignature::ValueToI8,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::Call,
            symbol: "runtime_call",
            function: runtime_call as *const u8,
            signature: RuntimeFunctionSignature::CtxPtrPtrI64ToValue,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::LoadName,
            symbol: "runtime_load_name",
            function: runtime_load_name as *const u8,
            signature: RuntimeFunctionSignature::CtxPtrI64ToValue,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::StoreName,
            symbol: "runtime_store_name",
            function: runtime_store_name as *const u8,
            signature: RuntimeFunctionSignature::CtxPtrI64ValueToVoid,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::LoadAttr,
            symbol: "runtime_load_attr",
            function: runtime_load_attr as *const u8,
            signature: RuntimeFunctionSignature::CtxValuePtrI64ToValue,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::StoreAttr,
            symbol: "runtime_store_attr",
            function: runtime_store_attr as *const u8,
            signature: RuntimeFunctionSignature::CtxValuePtrI64ValueToVoid,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::LoadIndex,
            symbol: "runtime_load_index",
            function: runtime_load_index as *const u8,
            signature: RuntimeFunctionSignature::CtxValueValueToValue,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::StoreIndexValue,
            symbol: "runtime_store_index_value",
            function: runtime_store_index_value as *const u8,
            signature: RuntimeFunctionSignature::CtxValueValueValueToVoid,
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::StoreIndexName,
            symbol: "runtime_store_index_name",
            function: runtime_store_index_name as *const u8,
            signature: RuntimeFunctionSignature::CtxPtrI64ValueValueToVoid,
        },
    ]
}

pub(super) fn register_runtime_symbols(builder: &mut JITBuilder) {
    for spec in runtime_function_specs() {
        builder.symbol(spec.symbol, spec.function);
    }
}

pub(super) fn run_prepared(
    entry: EntryFunction,
    functions: Arc<HashMap<String, CompiledFunctionPointer>>,
) -> Result<String> {
    let mut runtime = Runtime::new(functions);
    let result = (entry)(&mut runtime as *mut Runtime, ptr::null());
    if let Some(error) = runtime.error {
        bail!("{error}");
    }
    if result.is_null() {
        bail!("JIT execution failed");
    }
    Ok(runtime.output.join("\n"))
}
