use std::collections::HashMap;
use std::ptr;

use anyhow::{Result, bail};
use cranelift_jit::JITBuilder;

use crate::builtins::BuiltinFunction;
use crate::runtime::error::RuntimeError;
use crate::runtime::object::CallContext;
use crate::runtime::value::Value;

pub(super) const SYMBOL_RUNTIME_MAKE_INT: &str = "runtime_make_int";
pub(super) const SYMBOL_RUNTIME_MAKE_BOOL: &str = "runtime_make_bool";
pub(super) const SYMBOL_RUNTIME_MAKE_STRING: &str = "runtime_make_string";
pub(super) const SYMBOL_RUNTIME_MAKE_NONE: &str = "runtime_make_none";
pub(super) const SYMBOL_RUNTIME_MAKE_FUNCTION: &str = "runtime_make_function";
pub(super) const SYMBOL_RUNTIME_MAKE_LIST: &str = "runtime_make_list";
pub(super) const SYMBOL_RUNTIME_DEFINE_CLASS: &str = "runtime_define_class";
pub(super) const SYMBOL_RUNTIME_ADD: &str = "runtime_add";
pub(super) const SYMBOL_RUNTIME_SUB: &str = "runtime_sub";
pub(super) const SYMBOL_RUNTIME_LESS_THAN: &str = "runtime_less_than";
pub(super) const SYMBOL_RUNTIME_IS_TRUTHY: &str = "runtime_is_truthy";
pub(super) const SYMBOL_RUNTIME_CALL: &str = "runtime_call";
pub(super) const SYMBOL_RUNTIME_LOAD_NAME: &str = "runtime_load_name";
pub(super) const SYMBOL_RUNTIME_STORE_NAME: &str = "runtime_store_name";
pub(super) const SYMBOL_RUNTIME_LOAD_ATTR: &str = "runtime_load_attr";
pub(super) const SYMBOL_RUNTIME_STORE_ATTR: &str = "runtime_store_attr";
pub(super) const SYMBOL_RUNTIME_LOAD_INDEX: &str = "runtime_load_index";
pub(super) const SYMBOL_RUNTIME_STORE_INDEX_VALUE: &str = "runtime_store_index_value";
pub(super) const SYMBOL_RUNTIME_STORE_INDEX_NAME: &str = "runtime_store_index_name";

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
    functions: HashMap<String, CompiledFunctionPointer>,
    error: Option<String>,
    runtime_error: Option<RuntimeError>,
}

impl Runtime {
    fn new(functions: HashMap<String, CompiledFunctionPointer>) -> Self {
        Self {
            globals: HashMap::new(),
            output: Vec::new(),
            values: Vec::new(),
            functions,
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
    runtime.alloc_value(Value::int_object(value))
}

unsafe extern "C" fn runtime_make_bool(ctx: *mut Runtime, value: u8) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    runtime.alloc_value(Value::bool_object(value != 0))
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
    runtime.alloc_value(Value::none_object())
}

unsafe extern "C" fn runtime_make_function(
    ctx: *mut Runtime,
    ptr: *const u8,
    len: i64,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    let symbol = decode_runtime_string(ptr, len);
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

    let class_name = decode_runtime_string(class_name_ptr, class_name_len);
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

        let method_name = decode_runtime_string(method_name_ptr, method_name_len);
        let method_symbol = decode_runtime_string(method_symbol_ptr, method_symbol_len);
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
    let left = unsafe { (&*left).clone() };
    let right = unsafe { (&*right).clone() };
    match left.add(runtime, right) {
        Ok(value) => runtime.alloc_value(value),
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
    let left = unsafe { (&*left).clone() };
    let right = unsafe { (&*right).clone() };
    match left.sub(runtime, right) {
        Ok(value) => runtime.alloc_value(value),
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
    let left = unsafe { (&*left).clone() };
    let right = unsafe { (&*right).clone() };
    match left.less_than(runtime, right) {
        Ok(value) => runtime.alloc_value(value),
        Err(error) => {
            runtime.set_runtime_error(error);
            ptr::null_mut()
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
        Ok(value) => runtime.alloc_value(value),
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
    let name = decode_runtime_string(ptr, len);
    if let Some(value) = runtime.globals.get(&name).cloned() {
        return runtime.alloc_value(value);
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
    let name = decode_runtime_string(ptr, len);
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
    let attribute = decode_runtime_string(ptr, len);
    let object = unsafe { (&*object).clone() };
    match object.get_attribute(&attribute) {
        Ok(value) => runtime.alloc_value(value),
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
    let attribute = decode_runtime_string(ptr, len);
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
        Ok(value) => runtime.alloc_value(value),
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

    let name = decode_runtime_string(ptr, len);
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

pub(super) fn register_runtime_symbols(builder: &mut JITBuilder) {
    builder.symbol(SYMBOL_RUNTIME_MAKE_INT, runtime_make_int as *const u8);
    builder.symbol(SYMBOL_RUNTIME_MAKE_BOOL, runtime_make_bool as *const u8);
    builder.symbol(SYMBOL_RUNTIME_MAKE_STRING, runtime_make_string as *const u8);
    builder.symbol(SYMBOL_RUNTIME_MAKE_NONE, runtime_make_none as *const u8);
    builder.symbol(
        SYMBOL_RUNTIME_MAKE_FUNCTION,
        runtime_make_function as *const u8,
    );
    builder.symbol(SYMBOL_RUNTIME_MAKE_LIST, runtime_make_list as *const u8);
    builder.symbol(
        SYMBOL_RUNTIME_DEFINE_CLASS,
        runtime_define_class as *const u8,
    );
    builder.symbol(SYMBOL_RUNTIME_ADD, runtime_add as *const u8);
    builder.symbol(SYMBOL_RUNTIME_SUB, runtime_sub as *const u8);
    builder.symbol(SYMBOL_RUNTIME_LESS_THAN, runtime_less_than as *const u8);
    builder.symbol(SYMBOL_RUNTIME_IS_TRUTHY, runtime_is_truthy as *const u8);
    builder.symbol(SYMBOL_RUNTIME_CALL, runtime_call as *const u8);
    builder.symbol(SYMBOL_RUNTIME_LOAD_NAME, runtime_load_name as *const u8);
    builder.symbol(SYMBOL_RUNTIME_STORE_NAME, runtime_store_name as *const u8);
    builder.symbol(SYMBOL_RUNTIME_LOAD_ATTR, runtime_load_attr as *const u8);
    builder.symbol(SYMBOL_RUNTIME_STORE_ATTR, runtime_store_attr as *const u8);
    builder.symbol(SYMBOL_RUNTIME_LOAD_INDEX, runtime_load_index as *const u8);
    builder.symbol(
        SYMBOL_RUNTIME_STORE_INDEX_VALUE,
        runtime_store_index_value as *const u8,
    );
    builder.symbol(
        SYMBOL_RUNTIME_STORE_INDEX_NAME,
        runtime_store_index_name as *const u8,
    );
}

pub(super) fn run_prepared(
    entry: EntryFunction,
    functions: HashMap<String, CompiledFunctionPointer>,
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
