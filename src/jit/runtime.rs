use std::collections::HashMap;
use std::ptr;
use std::sync::Arc;

use anyhow::{Result, bail};
use cranelift_jit::JITBuilder;
use rustc_hash::FxHashMap;

use crate::builtins::BuiltinFunction;
use crate::runtime::call_registry::{CallRegistry, RegisteredCallable};
use crate::runtime::error::RuntimeError;
use crate::runtime::execution::call_builtin_with_output;
use crate::runtime::object::{CallContext, CallableId};
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
    IsTruthy,
    Call,
    LoadName,
    StoreName,
    LoadAttr,
    StoreAttr,
    LoadIndex,
    StoreIndexName,
}

#[derive(Clone, Copy, Debug)]
pub(super) struct RuntimeFunctionSpec {
    pub(super) id: RuntimeFunctionId,
    pub(super) symbol: &'static str,
    pub(super) function: *const u8,
    pub(super) param_types: &'static [RuntimeAbiType],
    pub(super) return_types: &'static [RuntimeAbiType],
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(super) enum RuntimeAbiType {
    Ptr,
    I64,
    I8,
}

const MIN_INTERNED_INT: i64 = -1_000_000;
const MAX_INTERNED_INT: i64 = 1_000_000;

pub(super) type EntryFunction = extern "C" fn(*mut Runtime, *const *mut Value) -> *mut Value;

/// Metadata for a compiled JIT function callable from runtime dispatch.
#[derive(Clone)]
pub(super) struct CompiledFunctionPointer {
    pub(super) name: String,
    pub(super) entry: EntryFunction,
    pub(super) arity: usize,
    pub(super) params: Vec<String>,
}

#[derive(Clone)]
struct RegisteredFunction {
    function: CompiledFunctionPointer,
}

/// Runtime state used by JIT-emitted code and runtime hooks.
pub(super) struct Runtime {
    globals: FxHashMap<u32, Value>,
    call_frames: Vec<FxHashMap<u32, Value>>,
    output: Vec<String>,
    #[allow(clippy::vec_box)]
    values: Vec<Box<Value>>,
    #[allow(clippy::vec_box)]
    interned_values: Vec<Box<Value>>,
    int_intern: FxHashMap<i64, usize>,
    functions: Arc<Vec<CompiledFunctionPointer>>,
    call_registry: CallRegistry<RegisteredFunction>,
    symbol_ids_by_ptr: FxHashMap<(usize, i64), u32>,
    symbol_ids_by_name: FxHashMap<String, u32>,
    symbol_names: Vec<String>,
    error: Option<String>,
    runtime_error: Option<RuntimeError>,
}

enum RuntimeValueSnapshot {
    Int(i64),
    Bool(bool),
    None,
    Owned(Value),
}

#[cfg(test)]
#[derive(Debug, Clone)]
enum NameResolution {
    Value(Value),
    Missing,
}

#[cfg(test)]
fn bind_call_frame(params: &[String], args: &[Value]) -> FxHashMap<String, Value> {
    params
        .iter()
        .cloned()
        .zip(args.iter().cloned())
        .collect::<FxHashMap<_, _>>()
}

#[cfg(test)]
fn resolve_name(local_value: Option<Value>, global_value: Option<Value>) -> NameResolution {
    if let Some(value) = local_value {
        return NameResolution::Value(value);
    }
    if let Some(value) = global_value {
        return NameResolution::Value(value);
    }
    NameResolution::Missing
}

impl Runtime {
    fn new(functions: Arc<Vec<CompiledFunctionPointer>>) -> Self {
        let interned_values = vec![
            Box::new(Value::bool_object(false)),
            Box::new(Value::bool_object(true)),
            Box::new(Value::none_object()),
        ];
        let mut runtime = Self {
            globals: FxHashMap::default(),
            call_frames: Vec::new(),
            output: Vec::new(),
            values: Vec::new(),
            interned_values,
            int_intern: FxHashMap::default(),
            functions,
            call_registry: CallRegistry::new(),
            symbol_ids_by_ptr: FxHashMap::default(),
            symbol_ids_by_name: FxHashMap::default(),
            symbol_names: Vec::new(),
            error: None,
            runtime_error: None,
        };
        runtime.seed_builtin_globals();
        runtime
    }

    fn seed_builtin_globals(&mut self) {
        for builtin in [BuiltinFunction::Print, BuiltinFunction::Len] {
            let symbol = self.intern_symbol_name(builtin.name());
            self.globals
                .entry(symbol)
                .or_insert_with(|| Value::builtin_function_object(builtin));
        }
    }

    fn register_function(
        &mut self,
        compiled_callable_id: u32,
    ) -> Result<(String, CallableId), RuntimeError> {
        let function = self
            .functions
            .get(compiled_callable_id as usize)
            .cloned()
            .ok_or_else(|| RuntimeError::UndefinedFunction {
                name: format!("<callable:{compiled_callable_id}>"),
            })?;
        let callable_id = self.call_registry.register_function(RegisteredFunction {
            function: function.clone(),
        });
        let function_name = function.name.clone();
        Ok((function_name, callable_id))
    }

    fn alloc_value(&mut self, value: Value) -> *mut Value {
        let mut boxed = Box::new(value);
        let ptr = &mut *boxed as *mut Value;
        self.values.push(boxed);
        ptr
    }

    fn interned_ptr(&mut self, index: usize) -> *mut Value {
        &mut *self.interned_values[index] as *mut Value
    }

    fn bool_ptr(&mut self, value: bool) -> *mut Value {
        self.interned_ptr(if value { 1 } else { 0 })
    }

    fn none_ptr(&mut self) -> *mut Value {
        self.interned_ptr(2)
    }

    fn int_ptr(&mut self, value: i64) -> *mut Value {
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

    fn alloc_or_intern_value(&mut self, value: Value) -> *mut Value {
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

    fn alloc_snapshot(&mut self, value: RuntimeValueSnapshot) -> *mut Value {
        match value {
            RuntimeValueSnapshot::Int(integer) => self.int_ptr(integer),
            RuntimeValueSnapshot::Bool(boolean) => self.bool_ptr(boolean),
            RuntimeValueSnapshot::None => self.none_ptr(),
            RuntimeValueSnapshot::Owned(value) => self.alloc_value(value),
        }
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

    fn intern_symbol_name(&mut self, name: &str) -> u32 {
        if let Some(symbol) = self.symbol_ids_by_name.get(name) {
            return *symbol;
        }
        let symbol = self.symbol_names.len() as u32;
        self.symbol_names.push(name.to_string());
        self.symbol_ids_by_name.insert(name.to_string(), symbol);
        symbol
    }

    fn intern_symbol_ptr(&mut self, ptr: *const u8, len: i64) -> u32 {
        let key = (ptr as usize, len);
        if let Some(symbol) = self.symbol_ids_by_ptr.get(&key) {
            return *symbol;
        }
        let name = decode_runtime_string(ptr, len);
        let symbol = self.intern_symbol_name(&name);
        self.symbol_ids_by_ptr.insert(key, symbol);
        symbol
    }

    fn symbol_name(&self, symbol: u32) -> &str {
        self.symbol_names
            .get(symbol as usize)
            .expect("symbol id must exist")
            .as_str()
    }

    fn load_named_value(&self, symbol: u32) -> Option<Value> {
        if let Some(frame) = self.call_frames.last()
            && let Some(value) = frame.get(&symbol)
        {
            return Some(value.clone());
        }
        self.globals.get(&symbol).cloned()
    }

    fn store_named_value(&mut self, symbol: u32, value: Value) {
        if let Some(frame) = self.call_frames.last_mut() {
            if let Some(existing) = frame.get_mut(&symbol) {
                *existing = value;
            } else {
                frame.insert(symbol, value);
            }
        } else if let Some(existing) = self.globals.get_mut(&symbol) {
            *existing = value;
        } else {
            self.globals.insert(symbol, value);
        }
    }
}

impl CallContext for Runtime {
    fn call_callable(
        &mut self,
        callable_id: &CallableId,
        args: Vec<Value>,
    ) -> std::result::Result<Value, RuntimeError> {
        let callable = self.call_registry.resolve(callable_id)?;
        let callable_function = match callable {
            RegisteredCallable::Builtin(builtin) => {
                return call_builtin_with_output(builtin, args, &mut self.output);
            }
            RegisteredCallable::Function(callable_function) => callable_function,
        };
        let function = callable_function.function;
        RuntimeError::expect_function_arity(function.name.as_str(), function.arity, args.len())?;

        let mut arg_values = args;
        let mut frame =
            FxHashMap::with_capacity_and_hasher(function.params.len(), Default::default());
        for (param, arg) in function.params.iter().zip(arg_values.iter()) {
            let symbol = self.intern_symbol_name(param);
            frame.insert(symbol, arg.clone());
        }

        let mut arg_ptrs = Vec::with_capacity(arg_values.len());
        for arg in &mut arg_values {
            arg_ptrs.push(arg as *mut Value);
        }

        self.call_frames.push(frame);
        let runtime_ptr = self as *mut Runtime;
        let result = (function.entry)(runtime_ptr, arg_ptrs.as_ptr());
        self.call_frames.pop();
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

fn snapshot_value(value: &Value) -> RuntimeValueSnapshot {
    if let Some(integer) = value.as_int() {
        return RuntimeValueSnapshot::Int(integer);
    }
    if let Some(boolean) = value.as_bool() {
        return RuntimeValueSnapshot::Bool(boolean);
    }
    if value.is_none() {
        return RuntimeValueSnapshot::None;
    }
    RuntimeValueSnapshot::Owned(value.clone())
}

unsafe extern "C" fn runtime_make_int(ctx: *mut Runtime, value: i64) -> *mut Value {
    let runtime = unsafe { &mut *ctx };
    runtime.int_ptr(value)
}

unsafe extern "C" fn runtime_make_bool(ctx: *mut Runtime, value: u8) -> *mut Value {
    let runtime = unsafe { &mut *ctx };
    runtime.bool_ptr(value != 0)
}

unsafe extern "C" fn runtime_make_string(
    ctx: *mut Runtime,
    ptr: *const u8,
    len: i64,
) -> *mut Value {
    let runtime = unsafe { &mut *ctx };
    runtime.alloc_value(Value::string_object(decode_runtime_string(ptr, len)))
}

unsafe extern "C" fn runtime_make_none(ctx: *mut Runtime) -> *mut Value {
    let runtime = unsafe { &mut *ctx };
    runtime.none_ptr()
}

unsafe extern "C" fn runtime_make_function(
    ctx: *mut Runtime,
    compiled_callable_id: i64,
) -> *mut Value {
    let runtime = unsafe { &mut *ctx };
    let compiled_callable_id = match u32::try_from(compiled_callable_id) {
        Ok(callable_id) => callable_id,
        Err(_) => {
            runtime.set_error_message("Callable id must fit in u32".to_string());
            return ptr::null_mut();
        }
    };
    let (function_name, callable_id) = match runtime.register_function(compiled_callable_id) {
        Ok(value) => value,
        Err(error) => {
            runtime.set_runtime_error(error);
            return ptr::null_mut();
        }
    };
    runtime.alloc_value(Value::function_object(function_name, callable_id))
}

unsafe extern "C" fn runtime_make_list(
    ctx: *mut Runtime,
    values: *const *mut Value,
    count: i64,
) -> *mut Value {
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
    method_callable_ids: *const i64,
    count: i64,
) -> *mut Value {
    let runtime = unsafe { &mut *ctx };
    if count < 0 {
        runtime.set_error_message("Class method count cannot be negative".to_string());
        return ptr::null_mut();
    }
    let count = count as usize;
    if count > 0
        && (method_name_ptrs.is_null()
            || method_name_lens.is_null()
            || method_callable_ids.is_null())
    {
        runtime.set_error_message("Class methods cannot be null".to_string());
        return ptr::null_mut();
    }

    let class_symbol = runtime.intern_symbol_ptr(class_name_ptr, class_name_len);
    let class_name = runtime.symbol_name(class_symbol).to_string();
    let mut methods = HashMap::with_capacity(count);
    for index in 0..count {
        let method_name_ptr = unsafe { *method_name_ptrs.add(index) };
        let method_name_len = unsafe { *method_name_lens.add(index) };
        let method_callable_id = unsafe { *method_callable_ids.add(index) };

        if method_name_ptr.is_null() {
            runtime.set_error_message("Class method entries cannot be null".to_string());
            return ptr::null_mut();
        }

        let method_name_symbol = runtime.intern_symbol_ptr(method_name_ptr, method_name_len);
        let method_name = runtime.symbol_name(method_name_symbol).to_string();
        let method_callable_id = match u32::try_from(method_callable_id) {
            Ok(callable_id) => callable_id,
            Err(_) => {
                runtime.set_error_message("Callable id must fit in u32".to_string());
                return ptr::null_mut();
            }
        };
        let (function_name, callable_id) = match runtime.register_function(method_callable_id) {
            Ok(value) => value,
            Err(error) => {
                runtime.set_runtime_error(error);
                return ptr::null_mut();
            }
        };
        methods.insert(
            method_name,
            Value::function_object(function_name, callable_id),
        );
    }
    runtime.alloc_value(Value::class_object(class_name, methods))
}

unsafe extern "C" fn runtime_add(
    ctx: *mut Runtime,
    left: *mut Value,
    right: *mut Value,
) -> *mut Value {
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
    left: *mut Value,
    right: *mut Value,
) -> *mut Value {
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
    left: *mut Value,
    right: *mut Value,
) -> *mut Value {
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

unsafe extern "C" fn runtime_is_truthy(value: *mut Value) -> u8 {
    if value.is_null() {
        return 0;
    }
    let value = unsafe { &*value };
    if value.is_truthy() { 1 } else { 0 }
}

unsafe extern "C" fn runtime_call(
    ctx: *mut Runtime,
    callee: *mut Value,
    args: *const *mut Value,
    count: i64,
) -> *mut Value {
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

/// Runtime hook for name lookup (current call-frame locals, then globals).
unsafe extern "C" fn runtime_load_name(ctx: *mut Runtime, ptr: *const u8, len: i64) -> *mut Value {
    let runtime = unsafe { &mut *ctx };
    let symbol = runtime.intern_symbol_ptr(ptr, len);

    if let Some(value) = runtime
        .load_named_value(symbol)
        .map(|value| snapshot_value(&value))
    {
        return runtime.alloc_snapshot(value);
    }

    runtime.set_runtime_error(RuntimeError::UndefinedVariable {
        name: runtime.symbol_name(symbol).to_string(),
    });
    ptr::null_mut()
}

/// Runtime hook for name assignment (current call-frame locals or globals).
unsafe extern "C" fn runtime_store_name(
    ctx: *mut Runtime,
    ptr: *const u8,
    len: i64,
    value: *mut Value,
) {
    let runtime = unsafe { &mut *ctx };
    if value.is_null() {
        runtime.set_error_message("Cannot store null value".to_string());
        return;
    }
    let symbol = runtime.intern_symbol_ptr(ptr, len);
    runtime.store_named_value(symbol, unsafe { (&*value).clone() });
}

unsafe extern "C" fn runtime_load_attr(
    ctx: *mut Runtime,
    object: *mut Value,
    ptr: *const u8,
    len: i64,
) -> *mut Value {
    let runtime = unsafe { &mut *ctx };
    if object.is_null() {
        runtime.set_error_message("Cannot load attribute from null value".to_string());
        return ptr::null_mut();
    }
    let attribute_symbol = runtime.intern_symbol_ptr(ptr, len);
    let object = unsafe { (&*object).clone() };
    let result = object.get_attribute(runtime.symbol_name(attribute_symbol));
    match result {
        Ok(value) => runtime.alloc_or_intern_value(value),
        Err(error) => {
            runtime.set_runtime_error(error);
            ptr::null_mut()
        }
    }
}

unsafe extern "C" fn runtime_store_attr(
    ctx: *mut Runtime,
    object: *mut Value,
    ptr: *const u8,
    len: i64,
    value: *mut Value,
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
    let attribute_symbol = runtime.intern_symbol_ptr(ptr, len);
    let object = unsafe { (&*object).clone() };
    let value = unsafe { (&*value).clone() };
    if let Err(error) = object.set_attribute(runtime.symbol_name(attribute_symbol), value) {
        runtime.set_runtime_error(error);
    }
}

unsafe extern "C" fn runtime_load_index(
    ctx: *mut Runtime,
    object: *mut Value,
    index: *mut Value,
) -> *mut Value {
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

/// Runtime hook for `name[index] = value` assignment with runtime name resolution.
unsafe extern "C" fn runtime_store_index_name(
    ctx: *mut Runtime,
    ptr: *const u8,
    len: i64,
    index: *mut Value,
    value: *mut Value,
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

    let symbol = runtime.intern_symbol_ptr(ptr, len);
    let Some(target) = runtime.load_named_value(symbol) else {
        runtime.set_runtime_error(RuntimeError::UndefinedVariable {
            name: runtime.symbol_name(symbol).to_string(),
        });
        return;
    };
    let index = unsafe { (&*index).clone() };
    let value = unsafe { (&*value).clone() };
    if let Err(error) = target.set_item_with_context(runtime, index, value) {
        runtime.set_runtime_error(error);
    }
}

/// Returns the runtime hook table imported by JIT codegen.
pub(super) fn runtime_function_specs() -> [RuntimeFunctionSpec; 18] {
    use RuntimeAbiType::{I8, I64, Ptr};

    [
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::MakeInt,
            symbol: "runtime_make_int",
            function: runtime_make_int as *const u8,
            param_types: &[Ptr, I64],
            return_types: &[Ptr],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::MakeBool,
            symbol: "runtime_make_bool",
            function: runtime_make_bool as *const u8,
            param_types: &[Ptr, I8],
            return_types: &[Ptr],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::MakeString,
            symbol: "runtime_make_string",
            function: runtime_make_string as *const u8,
            param_types: &[Ptr, Ptr, I64],
            return_types: &[Ptr],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::MakeNone,
            symbol: "runtime_make_none",
            function: runtime_make_none as *const u8,
            param_types: &[Ptr],
            return_types: &[Ptr],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::MakeFunction,
            symbol: "runtime_make_function",
            function: runtime_make_function as *const u8,
            param_types: &[Ptr, I64],
            return_types: &[Ptr],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::MakeList,
            symbol: "runtime_make_list",
            function: runtime_make_list as *const u8,
            param_types: &[Ptr, Ptr, I64],
            return_types: &[Ptr],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::DefineClass,
            symbol: "runtime_define_class",
            function: runtime_define_class as *const u8,
            param_types: &[Ptr, Ptr, I64, Ptr, Ptr, Ptr, I64],
            return_types: &[Ptr],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::Add,
            symbol: "runtime_add",
            function: runtime_add as *const u8,
            param_types: &[Ptr, Ptr, Ptr],
            return_types: &[Ptr],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::Sub,
            symbol: "runtime_sub",
            function: runtime_sub as *const u8,
            param_types: &[Ptr, Ptr, Ptr],
            return_types: &[Ptr],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::LessThan,
            symbol: "runtime_less_than",
            function: runtime_less_than as *const u8,
            param_types: &[Ptr, Ptr, Ptr],
            return_types: &[Ptr],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::IsTruthy,
            symbol: "runtime_is_truthy",
            function: runtime_is_truthy as *const u8,
            param_types: &[Ptr],
            return_types: &[I8],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::Call,
            symbol: "runtime_call",
            function: runtime_call as *const u8,
            param_types: &[Ptr, Ptr, Ptr, I64],
            return_types: &[Ptr],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::LoadName,
            symbol: "runtime_load_name",
            function: runtime_load_name as *const u8,
            param_types: &[Ptr, Ptr, I64],
            return_types: &[Ptr],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::StoreName,
            symbol: "runtime_store_name",
            function: runtime_store_name as *const u8,
            param_types: &[Ptr, Ptr, I64, Ptr],
            return_types: &[],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::LoadAttr,
            symbol: "runtime_load_attr",
            function: runtime_load_attr as *const u8,
            param_types: &[Ptr, Ptr, Ptr, I64],
            return_types: &[Ptr],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::StoreAttr,
            symbol: "runtime_store_attr",
            function: runtime_store_attr as *const u8,
            param_types: &[Ptr, Ptr, Ptr, I64, Ptr],
            return_types: &[],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::LoadIndex,
            symbol: "runtime_load_index",
            function: runtime_load_index as *const u8,
            param_types: &[Ptr, Ptr, Ptr],
            return_types: &[Ptr],
        },
        RuntimeFunctionSpec {
            id: RuntimeFunctionId::StoreIndexName,
            symbol: "runtime_store_index_name",
            function: runtime_store_index_name as *const u8,
            param_types: &[Ptr, Ptr, I64, Ptr, Ptr],
            return_types: &[],
        },
    ]
}

/// Registers runtime hook symbols with Cranelift's JIT linker.
pub(super) fn register_runtime_symbols(builder: &mut JITBuilder) {
    for spec in runtime_function_specs() {
        builder.symbol(spec.symbol, spec.function);
    }
}

/// Executes the JIT entry function inside a fresh runtime context.
pub(super) fn run_prepared(
    entry: EntryFunction,
    functions: Arc<Vec<CompiledFunctionPointer>>,
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

#[cfg(test)]
mod tests {
    use super::{NameResolution, bind_call_frame, resolve_name};
    use crate::runtime::value::Value;

    #[test]
    fn bind_call_frame_maps_params_to_args() {
        let params = vec!["x".to_string(), "y".to_string()];
        let args = vec![Value::int_object(1), Value::int_object(2)];
        let frame = bind_call_frame(&params, &args);
        assert_eq!(frame.get("x").and_then(|value| value.as_int()), Some(1));
        assert_eq!(frame.get("y").and_then(|value| value.as_int()), Some(2));
    }

    #[test]
    fn resolve_name_prefers_local_over_global() {
        let resolved = resolve_name(Some(Value::int_object(1)), Some(Value::int_object(2)));
        assert!(matches!(
            resolved,
            NameResolution::Value(value) if value.as_int() == Some(1)
        ));
    }

    #[test]
    fn resolve_name_falls_back_to_global() {
        let global = resolve_name(None, Some(Value::int_object(2)));
        assert!(matches!(
            global,
            NameResolution::Value(value) if value.as_int() == Some(2)
        ));
    }

    #[test]
    fn resolve_name_reports_missing() {
        assert!(matches!(resolve_name(None, None), NameResolution::Missing));
    }
}
