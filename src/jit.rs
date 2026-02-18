use std::collections::{BTreeSet, HashMap, VecDeque};
use std::ptr;

use anyhow::{Result, bail};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{AbiParam, InstBuilder, MemFlags, Signature, types};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module};

use crate::ast::Program;
use crate::backend::{Backend, PreparedBackend};
use crate::builtins::BuiltinFunction;
use crate::bytecode::{Instruction, compile};
use crate::runtime::error::RuntimeError;
use crate::runtime::object::CallContext;
use crate::runtime::value::Value;

type RuntimeValue = Value;
type EntryFn = extern "C" fn(*mut Runtime, *const *mut RuntimeValue) -> *mut RuntimeValue;

#[derive(Clone, Copy)]
struct CompiledFunctionPointer {
    entry: EntryFn,
    arity: usize,
}

struct Runtime {
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

pub struct JIT;

pub struct PreparedProgram {
    _module: JITModule,
    entry: EntryFn,
    functions: HashMap<String, CompiledFunctionPointer>,
}

pub struct PreparedJIT {
    prepared: PreparedProgram,
}

impl JIT {
    pub fn new() -> Self {
        Self
    }

    pub fn prepare(&self, program: &Program) -> Result<PreparedProgram> {
        let compiled = compile(program)?;
        let mut function_names: Vec<&String> = compiled.functions.keys().collect();
        function_names.sort();

        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())?;
        builder.symbol("runtime_make_int", runtime_make_int as *const u8);
        builder.symbol("runtime_make_bool", runtime_make_bool as *const u8);
        builder.symbol("runtime_make_string", runtime_make_string as *const u8);
        builder.symbol("runtime_make_none", runtime_make_none as *const u8);
        builder.symbol("runtime_make_function", runtime_make_function as *const u8);
        builder.symbol("runtime_add", runtime_add as *const u8);
        builder.symbol("runtime_sub", runtime_sub as *const u8);
        builder.symbol("runtime_less_than", runtime_less_than as *const u8);
        builder.symbol("runtime_is_truthy", runtime_is_truthy as *const u8);
        builder.symbol("runtime_call", runtime_call as *const u8);
        builder.symbol("runtime_load_name", runtime_load_name as *const u8);
        builder.symbol("runtime_store_name", runtime_store_name as *const u8);

        let mut module = JITModule::new(builder);
        let ptr_type = module.target_config().pointer_type();
        let runtime_funcs = declare_runtime_functions(&mut module, ptr_type)?;
        let mut string_data = StringData::new();

        let mut function_ids = HashMap::new();
        let mut function_arities = HashMap::new();
        for name in &function_names {
            let arity = compiled
                .functions
                .get(*name)
                .ok_or_else(|| anyhow::anyhow!("Missing function '{name}'"))?
                .params
                .len();
            let func_sig = value_function_signature(&mut module, ptr_type);
            let func_id =
                module.declare_function(&function_symbol(name), Linkage::Local, &func_sig)?;
            function_ids.insert((*name).clone(), func_id);
            function_arities.insert((*name).clone(), arity);
        }
        let main_sig = value_function_signature(&mut module, ptr_type);
        let main_id = module.declare_function("run_main", Linkage::Local, &main_sig)?;

        for name in &function_names {
            let function = compiled
                .functions
                .get(*name)
                .ok_or_else(|| anyhow::anyhow!("Missing function '{name}'"))?;
            let mut locals = collect_store_names(&function.code);
            for param in &function.params {
                locals.insert(param.clone());
            }
            define_function(
                &mut module,
                &runtime_funcs,
                &mut string_data,
                function_ids.get(*name).copied().unwrap(),
                &function.params,
                &function.code,
                &locals,
            )?;
        }

        define_function(
            &mut module,
            &runtime_funcs,
            &mut string_data,
            main_id,
            &[],
            &compiled.main,
            &BTreeSet::new(),
        )?;

        module.finalize_definitions()?;
        let entry = module.get_finalized_function(main_id);
        let entry: EntryFn = unsafe { std::mem::transmute(entry) };

        let mut functions = HashMap::new();
        for (name, func_id) in &function_ids {
            let entry = module.get_finalized_function(*func_id);
            let entry: EntryFn = unsafe { std::mem::transmute(entry) };
            let arity = function_arities
                .get(name)
                .copied()
                .ok_or_else(|| anyhow::anyhow!("Missing arity for function '{name}'"))?;
            functions.insert(name.clone(), CompiledFunctionPointer { entry, arity });
        }

        Ok(PreparedProgram {
            _module: module,
            entry,
            functions,
        })
    }

    pub fn run_prepared(&self, prepared: &PreparedProgram) -> Result<String> {
        let mut runtime = Runtime::new(prepared.functions.clone());
        let result = (prepared.entry)(&mut runtime as *mut Runtime, ptr::null());
        if let Some(error) = runtime.error {
            bail!("{error}");
        }
        if result.is_null() {
            bail!("JIT execution failed");
        }
        Ok(runtime.output.join("\n"))
    }
}

impl Default for JIT {
    fn default() -> Self {
        Self::new()
    }
}

impl Backend for JIT {
    fn name(&self) -> &'static str {
        "jit"
    }

    fn prepare(&self, program: &Program) -> Result<Box<dyn PreparedBackend>> {
        Ok(Box::new(PreparedJIT {
            prepared: JIT::prepare(self, program)?,
        }))
    }
}

impl PreparedBackend for PreparedJIT {
    fn run(&self) -> Result<String> {
        JIT::new().run_prepared(&self.prepared)
    }
}

struct RuntimeFunctions {
    make_int: FuncId,
    make_bool: FuncId,
    make_string: FuncId,
    make_none: FuncId,
    make_function: FuncId,
    add: FuncId,
    sub: FuncId,
    less_than: FuncId,
    is_truthy: FuncId,
    call: FuncId,
    load_name: FuncId,
    store_name: FuncId,
}

struct StringData {
    counter: usize,
}

impl StringData {
    fn new() -> Self {
        Self { counter: 0 }
    }

    fn declare(
        &mut self,
        module: &mut JITModule,
        prefix: &str,
        value: &str,
    ) -> Result<(cranelift_module::DataId, i64)> {
        let name = format!("{prefix}_{}", self.counter);
        self.counter += 1;
        let data_id = module.declare_data(&name, Linkage::Local, false, false)?;
        let mut data_ctx = DataDescription::new();
        data_ctx.define(value.as_bytes().to_vec().into());
        module.define_data(data_id, &data_ctx)?;
        Ok((data_id, value.len() as i64))
    }
}

fn value_function_signature(
    module: &mut JITModule,
    ptr_type: cranelift_codegen::ir::Type,
) -> Signature {
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(ptr_type));
    sig.params.push(AbiParam::new(ptr_type));
    sig.returns.push(AbiParam::new(ptr_type));
    sig
}

fn declare_runtime_functions(
    module: &mut JITModule,
    ptr_type: cranelift_codegen::ir::Type,
) -> Result<RuntimeFunctions> {
    let mut make_int_sig = module.make_signature();
    make_int_sig.params.push(AbiParam::new(ptr_type));
    make_int_sig.params.push(AbiParam::new(types::I64));
    make_int_sig.returns.push(AbiParam::new(ptr_type));

    let mut make_bool_sig = module.make_signature();
    make_bool_sig.params.push(AbiParam::new(ptr_type));
    make_bool_sig.params.push(AbiParam::new(types::I8));
    make_bool_sig.returns.push(AbiParam::new(ptr_type));

    let mut make_string_sig = module.make_signature();
    make_string_sig.params.push(AbiParam::new(ptr_type));
    make_string_sig.params.push(AbiParam::new(ptr_type));
    make_string_sig.params.push(AbiParam::new(types::I64));
    make_string_sig.returns.push(AbiParam::new(ptr_type));

    let mut make_none_sig = module.make_signature();
    make_none_sig.params.push(AbiParam::new(ptr_type));
    make_none_sig.returns.push(AbiParam::new(ptr_type));

    let mut make_function_sig = module.make_signature();
    make_function_sig.params.push(AbiParam::new(ptr_type));
    make_function_sig.params.push(AbiParam::new(ptr_type));
    make_function_sig.params.push(AbiParam::new(types::I64));
    make_function_sig.returns.push(AbiParam::new(ptr_type));

    let mut binary_sig = module.make_signature();
    binary_sig.params.push(AbiParam::new(ptr_type));
    binary_sig.params.push(AbiParam::new(ptr_type));
    binary_sig.params.push(AbiParam::new(ptr_type));
    binary_sig.returns.push(AbiParam::new(ptr_type));

    let mut truthy_sig = module.make_signature();
    truthy_sig.params.push(AbiParam::new(ptr_type));
    truthy_sig.returns.push(AbiParam::new(types::I8));

    let mut call_sig = module.make_signature();
    call_sig.params.push(AbiParam::new(ptr_type));
    call_sig.params.push(AbiParam::new(ptr_type));
    call_sig.params.push(AbiParam::new(ptr_type));
    call_sig.params.push(AbiParam::new(types::I64));
    call_sig.returns.push(AbiParam::new(ptr_type));

    let mut load_name_sig = module.make_signature();
    load_name_sig.params.push(AbiParam::new(ptr_type));
    load_name_sig.params.push(AbiParam::new(ptr_type));
    load_name_sig.params.push(AbiParam::new(types::I64));
    load_name_sig.returns.push(AbiParam::new(ptr_type));

    let mut store_name_sig = module.make_signature();
    store_name_sig.params.push(AbiParam::new(ptr_type));
    store_name_sig.params.push(AbiParam::new(ptr_type));
    store_name_sig.params.push(AbiParam::new(types::I64));
    store_name_sig.params.push(AbiParam::new(ptr_type));

    Ok(RuntimeFunctions {
        make_int: module.declare_function("runtime_make_int", Linkage::Import, &make_int_sig)?,
        make_bool: module.declare_function("runtime_make_bool", Linkage::Import, &make_bool_sig)?,
        make_string: module.declare_function(
            "runtime_make_string",
            Linkage::Import,
            &make_string_sig,
        )?,
        make_none: module.declare_function("runtime_make_none", Linkage::Import, &make_none_sig)?,
        make_function: module.declare_function(
            "runtime_make_function",
            Linkage::Import,
            &make_function_sig,
        )?,
        add: module.declare_function("runtime_add", Linkage::Import, &binary_sig)?,
        sub: module.declare_function("runtime_sub", Linkage::Import, &binary_sig)?,
        less_than: module.declare_function("runtime_less_than", Linkage::Import, &binary_sig)?,
        is_truthy: module.declare_function("runtime_is_truthy", Linkage::Import, &truthy_sig)?,
        call: module.declare_function("runtime_call", Linkage::Import, &call_sig)?,
        load_name: module.declare_function("runtime_load_name", Linkage::Import, &load_name_sig)?,
        store_name: module.declare_function(
            "runtime_store_name",
            Linkage::Import,
            &store_name_sig,
        )?,
    })
}

#[allow(clippy::too_many_arguments)]
fn define_function(
    module: &mut JITModule,
    runtime_funcs: &RuntimeFunctions,
    string_data: &mut StringData,
    func_id: FuncId,
    param_names: &[String],
    code: &[Instruction],
    locals: &BTreeSet<String>,
) -> Result<()> {
    let ptr_type = module.target_config().pointer_type();
    let stack_size = max_stack_depth(code)?.max(1);
    let ptr_size = ptr_type.bytes() as i64;
    let local_list: Vec<&String> = locals.iter().collect();
    let local_indices: HashMap<String, i64> = local_list
        .iter()
        .enumerate()
        .map(|(idx, name)| ((*name).clone(), idx as i64))
        .collect();

    let mut ctx = module.make_context();
    ctx.func.signature = value_function_signature(module, ptr_type);
    let make_int = module.declare_func_in_func(runtime_funcs.make_int, &mut ctx.func);
    let make_bool = module.declare_func_in_func(runtime_funcs.make_bool, &mut ctx.func);
    let make_string = module.declare_func_in_func(runtime_funcs.make_string, &mut ctx.func);
    let make_none = module.declare_func_in_func(runtime_funcs.make_none, &mut ctx.func);
    let make_function = module.declare_func_in_func(runtime_funcs.make_function, &mut ctx.func);
    let add = module.declare_func_in_func(runtime_funcs.add, &mut ctx.func);
    let sub = module.declare_func_in_func(runtime_funcs.sub, &mut ctx.func);
    let less_than = module.declare_func_in_func(runtime_funcs.less_than, &mut ctx.func);
    let is_truthy = module.declare_func_in_func(runtime_funcs.is_truthy, &mut ctx.func);
    let call_value = module.declare_func_in_func(runtime_funcs.call, &mut ctx.func);
    let load_name = module.declare_func_in_func(runtime_funcs.load_name, &mut ctx.func);
    let store_name = module.declare_func_in_func(runtime_funcs.store_name, &mut ctx.func);

    let mut builder_ctx = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);

    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);
    let ctx_param = builder.block_params(entry_block)[0];
    let args_param = builder.block_params(entry_block)[1];

    let ptr_align_shift = (ptr_size as u32).trailing_zeros() as u8;
    let stack_slot = builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
        cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
        (stack_size as u32) * (ptr_size as u32),
        ptr_align_shift,
    ));
    let call_args_slot =
        builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
            (stack_size as u32) * (ptr_size as u32),
            ptr_align_shift,
        ));
    let sp_slot = builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
        cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
        ptr_size as u32,
        ptr_align_shift,
    ));
    let tmp_slot = builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
        cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
        ptr_size as u32,
        ptr_align_shift,
    ));
    let locals_slot = if local_list.is_empty() {
        None
    } else {
        Some(
            builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
                cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                (local_list.len() as u32) * (ptr_size as u32),
                ptr_align_shift,
            )),
        )
    };
    let locals_set_slot = if local_list.is_empty() {
        None
    } else {
        Some(
            builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
                cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                local_list.len() as u32,
                0,
            )),
        )
    };

    let stack_base = builder.ins().stack_addr(ptr_type, stack_slot, 0);
    let call_args_base = builder.ins().stack_addr(ptr_type, call_args_slot, 0);
    let sp_addr = builder.ins().stack_addr(ptr_type, sp_slot, 0);
    let tmp_addr = builder.ins().stack_addr(ptr_type, tmp_slot, 0);
    let zero = builder.ins().iconst(ptr_type, 0);
    builder.ins().store(MemFlags::new(), zero, sp_addr, 0);

    if let Some(set_slot) = locals_set_slot {
        let locals_set_base = builder.ins().stack_addr(ptr_type, set_slot, 0);
        let zero_flag = builder.ins().iconst(types::I8, 0);
        for idx in 0..local_list.len() {
            let offset = builder.ins().iadd_imm(locals_set_base, idx as i64);
            builder.ins().store(MemFlags::new(), zero_flag, offset, 0);
        }
    }
    if !param_names.is_empty() {
        if locals_set_slot.is_none() || locals_slot.is_none() {
            bail!("Missing locals storage for function parameters");
        }
        let locals_base = builder.ins().stack_addr(ptr_type, locals_slot.unwrap(), 0);
        let locals_set_base = builder
            .ins()
            .stack_addr(ptr_type, locals_set_slot.unwrap(), 0);
        for (index, param_name) in param_names.iter().enumerate() {
            let local_index = local_indices.get(param_name).copied().ok_or_else(|| {
                anyhow::anyhow!("Missing local slot for parameter '{param_name}'")
            })?;
            let param_addr = builder
                .ins()
                .iadd_imm(args_param, (index as i64) * ptr_size);
            let value = builder.ins().load(ptr_type, MemFlags::new(), param_addr, 0);
            let local_addr = builder.ins().iadd_imm(locals_base, local_index * ptr_size);
            builder.ins().store(MemFlags::new(), value, local_addr, 0);
            let set_addr = builder.ins().iadd_imm(locals_set_base, local_index);
            let one = builder.ins().iconst(types::I8, 1);
            builder.ins().store(MemFlags::new(), one, set_addr, 0);
        }
    }

    let mut blocks = Vec::with_capacity(code.len() + 1);
    for _ in 0..=code.len() {
        blocks.push(builder.create_block());
    }
    let error_block = builder.create_block();

    builder.ins().jump(blocks[0], &[]);

    for (idx, instruction) in code.iter().enumerate() {
        let block = blocks[idx];
        builder.switch_to_block(block);
        let mut terminated = false;

        let load_sp = |builder: &mut FunctionBuilder| {
            builder.ins().load(ptr_type, MemFlags::new(), sp_addr, 0)
        };
        let stack_addr = |builder: &mut FunctionBuilder, index| {
            let offset = builder.ins().imul_imm(index, ptr_size);
            builder.ins().iadd(stack_base, offset)
        };
        let push_value = |builder: &mut FunctionBuilder, value| {
            let sp_value = load_sp(builder);
            let addr = stack_addr(builder, sp_value);
            builder.ins().store(MemFlags::new(), value, addr, 0);
            let new_sp = builder.ins().iadd_imm(sp_value, 1);
            builder.ins().store(MemFlags::new(), new_sp, sp_addr, 0);
        };
        let pop_value = |builder: &mut FunctionBuilder| {
            let sp_value = load_sp(builder);
            let new_sp = builder.ins().iadd_imm(sp_value, -1);
            builder.ins().store(MemFlags::new(), new_sp, sp_addr, 0);
            let addr = stack_addr(builder, new_sp);
            builder.ins().load(ptr_type, MemFlags::new(), addr, 0)
        };
        let check_null = |builder: &mut FunctionBuilder, value| {
            let ok_block = builder.create_block();
            let is_null = builder.ins().icmp_imm(IntCC::Equal, value, 0);
            builder.ins().brif(is_null, error_block, &[], ok_block, &[]);
            ok_block
        };

        match instruction {
            Instruction::PushInt(value) => {
                let imm = builder.ins().iconst(types::I64, *value);
                let call = builder.ins().call(make_int, &[ctx_param, imm]);
                let result = builder.inst_results(call)[0];
                push_value(&mut builder, result);
            }
            Instruction::PushBool(value) => {
                let imm = builder.ins().iconst(types::I8, if *value { 1 } else { 0 });
                let call = builder.ins().call(make_bool, &[ctx_param, imm]);
                let result = builder.inst_results(call)[0];
                push_value(&mut builder, result);
            }
            Instruction::PushString(value) => {
                let (data_id, len) = string_data.declare(module, "str", value)?;
                let gv = module.declare_data_in_func(data_id, builder.func);
                let data_ptr = builder.ins().global_value(ptr_type, gv);
                let len_val = builder.ins().iconst(types::I64, len);
                let call = builder
                    .ins()
                    .call(make_string, &[ctx_param, data_ptr, len_val]);
                let result = builder.inst_results(call)[0];
                push_value(&mut builder, result);
            }
            Instruction::BuildList(_) => {
                bail!("List literals are not supported in the JIT backend");
            }
            Instruction::PushNone => {
                let call = builder.ins().call(make_none, &[ctx_param]);
                let result = builder.inst_results(call)[0];
                push_value(&mut builder, result);
            }
            Instruction::DefineClass { .. } => {
                bail!("Class definitions are not supported in the JIT backend");
            }
            Instruction::DefineFunction { name, symbol } => {
                let (symbol_data_id, symbol_len) =
                    string_data.declare(module, "fn_symbol", symbol)?;
                let symbol_gv = module.declare_data_in_func(symbol_data_id, builder.func);
                let symbol_ptr = builder.ins().global_value(ptr_type, symbol_gv);
                let symbol_len_val = builder.ins().iconst(types::I64, symbol_len);
                let call = builder
                    .ins()
                    .call(make_function, &[ctx_param, symbol_ptr, symbol_len_val]);
                let function_value = builder.inst_results(call)[0];
                let ok_block = check_null(&mut builder, function_value);
                builder.switch_to_block(ok_block);

                if let Some(local_idx) = local_indices.get(name).copied() {
                    if locals_set_slot.is_none() || locals_slot.is_none() {
                        bail!("Missing locals storage for '{name}'");
                    }
                    let locals_base = builder.ins().stack_addr(ptr_type, locals_slot.unwrap(), 0);
                    let locals_set_base =
                        builder
                            .ins()
                            .stack_addr(ptr_type, locals_set_slot.unwrap(), 0);
                    let local_addr = builder.ins().iadd_imm(locals_base, local_idx * ptr_size);
                    builder
                        .ins()
                        .store(MemFlags::new(), function_value, local_addr, 0);
                    let set_addr = builder.ins().iadd_imm(locals_set_base, local_idx);
                    let one = builder.ins().iconst(types::I8, 1);
                    builder.ins().store(MemFlags::new(), one, set_addr, 0);
                } else {
                    let (name_data_id, name_len) = string_data.declare(module, "name", name)?;
                    let name_gv = module.declare_data_in_func(name_data_id, builder.func);
                    let name_ptr = builder.ins().global_value(ptr_type, name_gv);
                    let name_len_val = builder.ins().iconst(types::I64, name_len);
                    builder.ins().call(
                        store_name,
                        &[ctx_param, name_ptr, name_len_val, function_value],
                    );
                }
            }
            Instruction::LoadName(name) => {
                let (name_data_id, name_len) = string_data.declare(module, "name", name)?;
                let name_gv = module.declare_data_in_func(name_data_id, builder.func);
                let name_ptr = builder.ins().global_value(ptr_type, name_gv);
                let name_len_val = builder.ins().iconst(types::I64, name_len);

                if let Some(local_idx) = local_indices.get(name).copied() {
                    if locals_set_slot.is_none() || locals_slot.is_none() {
                        bail!("Missing locals storage for '{name}'");
                    }
                    let locals_base = builder.ins().stack_addr(ptr_type, locals_slot.unwrap(), 0);
                    let locals_set_base =
                        builder
                            .ins()
                            .stack_addr(ptr_type, locals_set_slot.unwrap(), 0);
                    let set_addr = builder.ins().iadd_imm(locals_set_base, local_idx);
                    let is_set = builder.ins().load(types::I8, MemFlags::new(), set_addr, 0);
                    let local_block = builder.create_block();
                    let global_block = builder.create_block();
                    let cont_block = builder.create_block();
                    builder
                        .ins()
                        .brif(is_set, local_block, &[], global_block, &[]);

                    builder.switch_to_block(local_block);
                    let local_addr = builder.ins().iadd_imm(locals_base, local_idx * ptr_size);
                    let local_val = builder.ins().load(ptr_type, MemFlags::new(), local_addr, 0);
                    builder.ins().store(MemFlags::new(), local_val, tmp_addr, 0);
                    builder.ins().jump(cont_block, &[]);

                    builder.switch_to_block(global_block);
                    let call = builder
                        .ins()
                        .call(load_name, &[ctx_param, name_ptr, name_len_val]);
                    let result = builder.inst_results(call)[0];
                    let ok_block = check_null(&mut builder, result);
                    builder.switch_to_block(ok_block);
                    builder.ins().store(MemFlags::new(), result, tmp_addr, 0);
                    builder.ins().jump(cont_block, &[]);

                    builder.switch_to_block(cont_block);
                    let value = builder.ins().load(ptr_type, MemFlags::new(), tmp_addr, 0);
                    push_value(&mut builder, value);
                } else {
                    let call = builder
                        .ins()
                        .call(load_name, &[ctx_param, name_ptr, name_len_val]);
                    let result = builder.inst_results(call)[0];
                    let ok_block = check_null(&mut builder, result);
                    builder.switch_to_block(ok_block);
                    push_value(&mut builder, result);
                }
            }
            Instruction::StoreName(name) => {
                let value = pop_value(&mut builder);
                if let Some(local_idx) = local_indices.get(name).copied() {
                    if locals_set_slot.is_none() || locals_slot.is_none() {
                        bail!("Missing locals storage for '{name}'");
                    }
                    let locals_base = builder.ins().stack_addr(ptr_type, locals_slot.unwrap(), 0);
                    let locals_set_base =
                        builder
                            .ins()
                            .stack_addr(ptr_type, locals_set_slot.unwrap(), 0);
                    let local_addr = builder.ins().iadd_imm(locals_base, local_idx * ptr_size);
                    builder.ins().store(MemFlags::new(), value, local_addr, 0);
                    let set_addr = builder.ins().iadd_imm(locals_set_base, local_idx);
                    let one = builder.ins().iconst(types::I8, 1);
                    builder.ins().store(MemFlags::new(), one, set_addr, 0);
                } else {
                    let (name_data_id, name_len) = string_data.declare(module, "name", name)?;
                    let name_gv = module.declare_data_in_func(name_data_id, builder.func);
                    let name_ptr = builder.ins().global_value(ptr_type, name_gv);
                    let name_len_val = builder.ins().iconst(types::I64, name_len);
                    builder
                        .ins()
                        .call(store_name, &[ctx_param, name_ptr, name_len_val, value]);
                }
            }
            Instruction::Add => {
                let right = pop_value(&mut builder);
                let left = pop_value(&mut builder);
                let call = builder.ins().call(add, &[ctx_param, left, right]);
                let result = builder.inst_results(call)[0];
                let ok_block = check_null(&mut builder, result);
                builder.switch_to_block(ok_block);
                push_value(&mut builder, result);
            }
            Instruction::Sub => {
                let right = pop_value(&mut builder);
                let left = pop_value(&mut builder);
                let call = builder.ins().call(sub, &[ctx_param, left, right]);
                let result = builder.inst_results(call)[0];
                let ok_block = check_null(&mut builder, result);
                builder.switch_to_block(ok_block);
                push_value(&mut builder, result);
            }
            Instruction::LessThan => {
                let right = pop_value(&mut builder);
                let left = pop_value(&mut builder);
                let call = builder.ins().call(less_than, &[ctx_param, left, right]);
                let result = builder.inst_results(call)[0];
                let ok_block = check_null(&mut builder, result);
                builder.switch_to_block(ok_block);
                push_value(&mut builder, result);
            }
            Instruction::LoadIndex => {
                bail!("List indexing is not supported in the JIT backend");
            }
            Instruction::StoreIndex(_) => {
                bail!("List index assignment is not supported in the JIT backend");
            }
            Instruction::LoadAttr(_) => {
                bail!("Attribute access is not supported in the JIT backend");
            }
            Instruction::StoreAttr(_) => {
                bail!("Attribute assignment is not supported in the JIT backend");
            }
            Instruction::Call { argc } => {
                for arg_index in (0..*argc).rev() {
                    let arg_value = pop_value(&mut builder);
                    let arg_addr = builder
                        .ins()
                        .iadd_imm(call_args_base, (arg_index as i64) * ptr_size);
                    builder.ins().store(MemFlags::new(), arg_value, arg_addr, 0);
                }
                let callee = pop_value(&mut builder);
                let argc_val = builder.ins().iconst(types::I64, *argc as i64);
                let call = builder
                    .ins()
                    .call(call_value, &[ctx_param, callee, call_args_base, argc_val]);
                let result = builder.inst_results(call)[0];
                let ok_block = check_null(&mut builder, result);
                builder.switch_to_block(ok_block);
                push_value(&mut builder, result);
            }
            Instruction::JumpIfFalse(target) => {
                let target_index = resolve_relative_target(idx, *target, code.len())?;
                let value = pop_value(&mut builder);
                let call = builder.ins().call(is_truthy, &[value]);
                let truthy = builder.inst_results(call)[0];
                builder
                    .ins()
                    .brif(truthy, blocks[idx + 1], &[], blocks[target_index], &[]);
                terminated = true;
            }
            Instruction::Jump(target) => {
                let target_index = resolve_relative_target(idx, *target, code.len())?;
                builder.ins().jump(blocks[target_index], &[]);
                terminated = true;
            }
            Instruction::Pop => {
                pop_value(&mut builder);
            }
            Instruction::Return => {
                let call = builder.ins().call(make_none, &[ctx_param]);
                let result = builder.inst_results(call)[0];
                builder.ins().return_(&[result]);
                terminated = true;
            }
            Instruction::ReturnValue => {
                let value = pop_value(&mut builder);
                builder.ins().return_(&[value]);
                terminated = true;
            }
        }

        if !terminated {
            builder.ins().jump(blocks[idx + 1], &[]);
        }
    }

    let exit_block = blocks[code.len()];
    builder.switch_to_block(exit_block);
    let call = builder.ins().call(make_none, &[ctx_param]);
    let result = builder.inst_results(call)[0];
    builder.ins().return_(&[result]);

    builder.switch_to_block(error_block);
    let null_ptr = builder.ins().iconst(ptr_type, 0);
    builder.ins().return_(&[null_ptr]);

    builder.seal_all_blocks();
    builder.finalize();
    module.define_function(func_id, &mut ctx)?;
    module.clear_context(&mut ctx);

    Ok(())
}

fn function_symbol(name: &str) -> String {
    format!("fn_{name}")
}

fn collect_store_names(code: &[Instruction]) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    for instruction in code {
        if let Instruction::StoreName(name) = instruction {
            names.insert(name.clone());
        }
    }
    names
}

fn max_stack_depth(code: &[Instruction]) -> Result<usize> {
    if code.is_empty() {
        return Ok(0);
    }

    let mut depths = vec![None; code.len()];
    let mut worklist = VecDeque::new();
    depths[0] = Some(0);
    worklist.push_back(0);
    let mut max_depth = 0;

    while let Some(ip) = worklist.pop_front() {
        let depth = depths[ip].unwrap_or(0);
        let instruction = &code[ip];
        let next_depth = depth + stack_effect(instruction);
        if next_depth < 0 {
            bail!("Bytecode stack underflow at {ip}");
        }
        max_depth = max_depth.max(next_depth as usize);

        match instruction {
            Instruction::Return | Instruction::ReturnValue => {}
            Instruction::Jump(target) => {
                let target_index = resolve_relative_target(ip, *target, code.len())?;
                propagate_depth(
                    &mut depths,
                    &mut worklist,
                    target_index,
                    next_depth,
                    code.len(),
                )?;
            }
            Instruction::JumpIfFalse(target) => {
                let target_index = resolve_relative_target(ip, *target, code.len())?;
                propagate_depth(
                    &mut depths,
                    &mut worklist,
                    target_index,
                    next_depth,
                    code.len(),
                )?;
                propagate_depth(&mut depths, &mut worklist, ip + 1, next_depth, code.len())?;
            }
            _ => {
                propagate_depth(&mut depths, &mut worklist, ip + 1, next_depth, code.len())?;
            }
        }
    }

    Ok(max_depth)
}

fn resolve_relative_target(ip: usize, offset: isize, code_len: usize) -> Result<usize> {
    let target = (ip as isize) + 1 + offset;
    if target < 0 || (target as usize) >= code_len {
        bail!("Invalid jump target at {ip} with offset {offset}");
    }
    Ok(target as usize)
}

fn propagate_depth(
    depths: &mut [Option<i32>],
    worklist: &mut VecDeque<usize>,
    target: usize,
    depth: i32,
    code_len: usize,
) -> Result<()> {
    if target >= code_len {
        return Ok(());
    }
    if let Some(existing) = depths[target] {
        if existing != depth {
            bail!("Bytecode stack mismatch at {target}");
        }
        return Ok(());
    }
    depths[target] = Some(depth);
    worklist.push_back(target);
    Ok(())
}

fn stack_effect(instruction: &Instruction) -> i32 {
    match instruction {
        Instruction::PushInt(_)
        | Instruction::PushBool(_)
        | Instruction::PushString(_)
        | Instruction::PushNone
        | Instruction::LoadName(_) => 1,
        Instruction::DefineClass { .. } | Instruction::DefineFunction { .. } => 0,
        Instruction::LoadAttr(_) => 0,
        Instruction::BuildList(count) => 1 - (*count as i32),
        Instruction::Call { argc } => -(*argc as i32),
        Instruction::StoreName(_) | Instruction::Pop | Instruction::JumpIfFalse(_) => -1,
        Instruction::LoadIndex => -1,
        Instruction::StoreIndex(_) => -2,
        Instruction::StoreAttr(_) => -2,
        Instruction::Add | Instruction::Sub | Instruction::LessThan => -1,
        Instruction::Jump(_) | Instruction::Return => 0,
        Instruction::ReturnValue => -1,
    }
}
