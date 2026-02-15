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
use crate::bytecode::{Instruction, compile};

type EntryFn = extern "C" fn(*mut Runtime) -> *mut RuntimeValue;

#[derive(Debug)]
enum RuntimeValue {
    Integer(i64),
    Boolean(bool),
    String(String),
    None,
}

impl RuntimeValue {
    fn to_output(&self) -> String {
        match self {
            RuntimeValue::Integer(value) => value.to_string(),
            RuntimeValue::Boolean(value) => {
                if *value {
                    "True".to_string()
                } else {
                    "False".to_string()
                }
            }
            RuntimeValue::String(value) => value.clone(),
            RuntimeValue::None => "None".to_string(),
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            RuntimeValue::Integer(value) => *value != 0,
            RuntimeValue::Boolean(value) => *value,
            RuntimeValue::String(value) => !value.is_empty(),
            RuntimeValue::None => false,
        }
    }
}

struct Runtime {
    globals: Vec<*mut RuntimeValue>,
    globals_set: Vec<bool>,
    global_names: Vec<String>,
    output: Vec<String>,
    #[allow(clippy::vec_box)]
    values: Vec<Box<RuntimeValue>>,
    none_ptr: *mut RuntimeValue,
    error: Option<String>,
}

impl Runtime {
    fn new(global_names: Vec<String>) -> Self {
        let mut runtime = Self {
            globals: Vec::new(),
            globals_set: Vec::new(),
            global_names,
            output: Vec::new(),
            values: Vec::new(),
            none_ptr: ptr::null_mut(),
            error: None,
        };
        let none_ptr = runtime.alloc_value(RuntimeValue::None);
        runtime.none_ptr = none_ptr;
        runtime.globals = vec![none_ptr; runtime.global_names.len()];
        runtime.globals_set = vec![false; runtime.global_names.len()];
        runtime
    }

    fn alloc_value(&mut self, value: RuntimeValue) -> *mut RuntimeValue {
        let mut boxed = Box::new(value);
        let ptr = &mut *boxed as *mut RuntimeValue;
        self.values.push(boxed);
        ptr
    }

    fn set_error(&mut self, message: String) {
        if self.error.is_none() {
            self.error = Some(message);
        }
    }
}

unsafe extern "C" fn runtime_make_int(ctx: *mut Runtime, value: i64) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    runtime.alloc_value(RuntimeValue::Integer(value))
}

unsafe extern "C" fn runtime_make_bool(ctx: *mut Runtime, value: u8) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    runtime.alloc_value(RuntimeValue::Boolean(value != 0))
}

unsafe extern "C" fn runtime_make_string(
    ctx: *mut Runtime,
    ptr: *const u8,
    len: i64,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    let slice = unsafe { std::slice::from_raw_parts(ptr, len as usize) };
    let value = String::from_utf8_lossy(slice).to_string();
    runtime.alloc_value(RuntimeValue::String(value))
}

unsafe extern "C" fn runtime_make_none(ctx: *mut Runtime) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    runtime.none_ptr
}

unsafe extern "C" fn runtime_add(
    ctx: *mut Runtime,
    left: *mut RuntimeValue,
    right: *mut RuntimeValue,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    let left_value = match unsafe { &*left } {
        RuntimeValue::Integer(value) => *value,
        other => {
            runtime.set_error(format!("Expected integer, got {other:?}"));
            return ptr::null_mut();
        }
    };
    let right_value = match unsafe { &*right } {
        RuntimeValue::Integer(value) => *value,
        other => {
            runtime.set_error(format!("Expected integer, got {other:?}"));
            return ptr::null_mut();
        }
    };
    runtime.alloc_value(RuntimeValue::Integer(left_value + right_value))
}

unsafe extern "C" fn runtime_sub(
    ctx: *mut Runtime,
    left: *mut RuntimeValue,
    right: *mut RuntimeValue,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    let left_value = match unsafe { &*left } {
        RuntimeValue::Integer(value) => *value,
        other => {
            runtime.set_error(format!("Expected integer, got {other:?}"));
            return ptr::null_mut();
        }
    };
    let right_value = match unsafe { &*right } {
        RuntimeValue::Integer(value) => *value,
        other => {
            runtime.set_error(format!("Expected integer, got {other:?}"));
            return ptr::null_mut();
        }
    };
    runtime.alloc_value(RuntimeValue::Integer(left_value - right_value))
}

unsafe extern "C" fn runtime_less_than(
    ctx: *mut Runtime,
    left: *mut RuntimeValue,
    right: *mut RuntimeValue,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    let left_value = match unsafe { &*left } {
        RuntimeValue::Integer(value) => *value,
        other => {
            runtime.set_error(format!("Expected integer, got {other:?}"));
            return ptr::null_mut();
        }
    };
    let right_value = match unsafe { &*right } {
        RuntimeValue::Integer(value) => *value,
        other => {
            runtime.set_error(format!("Expected integer, got {other:?}"));
            return ptr::null_mut();
        }
    };
    runtime.alloc_value(RuntimeValue::Boolean(left_value < right_value))
}

unsafe extern "C" fn runtime_is_truthy(value: *mut RuntimeValue) -> u8 {
    let value = unsafe { &*value };
    if value.is_truthy() { 1 } else { 0 }
}

unsafe extern "C" fn runtime_print(
    ctx: *mut Runtime,
    values: *const *mut RuntimeValue,
    count: i64,
) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    let mut rendered = Vec::new();
    for idx in 0..count {
        let value_ptr = unsafe { *values.offset(idx as isize) };
        rendered.push((unsafe { &*value_ptr }).to_output());
    }
    runtime.output.push(rendered.join(" "));
    runtime.none_ptr
}

unsafe extern "C" fn runtime_load_global(ctx: *mut Runtime, index: i64) -> *mut RuntimeValue {
    let runtime = unsafe { &mut *ctx };
    let idx = index as usize;
    if idx >= runtime.globals.len() {
        runtime.set_error("Undefined variable".to_string());
        return ptr::null_mut();
    }
    if runtime.globals_set[idx] {
        runtime.globals[idx]
    } else {
        let name = runtime.global_names[idx].clone();
        runtime.set_error(format!("Undefined variable '{name}'"));
        ptr::null_mut()
    }
}

unsafe extern "C" fn runtime_store_global(ctx: *mut Runtime, index: i64, value: *mut RuntimeValue) {
    let runtime = unsafe { &mut *ctx };
    let idx = index as usize;
    if idx >= runtime.globals.len() {
        runtime.set_error("Undefined variable".to_string());
        return;
    }
    runtime.globals[idx] = value;
    runtime.globals_set[idx] = true;
}

unsafe extern "C" fn runtime_set_error(ctx: *mut Runtime, ptr: *const u8, len: i64) {
    let runtime = unsafe { &mut *ctx };
    if runtime.error.is_some() {
        return;
    }
    let slice = unsafe { std::slice::from_raw_parts(ptr, len as usize) };
    let message = String::from_utf8_lossy(slice).to_string();
    runtime.error = Some(message);
}

pub struct JIT;

pub struct PreparedProgram {
    _module: JITModule,
    entry: EntryFn,
    global_names: Vec<String>,
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
        let globals = collect_store_names(&compiled.main);
        let mut global_names: Vec<String> = globals.iter().cloned().collect();
        global_names.sort();
        let global_indices: HashMap<String, i64> = global_names
            .iter()
            .enumerate()
            .map(|(idx, name)| (name.clone(), idx as i64))
            .collect();
        let mut function_names: Vec<&String> = compiled.functions.keys().collect();
        function_names.sort();

        let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())?;
        builder.symbol("runtime_make_int", runtime_make_int as *const u8);
        builder.symbol("runtime_make_bool", runtime_make_bool as *const u8);
        builder.symbol("runtime_make_string", runtime_make_string as *const u8);
        builder.symbol("runtime_make_none", runtime_make_none as *const u8);
        builder.symbol("runtime_add", runtime_add as *const u8);
        builder.symbol("runtime_sub", runtime_sub as *const u8);
        builder.symbol("runtime_less_than", runtime_less_than as *const u8);
        builder.symbol("runtime_is_truthy", runtime_is_truthy as *const u8);
        builder.symbol("runtime_print", runtime_print as *const u8);
        builder.symbol("runtime_load_global", runtime_load_global as *const u8);
        builder.symbol("runtime_store_global", runtime_store_global as *const u8);
        builder.symbol("runtime_set_error", runtime_set_error as *const u8);
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
            let func_sig = value_function_signature(&mut module, ptr_type, arity);
            let func_id =
                module.declare_function(&function_symbol(name), Linkage::Local, &func_sig)?;
            function_ids.insert((*name).clone(), func_id);
            function_arities.insert((*name).clone(), arity);
        }
        let main_sig = value_function_signature(&mut module, ptr_type, 0);
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
                &function_ids,
                &function_arities,
                &global_indices,
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
            &function_ids,
            &function_arities,
            &global_indices,
            main_id,
            &[],
            &compiled.main,
            &BTreeSet::new(),
        )?;

        module.finalize_definitions()?;
        let entry = module.get_finalized_function(main_id);
        let entry: EntryFn = unsafe { std::mem::transmute(entry) };

        Ok(PreparedProgram {
            _module: module,
            entry,
            global_names,
        })
    }

    pub fn run_prepared(&self, prepared: &PreparedProgram) -> Result<String> {
        let mut runtime = Runtime::new(prepared.global_names.clone());
        let result = (prepared.entry)(&mut runtime as *mut Runtime);
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
    add: FuncId,
    sub: FuncId,
    less_than: FuncId,
    is_truthy: FuncId,
    print: FuncId,
    load_global: FuncId,
    store_global: FuncId,
    set_error: FuncId,
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
    arity: usize,
) -> Signature {
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(ptr_type));
    for _ in 0..arity {
        sig.params.push(AbiParam::new(ptr_type));
    }
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

    let mut binary_sig = module.make_signature();
    binary_sig.params.push(AbiParam::new(ptr_type));
    binary_sig.params.push(AbiParam::new(ptr_type));
    binary_sig.params.push(AbiParam::new(ptr_type));
    binary_sig.returns.push(AbiParam::new(ptr_type));

    let mut truthy_sig = module.make_signature();
    truthy_sig.params.push(AbiParam::new(ptr_type));
    truthy_sig.returns.push(AbiParam::new(types::I8));

    let mut print_sig = module.make_signature();
    print_sig.params.push(AbiParam::new(ptr_type));
    print_sig.params.push(AbiParam::new(ptr_type));
    print_sig.params.push(AbiParam::new(types::I64));
    print_sig.returns.push(AbiParam::new(ptr_type));

    let mut load_global_sig = module.make_signature();
    load_global_sig.params.push(AbiParam::new(ptr_type));
    load_global_sig.params.push(AbiParam::new(types::I64));
    load_global_sig.returns.push(AbiParam::new(ptr_type));

    let mut store_global_sig = module.make_signature();
    store_global_sig.params.push(AbiParam::new(ptr_type));
    store_global_sig.params.push(AbiParam::new(types::I64));
    store_global_sig.params.push(AbiParam::new(ptr_type));

    let mut set_error_sig = module.make_signature();
    set_error_sig.params.push(AbiParam::new(ptr_type));
    set_error_sig.params.push(AbiParam::new(ptr_type));
    set_error_sig.params.push(AbiParam::new(types::I64));

    Ok(RuntimeFunctions {
        make_int: module.declare_function("runtime_make_int", Linkage::Import, &make_int_sig)?,
        make_bool: module.declare_function("runtime_make_bool", Linkage::Import, &make_bool_sig)?,
        make_string: module.declare_function(
            "runtime_make_string",
            Linkage::Import,
            &make_string_sig,
        )?,
        make_none: module.declare_function("runtime_make_none", Linkage::Import, &make_none_sig)?,
        add: module.declare_function("runtime_add", Linkage::Import, &binary_sig)?,
        sub: module.declare_function("runtime_sub", Linkage::Import, &binary_sig)?,
        less_than: module.declare_function("runtime_less_than", Linkage::Import, &binary_sig)?,
        is_truthy: module.declare_function("runtime_is_truthy", Linkage::Import, &truthy_sig)?,
        print: module.declare_function("runtime_print", Linkage::Import, &print_sig)?,
        load_global: module.declare_function(
            "runtime_load_global",
            Linkage::Import,
            &load_global_sig,
        )?,
        store_global: module.declare_function(
            "runtime_store_global",
            Linkage::Import,
            &store_global_sig,
        )?,
        set_error: module.declare_function("runtime_set_error", Linkage::Import, &set_error_sig)?,
    })
}

#[allow(clippy::too_many_arguments)]
fn define_function(
    module: &mut JITModule,
    runtime_funcs: &RuntimeFunctions,
    string_data: &mut StringData,
    _function_ids: &HashMap<String, FuncId>,
    _function_arities: &HashMap<String, usize>,
    global_indices: &HashMap<String, i64>,
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
    ctx.func.signature = value_function_signature(module, ptr_type, param_names.len());
    let make_int = module.declare_func_in_func(runtime_funcs.make_int, &mut ctx.func);
    let make_bool = module.declare_func_in_func(runtime_funcs.make_bool, &mut ctx.func);
    let make_string = module.declare_func_in_func(runtime_funcs.make_string, &mut ctx.func);
    let make_none = module.declare_func_in_func(runtime_funcs.make_none, &mut ctx.func);
    let add = module.declare_func_in_func(runtime_funcs.add, &mut ctx.func);
    let sub = module.declare_func_in_func(runtime_funcs.sub, &mut ctx.func);
    let less_than = module.declare_func_in_func(runtime_funcs.less_than, &mut ctx.func);
    let is_truthy = module.declare_func_in_func(runtime_funcs.is_truthy, &mut ctx.func);
    let _print = module.declare_func_in_func(runtime_funcs.print, &mut ctx.func);
    let load_global = module.declare_func_in_func(runtime_funcs.load_global, &mut ctx.func);
    let store_global = module.declare_func_in_func(runtime_funcs.store_global, &mut ctx.func);
    let set_error = module.declare_func_in_func(runtime_funcs.set_error, &mut ctx.func);

    let mut builder_ctx = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);

    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);
    let ctx_param = builder.block_params(entry_block)[0];

    let ptr_align_shift = (ptr_size as u32).trailing_zeros() as u8;
    let stack_slot = builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
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
            let value = builder.block_params(entry_block)[index + 1];
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
            Instruction::DefineFunction { .. } => {
                bail!("Function definitions are not supported in the JIT backend");
            }
            Instruction::LoadName(name) => {
                let local_index = local_indices.get(name).copied();
                let global_index = global_indices.get(name).copied();
                if let Some(local_idx) = local_index {
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
                    if let Some(index) = global_index {
                        let idx_val = builder.ins().iconst(types::I64, index);
                        let call = builder.ins().call(load_global, &[ctx_param, idx_val]);
                        let result = builder.inst_results(call)[0];
                        let ok_block = check_null(&mut builder, result);
                        builder.switch_to_block(ok_block);
                        builder.ins().store(MemFlags::new(), result, tmp_addr, 0);
                        builder.ins().jump(cont_block, &[]);
                    } else {
                        emit_error(
                            module,
                            string_data,
                            &mut builder,
                            set_error,
                            ctx_param,
                            &format!("Undefined variable '{name}'"),
                        )?;
                        builder.ins().jump(error_block, &[]);
                    }

                    builder.switch_to_block(cont_block);
                    let value = builder.ins().load(ptr_type, MemFlags::new(), tmp_addr, 0);
                    push_value(&mut builder, value);
                } else if let Some(index) = global_index {
                    let idx_val = builder.ins().iconst(types::I64, index);
                    let call = builder.ins().call(load_global, &[ctx_param, idx_val]);
                    let result = builder.inst_results(call)[0];
                    let ok_block = check_null(&mut builder, result);
                    builder.switch_to_block(ok_block);
                    push_value(&mut builder, result);
                } else {
                    emit_error(
                        module,
                        string_data,
                        &mut builder,
                        set_error,
                        ctx_param,
                        &format!("Undefined variable '{name}'"),
                    )?;
                    builder.ins().jump(error_block, &[]);
                    terminated = true;
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
                } else if let Some(index) = global_indices.get(name).copied() {
                    let idx_val = builder.ins().iconst(types::I64, index);
                    builder
                        .ins()
                        .call(store_global, &[ctx_param, idx_val, value]);
                } else {
                    emit_error(
                        module,
                        string_data,
                        &mut builder,
                        set_error,
                        ctx_param,
                        &format!("Undefined variable '{name}'"),
                    )?;
                    builder.ins().jump(error_block, &[]);
                    terminated = true;
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
            Instruction::Call { .. } => {
                bail!("Generic calls are not supported in the JIT backend");
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

fn emit_error(
    module: &mut JITModule,
    string_data: &mut StringData,
    builder: &mut FunctionBuilder,
    set_error: cranelift_codegen::ir::FuncRef,
    ctx_param: cranelift_codegen::ir::Value,
    message: &str,
) -> Result<()> {
    let (data_id, len) = string_data.declare(module, "err", message)?;
    let gv = module.declare_data_in_func(data_id, builder.func);
    let ptr_type = builder.func.dfg.value_type(ctx_param);
    let data_ptr = builder.ins().global_value(ptr_type, gv);
    let len_val = builder.ins().iconst(types::I64, len);
    builder
        .ins()
        .call(set_error, &[ctx_param, data_ptr, len_val]);
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
        Instruction::Add | Instruction::Sub | Instruction::LessThan => -1,
        Instruction::Jump(_) | Instruction::Return => 0,
        Instruction::ReturnValue => -1,
    }
}
