//! Bytecode-to-Cranelift lowering for the JIT backend.
//!
//! This module translates VM-style stack bytecode into Cranelift IR and wires
//! every semantic operation through imported runtime hooks.

use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

use anyhow::{Result, bail};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{
    AbiParam, Block, FuncRef, InstBuilder, MemFlags, Signature, Type, Value, types,
};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module};

use crate::ast::Program;
use crate::bytecode::{Instruction, compile};

use super::PreparedProgram;
use super::runtime::{self, CompiledFunctionPointer, EntryFunction};

/// Compiles the bytecode form of `program` into executable functions in a JIT module.
///
/// Emission pattern:
/// 1. Compile AST to VM-style bytecode.
/// 2. Declare all function symbols.
/// 3. Lower each function body via `define_function`.
/// 4. Finalize machine code and capture entry pointers.
pub(super) fn prepare_program(program: &Program) -> Result<PreparedProgram> {
    let compiled = compile(program)?;
    let callable_count = compiled.callables.len();

    let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())?;
    runtime::register_runtime_symbols(&mut builder);

    let mut module = JITModule::new(builder);
    let ptr_type = module.target_config().pointer_type();
    let runtime_funcs = declare_runtime_functions(&mut module, ptr_type)?;
    let mut string_data = StringData::new();

    // Declare all function symbols up front so later lowering can reference them.
    let mut function_ids = Vec::with_capacity(callable_count);
    // Lower each user-defined function into Cranelift IR, one function at a time.
    for callable_id in 0..callable_count {
        let func_sig = value_function_signature(&mut module, ptr_type);
        let func_id = module.declare_function(
            &function_symbol(callable_id as u32),
            Linkage::Local,
            &func_sig,
        )?;
        function_ids.push(func_id);
    }
    let main_sig = value_function_signature(&mut module, ptr_type);
    let main_id = module.declare_function("run_main", Linkage::Local, &main_sig)?;

    for (callable_id, callable) in compiled.callables.iter().enumerate() {
        define_function(
            &mut module,
            &runtime_funcs,
            &mut string_data,
            &callable.name,
            function_ids[callable_id],
            &callable.function.code,
        )?;
    }

    define_function(
        &mut module,
        &runtime_funcs,
        &mut string_data,
        "run_main",
        main_id,
        &compiled.main,
    )?;

    // Finalize all emitted function bodies into executable machine code.
    module.finalize_definitions()?;
    let entry = module.get_finalized_function(main_id);
    let entry: EntryFunction = unsafe { std::mem::transmute(entry) };

    let mut functions = Vec::with_capacity(callable_count);
    for (callable_id, callable) in compiled.callables.iter().enumerate() {
        let entry = module.get_finalized_function(function_ids[callable_id]);
        let entry: EntryFunction = unsafe { std::mem::transmute(entry) };
        functions.push(CompiledFunctionPointer {
            name: callable.name.clone(),
            entry,
            arity: callable.function.params.len(),
            params: callable.function.params.clone(),
        });
    }

    Ok(PreparedProgram {
        _module: module,
        entry,
        functions: Arc::new(functions),
    })
}

/// Lookup table from runtime hook IDs to imported Cranelift function IDs.
struct RuntimeFunctions {
    by_id: HashMap<runtime::RuntimeFunctionId, FuncId>,
}

impl RuntimeFunctions {
    fn get(&self, id: runtime::RuntimeFunctionId) -> Result<FuncId> {
        self.by_id
            .get(&id)
            .copied()
            .ok_or_else(|| anyhow::anyhow!("Missing runtime function import: {id:?}"))
    }
}

/// Interns string literals into data objects with unique symbol names.
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

/// Signature for compiled JIT functions: `(Runtime*, args_ptr) -> Value*`.
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

fn abi_type_to_ir_type(
    ptr_type: cranelift_codegen::ir::Type,
    abi_type: runtime::RuntimeAbiType,
) -> cranelift_codegen::ir::Type {
    match abi_type {
        runtime::RuntimeAbiType::Ptr => ptr_type,
        runtime::RuntimeAbiType::I64 => types::I64,
        runtime::RuntimeAbiType::I8 => types::I8,
    }
}

/// Builds a Cranelift call signature from explicit runtime ABI parameter/return type lists.
fn runtime_function_signature(
    module: &mut JITModule,
    ptr_type: cranelift_codegen::ir::Type,
    param_types: &[runtime::RuntimeAbiType],
    return_types: &[runtime::RuntimeAbiType],
) -> Signature {
    let mut sig = module.make_signature();
    for abi_type in param_types {
        sig.params
            .push(AbiParam::new(abi_type_to_ir_type(ptr_type, *abi_type)));
    }
    for abi_type in return_types {
        sig.returns
            .push(AbiParam::new(abi_type_to_ir_type(ptr_type, *abi_type)));
    }
    sig
}

/// Declares and imports all runtime helper symbols needed by JIT-generated code.
fn declare_runtime_functions(
    module: &mut JITModule,
    ptr_type: cranelift_codegen::ir::Type,
) -> Result<RuntimeFunctions> {
    let mut by_id = HashMap::new();
    for spec in runtime::runtime_function_specs() {
        let signature =
            runtime_function_signature(module, ptr_type, spec.param_types, spec.return_types);
        let func_id = module.declare_function(spec.symbol, Linkage::Import, &signature)?;
        by_id.insert(spec.id, func_id);
    }
    Ok(RuntimeFunctions { by_id })
}

#[derive(Clone, Copy)]
/// Per-function imported runtime call handles used during instruction lowering.
struct RuntimeCalls {
    make_int: FuncRef,
    make_bool: FuncRef,
    make_string: FuncRef,
    make_none: FuncRef,
    make_function: FuncRef,
    make_list: FuncRef,
    make_dict: FuncRef,
    define_class: FuncRef,
    add: FuncRef,
    sub: FuncRef,
    less_than: FuncRef,
    is_truthy: FuncRef,
    call_value: FuncRef,
    load_name: FuncRef,
    store_name: FuncRef,
    load_attr: FuncRef,
    store_attr: FuncRef,
    load_index: FuncRef,
    store_index_name: FuncRef,
}

impl RuntimeCalls {
    fn import(
        module: &mut JITModule,
        runtime_funcs: &RuntimeFunctions,
        func: &mut cranelift_codegen::ir::Function,
    ) -> Result<Self> {
        Ok(Self {
            make_int: module.declare_func_in_func(
                runtime_funcs.get(runtime::RuntimeFunctionId::MakeInt)?,
                func,
            ),
            make_bool: module.declare_func_in_func(
                runtime_funcs.get(runtime::RuntimeFunctionId::MakeBool)?,
                func,
            ),
            make_string: module.declare_func_in_func(
                runtime_funcs.get(runtime::RuntimeFunctionId::MakeString)?,
                func,
            ),
            make_none: module.declare_func_in_func(
                runtime_funcs.get(runtime::RuntimeFunctionId::MakeNone)?,
                func,
            ),
            make_function: module.declare_func_in_func(
                runtime_funcs.get(runtime::RuntimeFunctionId::MakeFunction)?,
                func,
            ),
            make_list: module.declare_func_in_func(
                runtime_funcs.get(runtime::RuntimeFunctionId::MakeList)?,
                func,
            ),
            make_dict: module.declare_func_in_func(
                runtime_funcs.get(runtime::RuntimeFunctionId::MakeDict)?,
                func,
            ),
            define_class: module.declare_func_in_func(
                runtime_funcs.get(runtime::RuntimeFunctionId::DefineClass)?,
                func,
            ),
            add: module
                .declare_func_in_func(runtime_funcs.get(runtime::RuntimeFunctionId::Add)?, func),
            sub: module
                .declare_func_in_func(runtime_funcs.get(runtime::RuntimeFunctionId::Sub)?, func),
            less_than: module.declare_func_in_func(
                runtime_funcs.get(runtime::RuntimeFunctionId::LessThan)?,
                func,
            ),
            is_truthy: module.declare_func_in_func(
                runtime_funcs.get(runtime::RuntimeFunctionId::IsTruthy)?,
                func,
            ),
            call_value: module
                .declare_func_in_func(runtime_funcs.get(runtime::RuntimeFunctionId::Call)?, func),
            load_name: module.declare_func_in_func(
                runtime_funcs.get(runtime::RuntimeFunctionId::LoadName)?,
                func,
            ),
            store_name: module.declare_func_in_func(
                runtime_funcs.get(runtime::RuntimeFunctionId::StoreName)?,
                func,
            ),
            load_attr: module.declare_func_in_func(
                runtime_funcs.get(runtime::RuntimeFunctionId::LoadAttr)?,
                func,
            ),
            store_attr: module.declare_func_in_func(
                runtime_funcs.get(runtime::RuntimeFunctionId::StoreAttr)?,
                func,
            ),
            load_index: module.declare_func_in_func(
                runtime_funcs.get(runtime::RuntimeFunctionId::LoadIndex)?,
                func,
            ),
            store_index_name: module.declare_func_in_func(
                runtime_funcs.get(runtime::RuntimeFunctionId::StoreIndexName)?,
                func,
            ),
        })
    }
}

/// Mutable lowering state shared while translating one bytecode function body.
///
/// This centralizes operand-stack addressing, control-flow blocks, and runtime
/// call handles so individual instruction emitters stay small.
struct LoweringContext {
    ptr_type: Type,
    ptr_size: i64,
    ctx_param: Value,
    runtime: RuntimeCalls,
    sp_var: Variable,
    stack_base: Value,
    call_args_base: Value,
    blocks: Vec<Block>,
    error_block: Block,
    code_len: usize,
}

impl LoweringContext {
    fn block(&self, index: usize) -> Block {
        self.blocks[index]
    }

    fn load_sp(&self, builder: &mut FunctionBuilder) -> Value {
        builder.use_var(self.sp_var)
    }

    fn stack_addr(&self, builder: &mut FunctionBuilder, index: Value) -> Value {
        let offset = builder.ins().imul_imm(index, self.ptr_size);
        builder.ins().iadd(self.stack_base, offset)
    }

    fn push_value(&self, builder: &mut FunctionBuilder, value: Value) {
        let sp_value = self.load_sp(builder);
        let addr = self.stack_addr(builder, sp_value);
        builder.ins().store(MemFlags::new(), value, addr, 0);
        let new_sp = builder.ins().iadd_imm(sp_value, 1);
        builder.def_var(self.sp_var, new_sp);
    }

    fn pop_value(&self, builder: &mut FunctionBuilder) -> Value {
        let sp_value = self.load_sp(builder);
        let new_sp = builder.ins().iadd_imm(sp_value, -1);
        builder.def_var(self.sp_var, new_sp);
        let addr = self.stack_addr(builder, new_sp);
        builder.ins().load(self.ptr_type, MemFlags::new(), addr, 0)
    }

    /// Emits null-guard branch pattern for runtime calls.
    ///
    /// Returns a fresh `ok_block` the caller should `switch_to_block` on success.
    fn check_null(&self, builder: &mut FunctionBuilder, value: Value) -> Block {
        let ok_block = builder.create_block();
        let is_null = builder.ins().icmp_imm(IntCC::Equal, value, 0);
        builder
            .ins()
            .brif(is_null, self.error_block, &[], ok_block, &[]);
        ok_block
    }

    fn resolve_target(&self, ip: usize, offset: isize) -> Result<usize> {
        resolve_relative_target(ip, offset, self.code_len)
    }
}

fn create_explicit_stack_slot(
    builder: &mut FunctionBuilder,
    byte_size: u32,
    align_shift: u8,
) -> cranelift_codegen::ir::StackSlot {
    builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
        cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
        byte_size,
        align_shift,
    ))
}

/// Materializes a string literal as readonly data and returns `(ptr, len)` IR values.
///
/// Lowering pattern:
/// 1. Declare data object in the JIT module.
/// 2. Reference it via `global_value`.
/// 3. Build an immediate i64 length constant.
fn declare_string_literal(
    module: &mut JITModule,
    string_data: &mut StringData,
    builder: &mut FunctionBuilder,
    ptr_type: Type,
    prefix: &str,
    value: &str,
) -> Result<(Value, Value)> {
    let (data_id, len) = string_data.declare(module, prefix, value)?;
    let gv = module.declare_data_in_func(data_id, builder.func);
    let data_ptr = builder.ins().global_value(ptr_type, gv);
    let len_val = builder.ins().iconst(types::I64, len);
    Ok((data_ptr, len_val))
}

#[allow(clippy::too_many_arguments)]
/// Lowers one compiled bytecode function into a single Cranelift function body.
///
/// High-level emitted pattern:
/// 1. Import runtime helper symbols used by bytecode operations.
/// 2. Allocate fixed stack slots for operand stack and call-argument scratch.
/// 3. Create one Cranelift block per bytecode instruction plus exit/error blocks.
/// 4. Dispatch each instruction to `emit_instruction`.
/// 5. Return `None` on normal fallthrough or null pointer on runtime error.
fn define_function(
    module: &mut JITModule,
    runtime_funcs: &RuntimeFunctions,
    string_data: &mut StringData,
    function_name: &str,
    func_id: FuncId,
    code: &[Instruction],
) -> Result<()> {
    let ptr_type = module.target_config().pointer_type();
    let stack_size = max_stack_depth(code)?.max(1);
    let ptr_size = ptr_type.bytes() as i64;

    // Build this function in an isolated Cranelift compilation context.
    let mut ctx = module.make_context();
    ctx.func.signature = value_function_signature(module, ptr_type);
    let runtime_calls = RuntimeCalls::import(module, runtime_funcs, &mut ctx.func)?;

    let mut builder_ctx = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);

    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);
    let ctx_param = builder.block_params(entry_block)[0];
    let _args_param = builder.block_params(entry_block)[1];

    // Reserve stack slots for the VM operand stack and call scratch space.
    let ptr_align_shift = (ptr_size as u32).trailing_zeros() as u8;
    let stack_slot_bytes = (stack_size as u32) * (ptr_size as u32);
    let stack_slot = create_explicit_stack_slot(&mut builder, stack_slot_bytes, ptr_align_shift);
    let call_args_slot =
        create_explicit_stack_slot(&mut builder, stack_slot_bytes, ptr_align_shift);

    // `stack_addr` returns a frame-relative pointer to the start of a stack slot.
    // These are the base addresses for our VM operand stack and call scratch space.
    let stack_base = builder.ins().stack_addr(ptr_type, stack_slot, 0);
    let call_args_base = builder.ins().stack_addr(ptr_type, call_args_slot, 0);
    let sp_var = Variable::from_u32(0);
    builder.declare_var(sp_var, ptr_type);
    // `sp_var` stores a logical stack depth (number of values), not a byte offset.
    let zero = builder.ins().iconst(ptr_type, 0);
    builder.def_var(sp_var, zero);

    // Create one block per bytecode instruction plus a dedicated exit block.
    let mut blocks = Vec::with_capacity(code.len() + 1);
    for _ in 0..=code.len() {
        blocks.push(builder.create_block());
    }
    let error_block = builder.create_block();

    let lowering = LoweringContext {
        ptr_type,
        ptr_size,
        ctx_param,
        runtime: runtime_calls,
        sp_var,
        stack_base,
        call_args_base,
        blocks,
        error_block,
        code_len: code.len(),
    };

    builder.ins().jump(lowering.block(0), &[]);

    // Translate bytecode instructions into Cranelift IR.
    for (idx, instruction) in code.iter().enumerate() {
        builder.switch_to_block(lowering.block(idx));
        let terminated = emit_instruction(
            &mut builder,
            module,
            string_data,
            &lowering,
            idx,
            instruction,
        )?;
        if !terminated {
            builder.ins().jump(lowering.block(idx + 1), &[]);
        }
    }

    builder.switch_to_block(lowering.block(code.len()));
    let call = builder
        .ins()
        .call(lowering.runtime.make_none, &[lowering.ctx_param]);
    let result = builder.inst_results(call)[0];
    builder.ins().return_(&[result]);

    builder.switch_to_block(lowering.error_block);
    let null_ptr = builder.ins().iconst(ptr_type, 0);
    builder.ins().return_(&[null_ptr]);

    builder.seal_all_blocks();
    builder.finalize();
    if super::dump_clif_enabled() {
        eprintln!("; ---- CLIF {function_name} ----");
        eprintln!("{}", ctx.func.display());
    }
    module.define_function(func_id, &mut ctx)?;
    module.clear_context(&mut ctx);

    Ok(())
}

fn store_to_scope_with_name_ptr(
    builder: &mut FunctionBuilder,
    lowering: &LoweringContext,
    name_ptr: Value,
    name_len: Value,
    value: Value,
) {
    builder.ins().call(
        lowering.runtime.store_name,
        &[lowering.ctx_param, name_ptr, name_len, value],
    );
}

fn store_to_scope(
    builder: &mut FunctionBuilder,
    module: &mut JITModule,
    string_data: &mut StringData,
    lowering: &LoweringContext,
    name: &str,
    value: Value,
) -> Result<()> {
    let (name_ptr, name_len) = declare_string_literal(
        module,
        string_data,
        builder,
        lowering.ptr_type,
        "name",
        name,
    )?;
    store_to_scope_with_name_ptr(builder, lowering, name_ptr, name_len, value);
    Ok(())
}

/// Lowers class definition metadata and emits a `runtime_define_class` call.
///
/// Lowering pattern:
/// 1. Materialize class name.
/// 2. Materialize method name/callable-id arrays in stack slots.
/// 3. Call `runtime_define_class`.
/// 4. Check for null error and store resulting class object under class name.
fn emit_define_class(
    builder: &mut FunctionBuilder,
    module: &mut JITModule,
    string_data: &mut StringData,
    lowering: &LoweringContext,
    name: &str,
    methods: &[(String, u32)],
) -> Result<()> {
    let (name_ptr, name_len_val) = declare_string_literal(
        module,
        string_data,
        builder,
        lowering.ptr_type,
        "class_name",
        name,
    )?;

    let method_count = methods.len();
    let method_count_val = builder.ins().iconst(types::I64, method_count as i64);
    let zero_ptr = builder.ins().iconst(lowering.ptr_type, 0);

    let (method_name_ptrs_base, method_name_lens_base, method_callable_ids_base) =
        if method_count == 0 {
            (zero_ptr, zero_ptr, zero_ptr)
        } else {
            let ptr_align_shift = (lowering.ptr_size as u32).trailing_zeros() as u8;
            let len_align_shift = (std::mem::size_of::<i64>() as u32).trailing_zeros() as u8;
            let pointer_array_bytes = (method_count as u32) * (lowering.ptr_size as u32);
            let length_array_bytes = (method_count as u32) * (std::mem::size_of::<i64>() as u32);
            let method_name_ptrs_slot =
                create_explicit_stack_slot(builder, pointer_array_bytes, ptr_align_shift);
            let method_name_lens_slot =
                create_explicit_stack_slot(builder, length_array_bytes, len_align_shift);
            let method_callable_ids_slot =
                create_explicit_stack_slot(builder, length_array_bytes, len_align_shift);

            let method_name_ptrs_base =
                builder
                    .ins()
                    .stack_addr(lowering.ptr_type, method_name_ptrs_slot, 0);
            let method_name_lens_base =
                builder
                    .ins()
                    .stack_addr(lowering.ptr_type, method_name_lens_slot, 0);
            let method_callable_ids_base =
                builder
                    .ins()
                    .stack_addr(lowering.ptr_type, method_callable_ids_slot, 0);

            for (method_index, (method_name, method_callable_id)) in methods.iter().enumerate() {
                let (method_name_ptr, method_name_len_val) = declare_string_literal(
                    module,
                    string_data,
                    builder,
                    lowering.ptr_type,
                    "method_name",
                    method_name,
                )?;
                let method_name_ptr_addr = builder.ins().iadd_imm(
                    method_name_ptrs_base,
                    (method_index as i64) * lowering.ptr_size,
                );
                let method_name_len_addr = builder.ins().iadd_imm(
                    method_name_lens_base,
                    (method_index as i64) * (std::mem::size_of::<i64>() as i64),
                );
                builder
                    .ins()
                    .store(MemFlags::new(), method_name_ptr, method_name_ptr_addr, 0);
                builder.ins().store(
                    MemFlags::new(),
                    method_name_len_val,
                    method_name_len_addr,
                    0,
                );

                let method_callable_id_val = builder
                    .ins()
                    .iconst(types::I64, i64::from(*method_callable_id));
                let method_callable_id_addr = builder.ins().iadd_imm(
                    method_callable_ids_base,
                    (method_index as i64) * (std::mem::size_of::<i64>() as i64),
                );
                builder.ins().store(
                    MemFlags::new(),
                    method_callable_id_val,
                    method_callable_id_addr,
                    0,
                );
            }

            (
                method_name_ptrs_base,
                method_name_lens_base,
                method_callable_ids_base,
            )
        };

    let class_value = emit_checked_runtime_call_result(
        builder,
        lowering,
        lowering.runtime.define_class,
        &[
            lowering.ctx_param,
            name_ptr,
            name_len_val,
            method_name_ptrs_base,
            method_name_lens_base,
            method_callable_ids_base,
            method_count_val,
        ],
    );
    store_to_scope_with_name_ptr(builder, lowering, name_ptr, name_len_val, class_value);
    Ok(())
}

/// Lowers `DefineFunction` by creating a runtime function object from callable ID.
///
/// Lowering pattern:
/// 1. Materialize callable ID immediate.
/// 2. Call `runtime_make_function`.
/// 3. Null-check result and store under the declared function name.
fn emit_define_function(
    builder: &mut FunctionBuilder,
    module: &mut JITModule,
    string_data: &mut StringData,
    lowering: &LoweringContext,
    name: &str,
    callable_id: u32,
) -> Result<()> {
    let callable_id_val = builder.ins().iconst(types::I64, i64::from(callable_id));
    let function_value = emit_checked_runtime_call_result(
        builder,
        lowering,
        lowering.runtime.make_function,
        &[lowering.ctx_param, callable_id_val],
    );
    store_to_scope(builder, module, string_data, lowering, name, function_value)
}

/// Lowers `LoadName` into runtime name resolution and pushes result onto operand stack.
///
/// Lowering pattern:
/// 1. Materialize name literal.
/// 2. Call `runtime_load_name`.
/// 3. Branch to shared error block on null.
/// 4. Push loaded value.
fn emit_load_name(
    builder: &mut FunctionBuilder,
    module: &mut JITModule,
    string_data: &mut StringData,
    lowering: &LoweringContext,
    name: &str,
) -> Result<()> {
    let (name_ptr, name_len_val) = declare_string_literal(
        module,
        string_data,
        builder,
        lowering.ptr_type,
        "name",
        name,
    )?;

    emit_checked_runtime_call_and_push(
        builder,
        lowering,
        lowering.runtime.load_name,
        &[lowering.ctx_param, name_ptr, name_len_val],
    );
    Ok(())
}

fn emit_store_name(
    builder: &mut FunctionBuilder,
    module: &mut JITModule,
    string_data: &mut StringData,
    lowering: &LoweringContext,
    name: &str,
) -> Result<()> {
    let value = lowering.pop_value(builder);
    store_to_scope(builder, module, string_data, lowering, name, value)
}

fn emit_store_index(
    builder: &mut FunctionBuilder,
    module: &mut JITModule,
    string_data: &mut StringData,
    lowering: &LoweringContext,
    name: &str,
) -> Result<()> {
    let value = lowering.pop_value(builder);
    let index = lowering.pop_value(builder);
    let (name_ptr, name_len_val) = declare_string_literal(
        module,
        string_data,
        builder,
        lowering.ptr_type,
        "name",
        name,
    )?;
    builder.ins().call(
        lowering.runtime.store_index_name,
        &[lowering.ctx_param, name_ptr, name_len_val, index, value],
    );
    Ok(())
}

fn emit_runtime_call_result(
    builder: &mut FunctionBuilder,
    runtime_function: FuncRef,
    args: &[Value],
) -> Value {
    let call = builder.ins().call(runtime_function, args);
    builder.inst_results(call)[0]
}

fn emit_checked_runtime_call_result(
    builder: &mut FunctionBuilder,
    lowering: &LoweringContext,
    runtime_function: FuncRef,
    args: &[Value],
) -> Value {
    let result = emit_runtime_call_result(builder, runtime_function, args);
    let ok_block = lowering.check_null(builder, result);
    builder.switch_to_block(ok_block);
    result
}

fn emit_runtime_call_and_push(
    builder: &mut FunctionBuilder,
    lowering: &LoweringContext,
    runtime_function: FuncRef,
    args: &[Value],
) {
    let result = emit_runtime_call_result(builder, runtime_function, args);
    lowering.push_value(builder, result);
}

fn emit_checked_runtime_call_and_push(
    builder: &mut FunctionBuilder,
    lowering: &LoweringContext,
    runtime_function: FuncRef,
    args: &[Value],
) {
    let result = emit_checked_runtime_call_result(builder, lowering, runtime_function, args);
    lowering.push_value(builder, result);
}

fn pop_binary_values(builder: &mut FunctionBuilder, lowering: &LoweringContext) -> (Value, Value) {
    let right = lowering.pop_value(builder);
    let left = lowering.pop_value(builder);
    (left, right)
}

fn emit_binary_operation(
    builder: &mut FunctionBuilder,
    lowering: &LoweringContext,
    runtime_function: FuncRef,
) {
    let (left, right) = pop_binary_values(builder, lowering);
    emit_checked_runtime_call_and_push(
        builder,
        lowering,
        runtime_function,
        &[lowering.ctx_param, left, right],
    );
}

fn store_stack_values_for_call_args(
    builder: &mut FunctionBuilder,
    lowering: &LoweringContext,
    count: usize,
) {
    for arg_index in (0..count).rev() {
        let arg_value = lowering.pop_value(builder);
        let arg_addr = builder.ins().iadd_imm(
            lowering.call_args_base,
            (arg_index as i64) * lowering.ptr_size,
        );
        builder.ins().store(MemFlags::new(), arg_value, arg_addr, 0);
    }
}

#[allow(clippy::too_many_arguments)]
/// Lowers one bytecode instruction into Cranelift IR.
///
/// Pattern shape by family:
/// - Stack producers/consumers: runtime call + `push_value`/`pop_value`.
/// - Control flow: `brif`/`jump` to pre-created per-instruction blocks.
/// - Errorable runtime calls: null-check and branch to shared error block.
/// - Returns: emit direct `return_` and mark instruction as terminating.
fn emit_instruction(
    builder: &mut FunctionBuilder,
    module: &mut JITModule,
    string_data: &mut StringData,
    lowering: &LoweringContext,
    idx: usize,
    instruction: &Instruction,
) -> Result<bool> {
    match instruction {
        Instruction::PushInt(value) => {
            let imm = builder.ins().iconst(types::I64, *value);
            emit_runtime_call_and_push(
                builder,
                lowering,
                lowering.runtime.make_int,
                &[lowering.ctx_param, imm],
            );
            Ok(false)
        }
        Instruction::PushBool(value) => {
            let imm = builder.ins().iconst(types::I8, if *value { 1 } else { 0 });
            emit_runtime_call_and_push(
                builder,
                lowering,
                lowering.runtime.make_bool,
                &[lowering.ctx_param, imm],
            );
            Ok(false)
        }
        Instruction::PushString(value) => {
            let (data_ptr, len_val) = declare_string_literal(
                module,
                string_data,
                builder,
                lowering.ptr_type,
                "str",
                value,
            )?;
            emit_runtime_call_and_push(
                builder,
                lowering,
                lowering.runtime.make_string,
                &[lowering.ctx_param, data_ptr, len_val],
            );
            Ok(false)
        }
        Instruction::BuildList(count) => {
            store_stack_values_for_call_args(builder, lowering, *count);
            let count_val = builder.ins().iconst(types::I64, *count as i64);
            emit_checked_runtime_call_and_push(
                builder,
                lowering,
                lowering.runtime.make_list,
                &[lowering.ctx_param, lowering.call_args_base, count_val],
            );
            Ok(false)
        }
        Instruction::BuildDict(count) => {
            store_stack_values_for_call_args(builder, lowering, count * 2);
            let count_val = builder.ins().iconst(types::I64, *count as i64);
            emit_checked_runtime_call_and_push(
                builder,
                lowering,
                lowering.runtime.make_dict,
                &[lowering.ctx_param, lowering.call_args_base, count_val],
            );
            Ok(false)
        }
        Instruction::PushNone => {
            emit_runtime_call_and_push(
                builder,
                lowering,
                lowering.runtime.make_none,
                &[lowering.ctx_param],
            );
            Ok(false)
        }
        Instruction::DefineClass { name, methods } => {
            emit_define_class(builder, module, string_data, lowering, name, methods)?;
            Ok(false)
        }
        Instruction::DefineFunction { name, callable_id } => {
            emit_define_function(builder, module, string_data, lowering, name, *callable_id)?;
            Ok(false)
        }
        Instruction::LoadName(name) => {
            emit_load_name(builder, module, string_data, lowering, name)?;
            Ok(false)
        }
        Instruction::StoreName(name) => {
            emit_store_name(builder, module, string_data, lowering, name)?;
            Ok(false)
        }
        Instruction::Add => {
            emit_binary_operation(builder, lowering, lowering.runtime.add);
            Ok(false)
        }
        Instruction::Sub => {
            emit_binary_operation(builder, lowering, lowering.runtime.sub);
            Ok(false)
        }
        Instruction::LessThan => {
            emit_binary_operation(builder, lowering, lowering.runtime.less_than);
            Ok(false)
        }
        Instruction::LoadIndex => {
            let index = lowering.pop_value(builder);
            let object = lowering.pop_value(builder);
            emit_checked_runtime_call_and_push(
                builder,
                lowering,
                lowering.runtime.load_index,
                &[lowering.ctx_param, object, index],
            );
            Ok(false)
        }
        Instruction::StoreIndex(name) => {
            emit_store_index(builder, module, string_data, lowering, name)?;
            Ok(false)
        }
        Instruction::LoadAttr(attribute) => {
            let object = lowering.pop_value(builder);
            let (attribute_ptr, attribute_len_val) = declare_string_literal(
                module,
                string_data,
                builder,
                lowering.ptr_type,
                "attr",
                attribute,
            )?;
            emit_checked_runtime_call_and_push(
                builder,
                lowering,
                lowering.runtime.load_attr,
                &[lowering.ctx_param, object, attribute_ptr, attribute_len_val],
            );
            Ok(false)
        }
        Instruction::StoreAttr(attribute) => {
            let object = lowering.pop_value(builder);
            let value = lowering.pop_value(builder);
            let (attribute_ptr, attribute_len_val) = declare_string_literal(
                module,
                string_data,
                builder,
                lowering.ptr_type,
                "attr",
                attribute,
            )?;
            builder.ins().call(
                lowering.runtime.store_attr,
                &[
                    lowering.ctx_param,
                    object,
                    attribute_ptr,
                    attribute_len_val,
                    value,
                ],
            );
            Ok(false)
        }
        Instruction::Call { argc } => {
            store_stack_values_for_call_args(builder, lowering, *argc);
            let callee = lowering.pop_value(builder);
            let argc_val = builder.ins().iconst(types::I64, *argc as i64);
            emit_checked_runtime_call_and_push(
                builder,
                lowering,
                lowering.runtime.call_value,
                &[
                    lowering.ctx_param,
                    callee,
                    lowering.call_args_base,
                    argc_val,
                ],
            );
            Ok(false)
        }
        Instruction::JumpIfFalse(target) => {
            let target_index = lowering.resolve_target(idx, *target)?;
            let value = lowering.pop_value(builder);
            let call = builder.ins().call(lowering.runtime.is_truthy, &[value]);
            let truthy = builder.inst_results(call)[0];
            builder.ins().brif(
                truthy,
                lowering.block(idx + 1),
                &[],
                lowering.block(target_index),
                &[],
            );
            Ok(true)
        }
        Instruction::Jump(target) => {
            let target_index = lowering.resolve_target(idx, *target)?;
            builder.ins().jump(lowering.block(target_index), &[]);
            Ok(true)
        }
        Instruction::Pop => {
            lowering.pop_value(builder);
            Ok(false)
        }
        Instruction::Return => {
            let result = emit_runtime_call_result(
                builder,
                lowering.runtime.make_none,
                &[lowering.ctx_param],
            );
            builder.ins().return_(&[result]);
            Ok(true)
        }
        Instruction::ReturnValue => {
            let value = lowering.pop_value(builder);
            builder.ins().return_(&[value]);
            Ok(true)
        }
    }
}

fn function_symbol(callable_id: u32) -> String {
    format!("fn_{callable_id}")
}

/// Computes maximum operand-stack depth for a bytecode block and validates stack consistency.
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

/// Resolves VM-style relative jump offsets into absolute instruction indices.
fn resolve_relative_target(ip: usize, offset: isize, code_len: usize) -> Result<usize> {
    let target = (ip as isize) + 1 + offset;
    if target < 0 || (target as usize) > code_len {
        bail!("Invalid jump target at {ip} with offset {offset}");
    }
    Ok(target as usize)
}

/// Propagates inferred stack depth to a control-flow successor and checks join consistency.
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

/// Returns net operand-stack delta for one bytecode instruction.
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
        Instruction::BuildDict(count) => 1 - ((*count as i32) * 2),
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

#[cfg(test)]
mod tests {
    use super::{max_stack_depth, resolve_relative_target};
    use crate::bytecode::Instruction;

    #[test]
    fn computes_max_stack_depth_for_linear_code() {
        let code = vec![
            Instruction::PushInt(1),
            Instruction::PushInt(2),
            Instruction::Add,
            Instruction::Pop,
        ];
        let depth = max_stack_depth(&code).expect("stack depth should be valid");
        assert_eq!(depth, 2);
    }

    #[test]
    fn computes_max_stack_depth_for_branching_code() {
        let code = vec![
            Instruction::PushBool(true),
            Instruction::JumpIfFalse(2),
            Instruction::PushInt(1),
            Instruction::Jump(1),
            Instruction::PushInt(2),
            Instruction::Pop,
        ];
        let depth = max_stack_depth(&code).expect("stack depth should be valid");
        assert_eq!(depth, 1);
    }

    #[test]
    fn errors_on_stack_underflow() {
        let code = vec![Instruction::Pop];
        let error = max_stack_depth(&code).expect_err("expected stack underflow");
        assert_eq!(error.to_string(), "Bytecode stack underflow at 0");
    }

    #[test]
    fn errors_on_stack_mismatch_at_join_point() {
        let code = vec![
            Instruction::PushBool(true),
            Instruction::JumpIfFalse(1),
            Instruction::PushInt(1),
            Instruction::Return,
        ];
        let error = max_stack_depth(&code).expect_err("expected stack mismatch");
        assert_eq!(error.to_string(), "Bytecode stack mismatch at 3");
    }

    #[test]
    fn resolves_relative_jump_targets() {
        let target = resolve_relative_target(2, -2, 6).expect("valid jump target");
        assert_eq!(target, 1);
    }

    #[test]
    fn rejects_invalid_relative_jump_target() {
        let error = resolve_relative_target(0, -2, 3).expect_err("expected invalid jump target");
        assert_eq!(error.to_string(), "Invalid jump target at 0 with offset -2");
    }
}
