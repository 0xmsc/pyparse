use std::collections::{BTreeSet, HashMap, VecDeque};
use std::sync::Arc;

use anyhow::{Result, bail};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{AbiParam, InstBuilder, MemFlags, Signature, types};
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, FuncId, Linkage, Module};

use crate::ast::Program;
use crate::bytecode::{Instruction, compile};

use super::PreparedProgram;
use super::runtime::{self, CompiledFunctionPointer, EntryFunction};

pub(super) fn prepare_program(program: &Program) -> Result<PreparedProgram> {
    let compiled = compile(program)?;
    let mut function_names: Vec<&String> = compiled.functions.keys().collect();
    function_names.sort();

    let mut builder = JITBuilder::new(cranelift_module::default_libcall_names())?;
    runtime::register_runtime_symbols(&mut builder);

    let mut module = JITModule::new(builder);
    let ptr_type = module.target_config().pointer_type();
    let runtime_funcs = declare_runtime_functions(&mut module, ptr_type)?;
    let mut string_data = StringData::new();

    // Declare all function symbols up front so later lowering can reference them.
    let mut function_ids = HashMap::new();
    let mut function_arities = HashMap::new();
    // Lower each user-defined function into Cranelift IR, one function at a time.
    for name in &function_names {
        let arity = compiled
            .functions
            .get(*name)
            .ok_or_else(|| anyhow::anyhow!("Missing function '{name}'"))?
            .params
            .len();
        let func_sig = value_function_signature(&mut module, ptr_type);
        let func_id = module.declare_function(&function_symbol(name), Linkage::Local, &func_sig)?;
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
            name,
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
        "run_main",
        main_id,
        &[],
        &compiled.main,
        &BTreeSet::new(),
    )?;

    // Finalize all emitted function bodies into executable machine code.
    module.finalize_definitions()?;
    let entry = module.get_finalized_function(main_id);
    let entry: EntryFunction = unsafe { std::mem::transmute(entry) };

    let mut functions = HashMap::new();
    for (name, func_id) in &function_ids {
        let entry = module.get_finalized_function(*func_id);
        let entry: EntryFunction = unsafe { std::mem::transmute(entry) };
        let arity = function_arities
            .get(name)
            .copied()
            .ok_or_else(|| anyhow::anyhow!("Missing arity for function '{name}'"))?;
        functions.insert(name.clone(), CompiledFunctionPointer { entry, arity });
    }

    Ok(PreparedProgram {
        _module: module,
        entry,
        functions: Arc::new(functions),
    })
}

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

fn runtime_function_signature(
    module: &mut JITModule,
    ptr_type: cranelift_codegen::ir::Type,
    signature: runtime::RuntimeFunctionSignature,
) -> Signature {
    let mut sig = module.make_signature();
    match signature {
        runtime::RuntimeFunctionSignature::CtxI64ToValue => {
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(ptr_type));
        }
        runtime::RuntimeFunctionSignature::CtxI8ToValue => {
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I8));
            sig.returns.push(AbiParam::new(ptr_type));
        }
        runtime::RuntimeFunctionSignature::CtxPtrI64ToValue => {
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(ptr_type));
        }
        runtime::RuntimeFunctionSignature::CtxToValue => {
            sig.params.push(AbiParam::new(ptr_type));
            sig.returns.push(AbiParam::new(ptr_type));
        }
        runtime::RuntimeFunctionSignature::CtxPtrPtrI64ToValue => {
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(ptr_type));
        }
        runtime::RuntimeFunctionSignature::CtxDefineClassToValue => {
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(ptr_type));
        }
        runtime::RuntimeFunctionSignature::CtxValueValueToValue => {
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.returns.push(AbiParam::new(ptr_type));
        }
        runtime::RuntimeFunctionSignature::CtxValueValueToI8 => {
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.returns.push(AbiParam::new(types::I8));
        }
        runtime::RuntimeFunctionSignature::ValueToI8 => {
            sig.params.push(AbiParam::new(ptr_type));
            sig.returns.push(AbiParam::new(types::I8));
        }
        runtime::RuntimeFunctionSignature::CtxPtrI64ValueToVoid => {
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(ptr_type));
        }
        runtime::RuntimeFunctionSignature::CtxValuePtrI64ToValue => {
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.returns.push(AbiParam::new(ptr_type));
        }
        runtime::RuntimeFunctionSignature::CtxValuePtrI64ValueToVoid => {
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(ptr_type));
        }
        runtime::RuntimeFunctionSignature::CtxValueValueValueToVoid => {
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
        }
        runtime::RuntimeFunctionSignature::CtxPtrI64ValueValueToVoid => {
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(types::I64));
            sig.params.push(AbiParam::new(ptr_type));
            sig.params.push(AbiParam::new(ptr_type));
        }
    }
    sig
}

fn declare_runtime_functions(
    module: &mut JITModule,
    ptr_type: cranelift_codegen::ir::Type,
) -> Result<RuntimeFunctions> {
    let mut by_id = HashMap::new();
    for spec in runtime::runtime_function_specs() {
        let signature = runtime_function_signature(module, ptr_type, spec.signature);
        let func_id = module.declare_function(spec.symbol, Linkage::Import, &signature)?;
        by_id.insert(spec.id, func_id);
    }
    Ok(RuntimeFunctions { by_id })
}

#[allow(clippy::too_many_arguments)]
fn define_function(
    module: &mut JITModule,
    runtime_funcs: &RuntimeFunctions,
    string_data: &mut StringData,
    function_name: &str,
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

    // Build this function in an isolated Cranelift compilation context.
    let mut ctx = module.make_context();
    ctx.func.signature = value_function_signature(module, ptr_type);
    let make_int = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::MakeInt)?,
        &mut ctx.func,
    );
    let make_bool = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::MakeBool)?,
        &mut ctx.func,
    );
    let make_string = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::MakeString)?,
        &mut ctx.func,
    );
    let make_none = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::MakeNone)?,
        &mut ctx.func,
    );
    let make_function = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::MakeFunction)?,
        &mut ctx.func,
    );
    let make_list = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::MakeList)?,
        &mut ctx.func,
    );
    let define_class = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::DefineClass)?,
        &mut ctx.func,
    );
    let add = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::Add)?,
        &mut ctx.func,
    );
    let sub = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::Sub)?,
        &mut ctx.func,
    );
    let less_than = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::LessThan)?,
        &mut ctx.func,
    );
    let less_than_truthy = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::LessThanTruthy)?,
        &mut ctx.func,
    );
    let is_truthy = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::IsTruthy)?,
        &mut ctx.func,
    );
    let call_value = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::Call)?,
        &mut ctx.func,
    );
    let load_name = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::LoadName)?,
        &mut ctx.func,
    );
    let store_name = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::StoreName)?,
        &mut ctx.func,
    );
    let load_attr = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::LoadAttr)?,
        &mut ctx.func,
    );
    let store_attr = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::StoreAttr)?,
        &mut ctx.func,
    );
    let load_index = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::LoadIndex)?,
        &mut ctx.func,
    );
    let store_index_value = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::StoreIndexValue)?,
        &mut ctx.func,
    );
    let store_index_name = module.declare_func_in_func(
        runtime_funcs.get(runtime::RuntimeFunctionId::StoreIndexName)?,
        &mut ctx.func,
    );

    let mut builder_ctx = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);

    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);
    let ctx_param = builder.block_params(entry_block)[0];
    let args_param = builder.block_params(entry_block)[1];

    // Reserve stack slots for the VM operand stack, call scratch space, and locals.
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

    // `stack_addr` returns a frame-relative pointer to the start of a stack slot.
    // These are the base addresses for our VM operand stack, call scratch space,
    // and scalar temporaries.
    let stack_base = builder.ins().stack_addr(ptr_type, stack_slot, 0);
    let call_args_base = builder.ins().stack_addr(ptr_type, call_args_slot, 0);
    let tmp_addr = builder.ins().stack_addr(ptr_type, tmp_slot, 0);
    let sp_var = Variable::from_u32(0);
    builder.declare_var(sp_var, ptr_type);
    // `sp_var` stores a logical stack depth (number of values), not a byte offset.
    let zero = builder.ins().iconst(ptr_type, 0);
    builder.def_var(sp_var, zero);

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

    // Create one block per bytecode instruction plus a dedicated exit block.
    let mut blocks = Vec::with_capacity(code.len() + 1);
    for _ in 0..=code.len() {
        blocks.push(builder.create_block());
    }
    let error_block = builder.create_block();

    builder.ins().jump(blocks[0], &[]);

    // Translate bytecode instructions into Cranelift IR.
    for (idx, instruction) in code.iter().enumerate() {
        let block = blocks[idx];
        builder.switch_to_block(block);
        let mut terminated = false;

        let _load_sp = |builder: &mut FunctionBuilder| builder.use_var(sp_var);
        // Read current logical stack depth.
        let load_sp = |builder: &mut FunctionBuilder| builder.use_var(sp_var);
        // Translate a logical stack index to an address in `stack_slot`.
        let stack_addr = |builder: &mut FunctionBuilder, index| {
            let offset = builder.ins().imul_imm(index, ptr_size);
            builder.ins().iadd(stack_base, offset)
        };
        // Push: store at current SP, then increment SP.
        let push_value = |builder: &mut FunctionBuilder, value| {
            let sp_value = load_sp(builder);
            let addr = stack_addr(builder, sp_value);
            builder.ins().store(MemFlags::new(), value, addr, 0);
            let new_sp = builder.ins().iadd_imm(sp_value, 1);
            builder.def_var(sp_var, new_sp);
        };
        // Pop: decrement SP first, then load the new top value.
        let pop_value = |builder: &mut FunctionBuilder| {
            let sp_value = load_sp(builder);
            let new_sp = builder.ins().iadd_imm(sp_value, -1);
            builder.def_var(sp_var, new_sp);
            let addr = stack_addr(builder, new_sp);
            builder.ins().load(ptr_type, MemFlags::new(), addr, 0)
        };
        // Runtime helpers return null on error; branch to shared error block.
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
            Instruction::BuildList(count) => {
                for element_index in (0..*count).rev() {
                    let element = pop_value(&mut builder);
                    let element_addr = builder
                        .ins()
                        .iadd_imm(call_args_base, (element_index as i64) * ptr_size);
                    builder
                        .ins()
                        .store(MemFlags::new(), element, element_addr, 0);
                }
                let count_val = builder.ins().iconst(types::I64, *count as i64);
                let call = builder
                    .ins()
                    .call(make_list, &[ctx_param, call_args_base, count_val]);
                let result = builder.inst_results(call)[0];
                let ok_block = check_null(&mut builder, result);
                builder.switch_to_block(ok_block);
                push_value(&mut builder, result);
            }
            Instruction::PushNone => {
                let call = builder.ins().call(make_none, &[ctx_param]);
                let result = builder.inst_results(call)[0];
                push_value(&mut builder, result);
            }
            Instruction::DefineClass { name, methods } => {
                let (name_data_id, name_len) = string_data.declare(module, "class_name", name)?;
                let name_gv = module.declare_data_in_func(name_data_id, builder.func);
                let name_ptr = builder.ins().global_value(ptr_type, name_gv);
                let name_len_val = builder.ins().iconst(types::I64, name_len);

                let method_count = methods.len();
                let method_count_val = builder.ins().iconst(types::I64, method_count as i64);
                let zero_ptr = builder.ins().iconst(ptr_type, 0);

                let (
                    method_name_ptrs_base,
                    method_name_lens_base,
                    method_symbol_ptrs_base,
                    method_symbol_lens_base,
                ) = if method_count == 0 {
                    (zero_ptr, zero_ptr, zero_ptr, zero_ptr)
                } else {
                    let len_align_shift =
                        (std::mem::size_of::<i64>() as u32).trailing_zeros() as u8;
                    let method_name_ptrs_slot =
                        builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
                            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                            (method_count as u32) * (ptr_size as u32),
                            ptr_align_shift,
                        ));
                    let method_name_lens_slot =
                        builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
                            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                            (method_count as u32) * (std::mem::size_of::<i64>() as u32),
                            len_align_shift,
                        ));
                    let method_symbol_ptrs_slot =
                        builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
                            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                            (method_count as u32) * (ptr_size as u32),
                            ptr_align_shift,
                        ));
                    let method_symbol_lens_slot =
                        builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData::new(
                            cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                            (method_count as u32) * (std::mem::size_of::<i64>() as u32),
                            len_align_shift,
                        ));

                    let method_name_ptrs_base =
                        builder.ins().stack_addr(ptr_type, method_name_ptrs_slot, 0);
                    let method_name_lens_base =
                        builder.ins().stack_addr(ptr_type, method_name_lens_slot, 0);
                    let method_symbol_ptrs_base =
                        builder
                            .ins()
                            .stack_addr(ptr_type, method_symbol_ptrs_slot, 0);
                    let method_symbol_lens_base =
                        builder
                            .ins()
                            .stack_addr(ptr_type, method_symbol_lens_slot, 0);

                    for (method_index, (method_name, method_symbol)) in methods.iter().enumerate() {
                        let (method_name_data_id, method_name_len) =
                            string_data.declare(module, "method_name", method_name)?;
                        let method_name_gv =
                            module.declare_data_in_func(method_name_data_id, builder.func);
                        let method_name_ptr = builder.ins().global_value(ptr_type, method_name_gv);
                        let method_name_len_val = builder.ins().iconst(types::I64, method_name_len);
                        let method_name_ptr_addr = builder
                            .ins()
                            .iadd_imm(method_name_ptrs_base, (method_index as i64) * ptr_size);
                        let method_name_len_addr = builder.ins().iadd_imm(
                            method_name_lens_base,
                            (method_index as i64) * (std::mem::size_of::<i64>() as i64),
                        );
                        builder.ins().store(
                            MemFlags::new(),
                            method_name_ptr,
                            method_name_ptr_addr,
                            0,
                        );
                        builder.ins().store(
                            MemFlags::new(),
                            method_name_len_val,
                            method_name_len_addr,
                            0,
                        );

                        let (method_symbol_data_id, method_symbol_len) =
                            string_data.declare(module, "method_symbol", method_symbol)?;
                        let method_symbol_gv =
                            module.declare_data_in_func(method_symbol_data_id, builder.func);
                        let method_symbol_ptr =
                            builder.ins().global_value(ptr_type, method_symbol_gv);
                        let method_symbol_len_val =
                            builder.ins().iconst(types::I64, method_symbol_len);
                        let method_symbol_ptr_addr = builder
                            .ins()
                            .iadd_imm(method_symbol_ptrs_base, (method_index as i64) * ptr_size);
                        let method_symbol_len_addr = builder.ins().iadd_imm(
                            method_symbol_lens_base,
                            (method_index as i64) * (std::mem::size_of::<i64>() as i64),
                        );
                        builder.ins().store(
                            MemFlags::new(),
                            method_symbol_ptr,
                            method_symbol_ptr_addr,
                            0,
                        );
                        builder.ins().store(
                            MemFlags::new(),
                            method_symbol_len_val,
                            method_symbol_len_addr,
                            0,
                        );
                    }

                    (
                        method_name_ptrs_base,
                        method_name_lens_base,
                        method_symbol_ptrs_base,
                        method_symbol_lens_base,
                    )
                };

                let call = builder.ins().call(
                    define_class,
                    &[
                        ctx_param,
                        name_ptr,
                        name_len_val,
                        method_name_ptrs_base,
                        method_name_lens_base,
                        method_symbol_ptrs_base,
                        method_symbol_lens_base,
                        method_count_val,
                    ],
                );
                let class_value = builder.inst_results(call)[0];
                let ok_block = check_null(&mut builder, class_value);
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
                        .store(MemFlags::new(), class_value, local_addr, 0);
                    let set_addr = builder.ins().iadd_imm(locals_set_base, local_idx);
                    let one = builder.ins().iconst(types::I8, 1);
                    builder.ins().store(MemFlags::new(), one, set_addr, 0);
                } else {
                    builder.ins().call(
                        store_name,
                        &[ctx_param, name_ptr, name_len_val, class_value],
                    );
                }
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
                if let Some(Instruction::JumpIfFalse(target)) = code.get(idx + 1) {
                    let call = builder
                        .ins()
                        .call(less_than_truthy, &[ctx_param, left, right]);
                    let truthy_or_error = builder.inst_results(call)[0];
                    let non_error_block = builder.create_block();
                    let is_error = builder.ins().icmp_imm(IntCC::Equal, truthy_or_error, 2);
                    builder
                        .ins()
                        .brif(is_error, error_block, &[], non_error_block, &[]);
                    builder.switch_to_block(non_error_block);
                    let target_index = resolve_relative_target(idx + 1, *target, code.len())?;
                    builder.ins().brif(
                        truthy_or_error,
                        blocks[idx + 2],
                        &[],
                        blocks[target_index],
                        &[],
                    );
                    terminated = true;
                } else {
                    let call = builder.ins().call(less_than, &[ctx_param, left, right]);
                    let result = builder.inst_results(call)[0];
                    let ok_block = check_null(&mut builder, result);
                    builder.switch_to_block(ok_block);
                    push_value(&mut builder, result);
                }
            }
            Instruction::LoadIndex => {
                let index = pop_value(&mut builder);
                let object = pop_value(&mut builder);
                let call = builder.ins().call(load_index, &[ctx_param, object, index]);
                let result = builder.inst_results(call)[0];
                let ok_block = check_null(&mut builder, result);
                builder.switch_to_block(ok_block);
                push_value(&mut builder, result);
            }
            Instruction::StoreIndex(name) => {
                let value = pop_value(&mut builder);
                let index = pop_value(&mut builder);
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
                    let target_addr = builder.ins().iadd_imm(locals_base, local_idx * ptr_size);
                    let target_value =
                        builder
                            .ins()
                            .load(ptr_type, MemFlags::new(), target_addr, 0);
                    builder
                        .ins()
                        .call(store_index_value, &[ctx_param, target_value, index, value]);
                    builder.ins().jump(cont_block, &[]);

                    builder.switch_to_block(global_block);
                    let (name_data_id, name_len) = string_data.declare(module, "name", name)?;
                    let name_gv = module.declare_data_in_func(name_data_id, builder.func);
                    let name_ptr = builder.ins().global_value(ptr_type, name_gv);
                    let name_len_val = builder.ins().iconst(types::I64, name_len);
                    builder.ins().call(
                        store_index_name,
                        &[ctx_param, name_ptr, name_len_val, index, value],
                    );
                    builder.ins().jump(cont_block, &[]);

                    builder.switch_to_block(cont_block);
                } else {
                    let (name_data_id, name_len) = string_data.declare(module, "name", name)?;
                    let name_gv = module.declare_data_in_func(name_data_id, builder.func);
                    let name_ptr = builder.ins().global_value(ptr_type, name_gv);
                    let name_len_val = builder.ins().iconst(types::I64, name_len);
                    builder.ins().call(
                        store_index_name,
                        &[ctx_param, name_ptr, name_len_val, index, value],
                    );
                }
            }
            Instruction::LoadAttr(attribute) => {
                let object = pop_value(&mut builder);
                let (attribute_data_id, attribute_len) =
                    string_data.declare(module, "attr", attribute)?;
                let attribute_gv = module.declare_data_in_func(attribute_data_id, builder.func);
                let attribute_ptr = builder.ins().global_value(ptr_type, attribute_gv);
                let attribute_len_val = builder.ins().iconst(types::I64, attribute_len);
                let call = builder.ins().call(
                    load_attr,
                    &[ctx_param, object, attribute_ptr, attribute_len_val],
                );
                let result = builder.inst_results(call)[0];
                let ok_block = check_null(&mut builder, result);
                builder.switch_to_block(ok_block);
                push_value(&mut builder, result);
            }
            Instruction::StoreAttr(attribute) => {
                let object = pop_value(&mut builder);
                let value = pop_value(&mut builder);
                let (attribute_data_id, attribute_len) =
                    string_data.declare(module, "attr", attribute)?;
                let attribute_gv = module.declare_data_in_func(attribute_data_id, builder.func);
                let attribute_ptr = builder.ins().global_value(ptr_type, attribute_gv);
                let attribute_len_val = builder.ins().iconst(types::I64, attribute_len);
                builder.ins().call(
                    store_attr,
                    &[ctx_param, object, attribute_ptr, attribute_len_val, value],
                );
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
    if super::dump_clif_enabled() {
        eprintln!("; ---- CLIF {function_name} ----");
        eprintln!("{}", ctx.func.display());
    }
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
    if target < 0 || (target as usize) > code_len {
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
