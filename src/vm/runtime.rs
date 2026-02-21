use std::collections::HashMap;

use crate::builtins::BuiltinFunction;
use crate::bytecode::{CompiledProgram, Instruction};
use crate::runtime::error::RuntimeError;
use crate::runtime::execution::{Environment, call_builtin_with_output};
use crate::runtime::object::{CallContext, CallableId};
use crate::runtime::value::Value;
use thiserror::Error;

pub(super) type VmResult<T> = std::result::Result<T, VmError>;

/// VM execution errors produced while running compiled bytecode.
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub(super) enum VmError {
    #[error("Stack underflow")]
    StackUnderflow,
    #[error(transparent)]
    Runtime(#[from] RuntimeError),
    #[error("Invalid jump target")]
    InvalidJumpTarget,
}

/// Executes a compiled program with VM semantics and returns printed output.
pub(super) fn run_compiled_program(
    program: &CompiledProgram,
    globals: &mut HashMap<String, Value>,
) -> VmResult<String> {
    let mut runtime = VmRuntime::new(program);
    let mut environment = Environment::top_level(globals);
    runtime.execute_code(&program.main, &mut environment)?;
    Ok(runtime.output_string())
}

/// Stateful bytecode executor shared across nested VM function calls.
struct VmRuntime<'a> {
    program: &'a CompiledProgram,
    output: Vec<String>,
    stack: Vec<Value>,
}

/// Call adapter passed into `Value` operations while running in the VM backend.
struct VmCallContext<'runtime, 'program, 'env> {
    runtime: &'runtime mut VmRuntime<'program>,
    environment: &'runtime mut Environment<'env>,
}

impl CallContext for VmCallContext<'_, '_, '_> {
    fn call_callable(
        &mut self,
        callable_id: &CallableId,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match callable_id {
            CallableId::Builtin(builtin) => {
                call_builtin_with_output(*builtin, args, &mut self.runtime.output)
            }
            CallableId::Function(name) => {
                let function = self
                    .runtime
                    .program
                    .functions
                    .get(name)
                    .ok_or_else(|| RuntimeError::UndefinedFunction { name: name.clone() })?
                    .clone();
                RuntimeError::expect_function_arity(name, function.params.len(), args.len())?;
                let mut locals_map = HashMap::new();
                for (param, value) in function.params.iter().zip(args) {
                    locals_map.insert(param.to_string(), value);
                }
                let mut child_environment = self.environment.child_with_locals(&mut locals_map);
                let parent_stack = std::mem::take(&mut self.runtime.stack);
                let result = self
                    .runtime
                    .execute_code(&function.code, &mut child_environment);
                self.runtime.stack = parent_stack;
                result.map_err(map_vm_error_to_runtime_error)
            }
        }
    }
}

impl<'a> VmRuntime<'a> {
    fn new(program: &'a CompiledProgram) -> Self {
        Self {
            program,
            output: Vec::new(),
            stack: Vec::new(),
        }
    }

    fn output_string(self) -> String {
        self.output.join("\n")
    }

    fn execute_code(
        &mut self,
        code: &[Instruction],
        environment: &mut Environment<'_>,
    ) -> VmResult<Value> {
        let mut ip = 0;
        loop {
            let instruction = match code.get(ip) {
                Some(instruction) => instruction.clone(),
                None => return Ok(Value::none_object()),
            };
            ip += 1;
            match instruction {
                Instruction::PushInt(value) => self.stack.push(Value::int_object(value)),
                Instruction::PushBool(value) => self.stack.push(Value::bool_object(value)),
                Instruction::PushString(value) => self.stack.push(Value::string_object(value)),
                Instruction::BuildList(count) => {
                    let mut values = Vec::with_capacity(count);
                    for _ in 0..count {
                        let value = self.pop_stack()?;
                        values.push(value);
                    }
                    values.reverse();
                    self.stack.push(Value::list_object(values));
                }
                Instruction::PushNone => self.stack.push(Value::none_object()),
                Instruction::LoadName(name) => {
                    let value = if let Some(value) = environment.load_cloned(&name) {
                        value
                    } else if let Some(builtin) = BuiltinFunction::from_name(&name) {
                        Value::builtin_function_object(builtin)
                    } else {
                        return Err(RuntimeError::UndefinedVariable { name: name.clone() }.into());
                    };
                    self.stack.push(value);
                }
                Instruction::DefineFunction { name, symbol } => {
                    if !self.program.functions.contains_key(&symbol) {
                        return Err(RuntimeError::UndefinedFunction { name: symbol }.into());
                    }
                    environment.store(name, Value::function_object(symbol));
                }
                Instruction::StoreName(name) => {
                    let value = self.pop_stack()?;
                    environment.store(name, value);
                }
                Instruction::DefineClass { name, methods } => {
                    let methods = methods
                        .into_iter()
                        .map(|(method_name, symbol)| (method_name, Value::function_object(symbol)))
                        .collect::<HashMap<_, _>>();
                    let class_value = Value::class_object(name.clone(), methods);
                    environment.store(name, class_value);
                }
                Instruction::LoadAttr(attribute) => {
                    let object = self.pop_stack()?;
                    let attribute_value = object.get_attribute(&attribute)?;
                    self.stack.push(attribute_value);
                }
                Instruction::StoreAttr(attribute) => {
                    let object = self.pop_stack()?;
                    let value = self.pop_stack()?;
                    object.set_attribute(&attribute, value)?;
                }
                Instruction::Add => {
                    let right = self.pop_stack()?;
                    let left = self.pop_stack()?;
                    let mut context = VmCallContext {
                        runtime: self,
                        environment,
                    };
                    let result = left.add(&mut context, right)?;
                    self.stack.push(result);
                }
                Instruction::Sub => {
                    let right = self.pop_stack()?;
                    let left = self.pop_stack()?;
                    let mut context = VmCallContext {
                        runtime: self,
                        environment,
                    };
                    let result = left.sub(&mut context, right)?;
                    self.stack.push(result);
                }
                Instruction::LessThan => {
                    let right = self.pop_stack()?;
                    let left = self.pop_stack()?;
                    let mut context = VmCallContext {
                        runtime: self,
                        environment,
                    };
                    let result = left.less_than(&mut context, right)?;
                    self.stack.push(result);
                }
                Instruction::LoadIndex => {
                    let index_value = self.pop_stack()?;
                    let object_value = self.pop_stack()?;
                    let mut context = VmCallContext {
                        runtime: self,
                        environment,
                    };
                    let value = object_value.get_item_with_context(&mut context, index_value)?;
                    self.stack.push(value);
                }
                Instruction::StoreIndex(name) => {
                    let value = self.pop_stack()?;
                    let index_value = self.pop_stack()?;
                    let target = environment
                        .load_cloned(&name)
                        .ok_or_else(|| RuntimeError::UndefinedVariable { name: name.clone() })?;
                    let mut context = VmCallContext {
                        runtime: self,
                        environment,
                    };
                    target.set_item_with_context(&mut context, index_value, value)?;
                }
                Instruction::Call { argc } => {
                    let mut args = Vec::with_capacity(argc);
                    for _ in 0..argc {
                        let value = self.pop_stack()?;
                        args.push(value);
                    }
                    args.reverse();
                    let callee = self.pop_stack()?;
                    let value = self.call_value(callee, args, environment)?;
                    self.stack.push(value);
                }
                Instruction::JumpIfFalse(target) => {
                    let value = self.pop_stack()?;
                    if !value.is_truthy() {
                        let next_ip = (ip as isize) + target;
                        if next_ip < 0 || (next_ip as usize) > code.len() {
                            return Err(VmError::InvalidJumpTarget);
                        }
                        ip = next_ip as usize;
                    }
                }
                Instruction::Jump(target) => {
                    let next_ip = (ip as isize) + target;
                    if next_ip < 0 || (next_ip as usize) > code.len() {
                        return Err(VmError::InvalidJumpTarget);
                    }
                    ip = next_ip as usize;
                }
                Instruction::Pop => {
                    self.pop_stack()?;
                }
                Instruction::Return => return Ok(Value::none_object()),
                Instruction::ReturnValue => {
                    let value = self.pop_stack()?;
                    return Ok(value);
                }
            }
        }
    }

    fn pop_stack(&mut self) -> VmResult<Value> {
        self.stack.pop().ok_or(VmError::StackUnderflow)
    }

    fn call_value(
        &mut self,
        callee: Value,
        args: Vec<Value>,
        environment: &mut Environment<'_>,
    ) -> VmResult<Value> {
        let mut context = VmCallContext {
            runtime: self,
            environment,
        };
        callee.call(&mut context, args).map_err(Into::into)
    }
}

fn map_vm_error_to_runtime_error(error: VmError) -> RuntimeError {
    match error {
        VmError::StackUnderflow => panic!("stack underflow while calling function"),
        VmError::Runtime(error) => error,
        VmError::InvalidJumpTarget => panic!("invalid jump target while calling function"),
    }
}
