use std::collections::HashMap;

use crate::builtins::BuiltinFunction;
use crate::bytecode::{CompiledCallable, CompiledProgram, Instruction};
use crate::runtime::call_registry::CallRegistry;
use crate::runtime::error::RuntimeError;
use crate::runtime::execution::{Environment, call_builtin_with_output, seed_builtin_globals};
use crate::runtime::object::{CallContext, CallableId};
use crate::runtime::value::Value;
use thiserror::Error;

pub(super) type VmResult<T> = std::result::Result<T, VmError>;

#[derive(Clone)]
struct RegisteredFunction {
    name: String,
    implementation: RegisteredFunctionImplementation,
}

#[derive(Clone)]
enum RegisteredFunctionImplementation {
    Builtin(BuiltinFunction),
    User {
        params: Vec<String>,
        code: Vec<Instruction>,
    },
}

impl RegisteredFunction {
    fn builtin(builtin: BuiltinFunction) -> Self {
        Self {
            name: builtin.name().to_string(),
            implementation: RegisteredFunctionImplementation::Builtin(builtin),
        }
    }

    fn user(name: String, params: Vec<String>, code: Vec<Instruction>) -> Self {
        Self {
            name,
            implementation: RegisteredFunctionImplementation::User { params, code },
        }
    }

    fn invoke(
        self,
        runtime: &mut VmRuntime<'_>,
        environment: &mut Environment<'_>,
        args: Vec<Value>,
    ) -> VmResult<Value> {
        match self.implementation {
            RegisteredFunctionImplementation::Builtin(builtin) => {
                call_builtin_with_output(builtin, args, &mut runtime.output).map_err(Into::into)
            }
            RegisteredFunctionImplementation::User { params, code } => {
                RuntimeError::expect_function_arity(self.name.as_str(), params.len(), args.len())?;
                let mut locals_map = HashMap::new();
                for (param, value) in params.into_iter().zip(args) {
                    locals_map.insert(param, value);
                }
                let mut child_environment = environment.child_with_locals(&mut locals_map);
                let parent_stack = std::mem::take(&mut runtime.stack);
                let result = runtime.execute_code(&code, &mut child_environment);
                runtime.stack = parent_stack;
                result
            }
        }
    }
}

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
    seed_builtin_globals(globals);
    let mut runtime = VmRuntime::new(program);
    let mut environment = Environment::top_level(globals);
    runtime.execute_code(&program.main, &mut environment)?;
    Ok(runtime.output_string())
}

/// Stateful bytecode executor shared across nested VM function calls.
///
/// One operand stack is reused while running a code object; function calls save
/// and restore that stack around child execution.
struct VmRuntime<'a> {
    program: &'a CompiledProgram,
    call_registry: CallRegistry<RegisteredFunction>,
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
        let callable = self.runtime.call_registry.resolve(callable_id)?;
        callable
            .invoke(self.runtime, self.environment, args)
            .map_err(map_vm_error_to_runtime_error)
    }
}

impl<'a> VmRuntime<'a> {
    /// Initializes a VM runtime with builtin callable registrations.
    fn new(program: &'a CompiledProgram) -> Self {
        let mut call_registry = CallRegistry::new();
        for builtin in [BuiltinFunction::Print, BuiltinFunction::Len] {
            call_registry
                .register_with_id(builtin.callable_id(), RegisteredFunction::builtin(builtin));
        }
        Self {
            program,
            call_registry,
            output: Vec::new(),
            stack: Vec::new(),
        }
    }

    /// Maps a compiled callable ID to a freshly registered runtime callable.
    fn register_function(
        &mut self,
        compiled_callable_id: u32,
    ) -> Result<(String, CallableId), RuntimeError> {
        let callable = self
            .program
            .callables
            .get(compiled_callable_id as usize)
            .ok_or_else(|| RuntimeError::UndefinedFunction {
                name: format!("<callable:{compiled_callable_id}>"),
            })?;
        let function = registered_function_from_compiled(callable);
        let callable_id = self.call_registry.register_function(function.clone());
        let function_name = function.name.clone();
        Ok((function_name, callable_id))
    }

    fn output_string(self) -> String {
        self.output.join("\n")
    }

    /// Executes stack bytecode and returns the function result value.
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
                Instruction::BuildDict(count) => {
                    let mut entries = Vec::with_capacity(count);
                    for _ in 0..count {
                        let value = self.pop_stack()?;
                        let key = self.pop_stack()?;
                        entries.push((key, value));
                    }
                    entries.reverse();
                    let dict = Value::dict_object(entries)?;
                    self.stack.push(dict);
                }
                Instruction::PushNone => self.stack.push(Value::none_object()),
                Instruction::LoadName(name) => {
                    let value = if let Some(value) = environment.load_cloned(&name) {
                        value
                    } else {
                        return Err(RuntimeError::UndefinedVariable { name: name.clone() }.into());
                    };
                    self.stack.push(value);
                }
                Instruction::DefineFunction { name, callable_id } => {
                    let (function_name, callable_id) = self.register_function(callable_id)?;
                    environment.store(name, Value::function_object(function_name, callable_id));
                }
                Instruction::StoreName(name) => {
                    let value = self.pop_stack()?;
                    environment.store(name, value);
                }
                Instruction::DefineClass { name, methods } => {
                    let mut class_methods = HashMap::with_capacity(methods.len());
                    for (method_name, method_callable_id) in methods {
                        let (function_name, callable_id) =
                            self.register_function(method_callable_id)?;
                        class_methods.insert(
                            method_name,
                            Value::function_object(function_name, callable_id),
                        );
                    }
                    let class_value = Value::class_object(name.clone(), class_methods);
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

    /// Invokes a runtime `Value` as callable using VM call-context dispatch.
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

/// Converts VM-internal errors surfaced from nested calls into runtime errors.
fn map_vm_error_to_runtime_error(error: VmError) -> RuntimeError {
    match error {
        VmError::StackUnderflow => panic!("stack underflow while calling function"),
        VmError::Runtime(error) => error,
        VmError::InvalidJumpTarget => panic!("invalid jump target while calling function"),
    }
}

/// Copies compiled callable metadata into runtime-owned dispatch metadata.
fn registered_function_from_compiled(callable: &CompiledCallable) -> RegisteredFunction {
    RegisteredFunction::user(
        callable.name.clone(),
        callable.function.params.clone(),
        callable.function.code.clone(),
    )
}
