use std::collections::HashMap;

use crate::builtins::BuiltinFunction;
use crate::bytecode::{CompiledCallable, CompiledProgram, ExceptionHandlerKind, Instruction};
use crate::runtime::call_registry::CallRegistry;
use crate::runtime::error::RuntimeError;
use crate::runtime::exception::RaisedException;
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
                let parent_exception_handlers = std::mem::take(&mut runtime.exception_handlers);
                let parent_pending_unwind = runtime.pending_unwind.take();
                let result = runtime.execute_code(&code, &mut child_environment);
                runtime.stack = parent_stack;
                runtime.exception_handlers = parent_exception_handlers;
                runtime.pending_unwind = parent_pending_unwind;
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
    exception_handlers: Vec<ExceptionHandler>,
    pending_unwind: Option<UnwindReason>,
}

#[derive(Debug)]
enum UnwindReason {
    Return(Value),
    Exception(RaisedException),
}

#[derive(Clone, Copy, Debug)]
struct ExceptionHandler {
    target_ip: usize,
    kind: ExceptionHandlerKind,
    stack_depth: usize,
}

enum VmStepOutcome {
    Continue,
    Return(Value),
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
        for builtin in [
            BuiltinFunction::Print,
            BuiltinFunction::Len,
            BuiltinFunction::Range,
        ] {
            call_registry
                .register_with_id(builtin.callable_id(), RegisteredFunction::builtin(builtin));
        }
        Self {
            program,
            call_registry,
            output: Vec::new(),
            stack: Vec::new(),
            exception_handlers: Vec::new(),
            pending_unwind: None,
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

    fn call_context<'runtime, 'env>(
        &'runtime mut self,
        environment: &'runtime mut Environment<'env>,
    ) -> VmCallContext<'runtime, 'a, 'env> {
        VmCallContext {
            runtime: self,
            environment,
        }
    }

    fn pop_call_args(&mut self, argc: usize) -> VmResult<Vec<Value>> {
        let mut args = Vec::with_capacity(argc);
        for _ in 0..argc {
            args.push(self.pop_stack()?);
        }
        args.reverse();
        Ok(args)
    }

    fn execute_binary_op(
        &mut self,
        environment: &mut Environment<'_>,
        op: impl FnOnce(&Value, &mut dyn CallContext, Value) -> Result<Value, RuntimeError>,
    ) -> VmResult<()> {
        let right = self.pop_stack()?;
        let left = self.pop_stack()?;
        let result = {
            let mut context = self.call_context(environment);
            op(&left, &mut context, right)?
        };
        self.stack.push(result);
        Ok(())
    }

    fn build_dict_from_stack(
        &mut self,
        count: usize,
        environment: &mut Environment<'_>,
    ) -> VmResult<()> {
        let mut entries = Vec::with_capacity(count);
        for _ in 0..count {
            let value = self.pop_stack()?;
            let key = self.pop_stack()?;
            entries.push((key, value));
        }
        entries.reverse();
        let dict = {
            let mut context = self.call_context(environment);
            Value::dict_object_with_context(entries, &mut context)?
        };
        self.stack.push(dict);
        Ok(())
    }

    fn load_index(&mut self, environment: &mut Environment<'_>) -> VmResult<()> {
        let index_value = self.pop_stack()?;
        let object_value = self.pop_stack()?;
        let value = {
            let mut context = self.call_context(environment);
            object_value.get_item_with_context(&mut context, index_value)?
        };
        self.stack.push(value);
        Ok(())
    }

    fn store_index(&mut self, name: &str, environment: &mut Environment<'_>) -> VmResult<()> {
        let value = self.pop_stack()?;
        let index_value = self.pop_stack()?;
        let target =
            environment
                .load_cloned(name)
                .ok_or_else(|| RuntimeError::UndefinedVariable {
                    name: name.to_string(),
                })?;
        {
            let mut context = self.call_context(environment);
            target.set_item_with_context(&mut context, index_value, value)?;
        }
        Ok(())
    }

    fn get_iter(&mut self, environment: &mut Environment<'_>) -> VmResult<()> {
        let iterable = self.pop_stack()?;
        let iterator = {
            let mut context = self.call_context(environment);
            iterable.iter_with_context(&mut context)?
        };
        self.stack.push(iterator);
        Ok(())
    }

    fn step_for_iter(
        &mut self,
        target: isize,
        ip: &mut usize,
        code_len: usize,
        environment: &mut Environment<'_>,
    ) -> VmResult<()> {
        let iterator = self.pop_stack()?;
        let next_result = {
            let mut context = self.call_context(environment);
            iterator.next_with_context(&mut context)
        };
        match next_result {
            Ok(value) => {
                self.stack.push(iterator);
                self.stack.push(value);
            }
            Err(RuntimeError::Raised { exception }) if exception.is_stop_iteration() => {
                *ip = resolve_jump_target(*ip, target, code_len)?;
            }
            Err(error) => return Err(error.into()),
        }
        Ok(())
    }

    fn raise_from_stack(&mut self, environment: &mut Environment<'_>) -> VmResult<VmStepOutcome> {
        let exception = self.pop_stack()?;
        let mut context = self.call_context(environment);
        let error = match exception.to_raised_runtime_error(&mut context) {
            Ok(error) | Err(error) => error,
        };
        Err(error.into())
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
            let step_result = self.execute_instruction(&instruction, &mut ip, code, environment);
            match step_result {
                Ok(VmStepOutcome::Continue) => {}
                Ok(VmStepOutcome::Return(value)) => {
                    if let Some(reason) = self.start_unwind(UnwindReason::Return(value), &mut ip)? {
                        return self.finish_unwind(reason);
                    }
                }
                Err(VmError::Runtime(RuntimeError::Raised { exception })) => {
                    if let Some(reason) =
                        self.start_unwind(UnwindReason::Exception(exception), &mut ip)?
                    {
                        return self.finish_unwind(reason);
                    }
                }
                Err(error) => return Err(error),
            }
        }
    }

    fn execute_instruction(
        &mut self,
        instruction: &Instruction,
        ip: &mut usize,
        code: &[Instruction],
        environment: &mut Environment<'_>,
    ) -> VmResult<VmStepOutcome> {
        match instruction {
            Instruction::PushInt(value) => self.stack.push(Value::int_object(*value)),
            Instruction::PushBool(value) => self.stack.push(Value::bool_object(*value)),
            Instruction::PushString(value) => self.stack.push(Value::string_object(value.clone())),
            Instruction::BuildList(count) => {
                let mut values = Vec::with_capacity(*count);
                for _ in 0..*count {
                    let value = self.pop_stack()?;
                    values.push(value);
                }
                values.reverse();
                self.stack.push(Value::list_object(values));
            }
            Instruction::BuildDict(count) => {
                self.build_dict_from_stack(*count, environment)?;
            }
            Instruction::PushNone => self.stack.push(Value::none_object()),
            Instruction::LoadName(name) => {
                let value = if let Some(value) = environment.load_cloned(name) {
                    value
                } else {
                    return Err(RuntimeError::UndefinedVariable { name: name.clone() }.into());
                };
                self.stack.push(value);
            }
            Instruction::DefineFunction { name, callable_id } => {
                let (function_name, callable_id) = self.register_function(*callable_id)?;
                environment.store(
                    name.clone(),
                    Value::function_object(function_name, callable_id),
                );
            }
            Instruction::StoreName(name) => {
                let value = self.pop_stack()?;
                environment.store(name.clone(), value);
            }
            Instruction::DefineClass { name, methods } => {
                let mut class_methods = HashMap::with_capacity(methods.len());
                for (method_name, method_callable_id) in methods {
                    let (function_name, callable_id) =
                        self.register_function(*method_callable_id)?;
                    class_methods.insert(
                        method_name.clone(),
                        Value::function_object(function_name, callable_id),
                    );
                }
                let class_value = Value::class_object(name.clone(), class_methods);
                environment.store(name.clone(), class_value);
            }
            Instruction::LoadAttr(attribute) => {
                let object = self.pop_stack()?;
                let attribute_value = object.get_attribute(attribute)?;
                self.stack.push(attribute_value);
            }
            Instruction::StoreAttr(attribute) => {
                let object = self.pop_stack()?;
                let value = self.pop_stack()?;
                object.set_attribute(attribute, value)?;
            }
            Instruction::Add => {
                self.execute_binary_op(environment, Value::add)?;
            }
            Instruction::Sub => {
                self.execute_binary_op(environment, Value::sub)?;
            }
            Instruction::LessThan => {
                self.execute_binary_op(environment, Value::less_than)?;
            }
            Instruction::LoadIndex => {
                self.load_index(environment)?;
            }
            Instruction::StoreIndex(name) => {
                self.store_index(name, environment)?;
            }
            Instruction::GetIter => {
                self.get_iter(environment)?;
            }
            Instruction::ForIter(target) => {
                self.step_for_iter(*target, ip, code.len(), environment)?;
            }
            Instruction::PushExceptionHandler { target, kind } => {
                let target_ip = resolve_jump_target(*ip, *target, code.len())?;
                self.exception_handlers.push(ExceptionHandler {
                    target_ip,
                    kind: *kind,
                    stack_depth: self.stack.len(),
                });
            }
            Instruction::PopExceptionHandler => {
                let _ = self
                    .exception_handlers
                    .pop()
                    .expect("exception handler stack must not underflow");
            }
            Instruction::Call { argc } => {
                let args = self.pop_call_args(*argc)?;
                let callee = self.pop_stack()?;
                let value = self.call_value(callee, args, environment)?;
                self.stack.push(value);
            }
            Instruction::Raise => {
                return self.raise_from_stack(environment);
            }
            Instruction::ResumeUnwind => {
                let reason = self
                    .pending_unwind
                    .take()
                    .ok_or(RuntimeError::NoActiveUnwind)?;
                if let Some(reason) = self.start_unwind(reason, ip)? {
                    return Ok(match reason {
                        UnwindReason::Return(value) => VmStepOutcome::Return(value),
                        UnwindReason::Exception(exception) => {
                            return Err(RuntimeError::Raised { exception }.into());
                        }
                    });
                }
            }
            Instruction::JumpIfFalse(target) => {
                let value = self.pop_stack()?;
                if !value.is_truthy() {
                    *ip = resolve_jump_target(*ip, *target, code.len())?;
                }
            }
            Instruction::Jump(target) => {
                *ip = resolve_jump_target(*ip, *target, code.len())?;
            }
            Instruction::Pop => {
                self.pop_stack()?;
            }
            Instruction::Return => return Ok(VmStepOutcome::Return(Value::none_object())),
            Instruction::ReturnValue => {
                let value = self.pop_stack()?;
                return Ok(VmStepOutcome::Return(value));
            }
        }

        Ok(VmStepOutcome::Continue)
    }

    fn start_unwind(
        &mut self,
        reason: UnwindReason,
        ip: &mut usize,
    ) -> VmResult<Option<UnwindReason>> {
        let reason = reason;
        while let Some(handler) = self.exception_handlers.pop() {
            self.stack.truncate(handler.stack_depth);
            match handler.kind {
                ExceptionHandlerKind::Except => {
                    if matches!(reason, UnwindReason::Exception(_)) {
                        *ip = handler.target_ip;
                        return Ok(None);
                    }
                }
                ExceptionHandlerKind::Finally => {
                    self.pending_unwind = Some(reason);
                    *ip = handler.target_ip;
                    return Ok(None);
                }
            }
        }
        Ok(Some(reason))
    }

    fn finish_unwind(&mut self, reason: UnwindReason) -> VmResult<Value> {
        match reason {
            UnwindReason::Return(value) => Ok(value),
            UnwindReason::Exception(exception) => Err(RuntimeError::Raised { exception }.into()),
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

fn resolve_jump_target(ip: usize, offset: isize, code_len: usize) -> VmResult<usize> {
    let next_ip = (ip as isize) + offset;
    if next_ip < 0 || (next_ip as usize) > code_len {
        return Err(VmError::InvalidJumpTarget);
    }
    Ok(next_ip as usize)
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
