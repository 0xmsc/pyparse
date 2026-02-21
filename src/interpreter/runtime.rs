use std::collections::HashMap;

use crate::ast::{AssignTarget, BinaryOperator, Expression, Statement};
use crate::builtins::BuiltinFunction;
use crate::runtime::error::RuntimeError;
use crate::runtime::execution::{Environment, call_builtin_with_output};
use crate::runtime::object::{CallContext, CallableId};

use super::{InterpreterError, value::Value};

#[derive(Clone)]
struct RegisteredFunction {
    name: String,
    params: Vec<String>,
    body: Vec<Statement>,
}

#[derive(Clone)]
enum RegisteredCallable {
    Builtin(BuiltinFunction),
    Function(RegisteredFunction),
}

/// Control-flow marker for statement execution.
pub(super) enum ExecResult {
    Continue,
    Return(Value),
}

/// Runtime executor for interpreted statements and expressions.
pub(super) struct InterpreterRuntime {
    next_callable_id: u32,
    callables_by_id: HashMap<u32, RegisteredCallable>,
    pub(super) output: Vec<String>,
}

/// Call adapter passed into `Value` operations while running in interpreter mode.
struct InterpreterCallContext<'runtime, 'env> {
    runtime: &'runtime mut InterpreterRuntime,
    environment: &'runtime mut Environment<'env>,
}

impl CallContext for InterpreterCallContext<'_, '_> {
    fn call_callable(
        &mut self,
        callable_id: &CallableId,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let callable_id = callable_id.0;
        let callable = self
            .runtime
            .callables_by_id
            .get(&callable_id)
            .ok_or_else(|| RuntimeError::UndefinedFunction {
                name: format!("<callable:{callable_id}>"),
            })?
            .clone();
        let callable_function = match callable {
            RegisteredCallable::Builtin(builtin) => {
                return call_builtin_with_output(builtin, args, &mut self.runtime.output);
            }
            RegisteredCallable::Function(callable_function) => callable_function,
        };
        RuntimeError::expect_function_arity(
            callable_function.name.as_str(),
            callable_function.params.len(),
            args.len(),
        )?;
        let mut local_scope = HashMap::new();
        for (param, value) in callable_function.params.iter().zip(args) {
            local_scope.insert(param.clone(), value);
        }
        let mut local_environment = self.environment.child_with_locals(&mut local_scope);
        match self
            .runtime
            .exec_block(&callable_function.body, &mut local_environment)
            .map_err(runtime_error_from_interpreter)?
        {
            ExecResult::Continue => Ok(Value::none_object()),
            ExecResult::Return(value) => Ok(value),
        }
    }
}

impl InterpreterRuntime {
    pub(super) fn new() -> Self {
        let mut callables_by_id = HashMap::new();
        for builtin in [BuiltinFunction::Print, BuiltinFunction::Len] {
            callables_by_id.insert(builtin.callable_id(), RegisteredCallable::Builtin(builtin));
        }
        let next_callable_id = callables_by_id
            .keys()
            .copied()
            .max()
            .unwrap_or(0)
            .checked_add(1)
            .expect("callable id overflow");
        Self {
            next_callable_id,
            callables_by_id,
            output: Vec::new(),
        }
    }

    fn register_function(
        &mut self,
        name: String,
        params: Vec<String>,
        body: Vec<Statement>,
    ) -> CallableId {
        let callable_id = CallableId(self.next_callable_id);
        self.next_callable_id = self
            .next_callable_id
            .checked_add(1)
            .expect("callable id overflow");
        self.callables_by_id.insert(
            callable_id.0,
            RegisteredCallable::Function(RegisteredFunction { name, params, body }),
        );
        callable_id
    }

    pub(super) fn exec_block(
        &mut self,
        body: &[Statement],
        environment: &mut Environment<'_>,
    ) -> std::result::Result<ExecResult, InterpreterError> {
        // Execute statements in order until one returns, then bubble that up.
        for statement in body {
            match self.exec_statement(statement, environment)? {
                ExecResult::Continue => {}
                ExecResult::Return(value) => return Ok(ExecResult::Return(value)),
            }
        }
        Ok(ExecResult::Continue)
    }

    fn exec_statement(
        &mut self,
        statement: &Statement,
        environment: &mut Environment<'_>,
    ) -> std::result::Result<ExecResult, InterpreterError> {
        match statement {
            Statement::FunctionDef { name, params, body } => {
                if !environment.is_top_level() {
                    return Err(RuntimeError::NestedFunctionDefinitionsUnsupported.into());
                }
                let callable_id =
                    self.register_function(name.to_string(), params.clone(), body.clone());
                environment.store(
                    name.to_string(),
                    Value::function_object(name.to_string(), callable_id),
                );
                Ok(ExecResult::Continue)
            }
            Statement::ClassDef { name, body } => {
                let mut methods = HashMap::new();
                for class_statement in body {
                    match class_statement {
                        Statement::FunctionDef {
                            name: method_name,
                            params,
                            body,
                        } => {
                            let method_symbol = class_method_symbol(name, method_name);
                            let callable_id = self.register_function(
                                method_symbol.clone(),
                                params.clone(),
                                body.clone(),
                            );
                            methods.insert(
                                method_name.clone(),
                                Value::function_object(method_symbol, callable_id),
                            );
                        }
                        Statement::Pass => {}
                        _ => {
                            return Err(RuntimeError::UnsupportedOperation {
                                operation: "class body statement".to_string(),
                                type_name: "class".to_string(),
                            }
                            .into());
                        }
                    }
                }
                environment.store(name.to_string(), Value::class_object(name.clone(), methods));
                Ok(ExecResult::Continue)
            }
            Statement::Assign { target, value } => {
                let value = self.eval_expression(value, environment)?;
                match target {
                    AssignTarget::Name(name) => {
                        environment.store(name.to_string(), value);
                    }
                    AssignTarget::Index { name, index } => {
                        let index_value = self.eval_expression(index, environment)?;
                        let target = environment.load_cloned(name).ok_or_else(|| {
                            RuntimeError::UndefinedVariable {
                                name: name.to_string(),
                            }
                        })?;
                        let mut context = InterpreterCallContext {
                            runtime: self,
                            environment,
                        };
                        target
                            .set_item_with_context(&mut context, index_value, value)
                            .map_err(InterpreterError::from)?;
                    }
                    AssignTarget::Attribute { object, name } => {
                        let object = self.eval_expression(object, environment)?;
                        object.set_attribute(name, value)?;
                    }
                }
                Ok(ExecResult::Continue)
            }
            Statement::If {
                condition,
                then_body,
                else_body,
            } => {
                let condition = self.eval_expression(condition, environment)?;
                let body = if condition.is_truthy() {
                    then_body
                } else {
                    else_body
                };
                self.exec_block(body, environment)
            }
            Statement::While { condition, body } => {
                loop {
                    let condition = self.eval_expression(condition, environment)?;
                    if !condition.is_truthy() {
                        break;
                    }
                    if let ExecResult::Return(value) = self.exec_block(body, environment)? {
                        return Ok(ExecResult::Return(value));
                    }
                }
                Ok(ExecResult::Continue)
            }
            Statement::Return(value) => {
                let value = if let Some(value) = value {
                    self.eval_expression(value, environment)?
                } else {
                    Value::none_object()
                };
                Ok(ExecResult::Return(value))
            }
            Statement::Pass => Ok(ExecResult::Continue),
            Statement::Expr(expr) => {
                self.eval_expression(expr, environment)?;
                Ok(ExecResult::Continue)
            }
        }
    }

    fn eval_expression(
        &mut self,
        expr: &Expression,
        environment: &mut Environment<'_>,
    ) -> std::result::Result<Value, InterpreterError> {
        // Expression evaluation can recurse into calls, which may execute statements.
        match expr {
            Expression::Integer(value) => Ok(Value::int_object(*value)),
            Expression::Boolean(value) => Ok(Value::bool_object(*value)),
            Expression::String(value) => Ok(Value::string_object(value.clone())),
            Expression::List(elements) => {
                let mut values = Vec::with_capacity(elements.len());
                for element in elements {
                    values.push(self.eval_expression(element, environment)?);
                }
                Ok(Value::list_object(values))
            }
            Expression::Identifier(name) => {
                if let Some(value) = environment.load_cloned(name) {
                    return Ok(value);
                }
                Err(RuntimeError::UndefinedVariable {
                    name: name.to_string(),
                }
                .into())
            }
            Expression::Index { object, index } => {
                let object_value = self.eval_expression(object, environment)?;
                let index_value = self.eval_expression(index, environment)?;
                let mut context = InterpreterCallContext {
                    runtime: self,
                    environment,
                };
                object_value
                    .get_item_with_context(&mut context, index_value)
                    .map_err(Into::into)
            }
            Expression::BinaryOp { left, op, right } => {
                let left = self.eval_expression(left, environment)?;
                let right = self.eval_expression(right, environment)?;
                let mut context = InterpreterCallContext {
                    runtime: self,
                    environment,
                };
                match op {
                    BinaryOperator::Add => left.add(&mut context, right),
                    BinaryOperator::Sub => left.sub(&mut context, right),
                    BinaryOperator::LessThan => left.less_than(&mut context, right),
                }
                .map_err(Into::into)
            }
            Expression::Attribute { object, name } => {
                let object = self.eval_expression(object, environment)?;
                let attribute = name.to_string();
                object.get_attribute(&attribute).map_err(Into::into)
            }
            Expression::Call { callee, args } => self.eval_call(callee, args, environment),
        }
    }

    fn eval_call(
        &mut self,
        callee: &Expression,
        args: &[Expression],
        environment: &mut Environment<'_>,
    ) -> std::result::Result<Value, InterpreterError> {
        let callee = self.eval_expression(callee, environment)?;
        let mut evaluated_args = Vec::with_capacity(args.len());
        for arg in args {
            evaluated_args.push(self.eval_expression(arg, environment)?);
        }
        self.call_value(callee, evaluated_args, environment)
    }

    fn call_value(
        &mut self,
        callee: Value,
        args: Vec<Value>,
        environment: &mut Environment<'_>,
    ) -> std::result::Result<Value, InterpreterError> {
        let mut context = InterpreterCallContext {
            runtime: self,
            environment,
        };
        callee.call(&mut context, args).map_err(Into::into)
    }
}

fn class_method_symbol(class_name: &str, method_name: &str) -> String {
    format!("__class_method::{class_name}::{method_name}")
}

fn runtime_error_from_interpreter(error: InterpreterError) -> RuntimeError {
    match error {
        InterpreterError::Runtime(error) => error,
    }
}
