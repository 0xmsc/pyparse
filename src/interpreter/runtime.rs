use std::collections::HashMap;

use crate::ast::{AssignTarget, BinaryOperator, Expression, Statement};
use crate::builtins::BuiltinFunction;
use crate::runtime::error::RuntimeError;
use crate::runtime::execution::{Environment, call_builtin_with_output};
use crate::runtime::object::{CallContext, CallableId};

use super::{Function, InterpreterError, Value};

/// Control-flow marker for statement execution.
pub(super) enum ExecResult {
    Continue,
    Return(Value),
}

/// Runtime executor for interpreted statements and expressions.
pub(super) struct InterpreterRuntime<'a> {
    pub(super) functions: &'a HashMap<String, Function>,
    pub(super) function_ids_by_name: HashMap<String, CallableId>,
    pub(super) function_names_by_id: HashMap<u32, String>,
    pub(super) output: Vec<String>,
}

/// Call adapter passed into `Value` operations while running in interpreter mode.
struct InterpreterCallContext<'runtime, 'functions, 'env> {
    runtime: &'runtime mut InterpreterRuntime<'functions>,
    environment: &'runtime mut Environment<'env>,
}

impl CallContext for InterpreterCallContext<'_, '_, '_> {
    fn call_callable(
        &mut self,
        callable_id: &CallableId,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let callable_id = callable_id.as_u32();
        if let Some(builtin) = BuiltinFunction::from_callable_id(callable_id) {
            return call_builtin_with_output(builtin, args, &mut self.runtime.output);
        }

        let function_name = self
            .runtime
            .function_names_by_id
            .get(&callable_id)
            .ok_or_else(|| RuntimeError::UndefinedFunction {
                name: format!("<callable:{callable_id}>"),
            })?;
        let function = self
            .runtime
            .functions
            .get(function_name)
            .cloned()
            .ok_or_else(|| RuntimeError::UndefinedFunction {
                name: function_name.clone(),
            })?;
        RuntimeError::expect_function_arity(
            function_name.as_str(),
            function.params.len(),
            args.len(),
        )?;
        let mut local_scope = HashMap::new();
        for (param, value) in function.params.iter().zip(args) {
            local_scope.insert(param.clone(), value);
        }
        let mut local_environment = self.environment.child_with_locals(&mut local_scope);
        match self
            .runtime
            .exec_block(&function.body, &mut local_environment)
            .map_err(runtime_error_from_interpreter)?
        {
            ExecResult::Continue => Ok(Value::none_object()),
            ExecResult::Return(value) => Ok(value),
        }
    }
}

impl<'a> InterpreterRuntime<'a> {
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
            Statement::FunctionDef { name, .. } => {
                if !environment.is_top_level() {
                    return Err(RuntimeError::NestedFunctionDefinitionsUnsupported.into());
                }
                let callable_id = *self
                    .function_ids_by_name
                    .get(name)
                    .ok_or_else(|| RuntimeError::UndefinedFunction { name: name.clone() })?;
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
                            name: method_name, ..
                        } => {
                            let method_symbol = class_method_symbol(name, method_name);
                            let callable_id = *self
                                .function_ids_by_name
                                .get(method_symbol.as_str())
                                .ok_or_else(|| RuntimeError::UndefinedFunction {
                                    name: method_symbol.clone(),
                                })?;
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
                if let Some(builtin) = BuiltinFunction::from_name(name) {
                    return Ok(Value::builtin_function_object(builtin));
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
