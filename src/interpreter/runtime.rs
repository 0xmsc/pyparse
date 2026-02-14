use std::collections::HashMap;

use crate::ast::{AssignTarget, BinaryOperator, Expression, Statement};
use crate::builtins::BuiltinFunction;

use super::{Function, InterpreterError, Value, value::ObjectKind};

/// Control-flow marker for statement execution.
pub(super) enum ExecResult {
    Continue,
    Return(Value),
}

/// Scoped variable environment with shared globals and optional function locals.
pub(super) struct Environment<'a> {
    globals: &'a mut HashMap<String, Value>,
    locals: Option<&'a mut HashMap<String, Value>>,
}

impl<'a> Environment<'a> {
    pub(super) fn top_level(globals: &'a mut HashMap<String, Value>) -> Self {
        Self {
            globals,
            locals: None,
        }
    }

    fn with_locals(
        globals: &'a mut HashMap<String, Value>,
        locals: &'a mut HashMap<String, Value>,
    ) -> Self {
        Self {
            globals,
            locals: Some(locals),
        }
    }

    fn load(&self, name: &str) -> Option<Value> {
        if let Some(locals) = self.locals.as_deref()
            && let Some(value) = locals.get(name)
        {
            return Some(value.clone());
        }
        self.globals.get(name).cloned()
    }

    fn load_mut(&mut self, name: &str) -> Option<&mut Value> {
        if let Some(locals) = self.locals.as_deref_mut()
            && locals.contains_key(name)
        {
            return locals.get_mut(name);
        }
        self.globals.get_mut(name)
    }

    fn store(&mut self, name: String, value: Value) {
        if let Some(locals) = self.locals.as_deref_mut() {
            locals.insert(name, value);
        } else {
            self.globals.insert(name, value);
        }
    }

    fn child_with_locals<'b>(
        &'b mut self,
        locals: &'b mut HashMap<String, Value>,
    ) -> Environment<'b> {
        Environment::with_locals(self.globals, locals)
    }
}

/// Runtime executor for interpreted statements and expressions.
pub(super) struct InterpreterRuntime<'a> {
    pub(super) functions: &'a HashMap<String, Function>,
    pub(super) output: Vec<String>,
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
            Statement::FunctionDef { .. } => {
                Err(InterpreterError::NestedFunctionDefinitionsUnsupported)
            }
            Statement::Assign { target, value } => {
                let value = self.eval_expression(value, environment)?;
                match target {
                    AssignTarget::Name(name) => {
                        environment.store(name.to_string(), value);
                    }
                    AssignTarget::Index { name, index } => {
                        let raw_index = self.eval_expression(index, environment)?.as_int()?;
                        if raw_index < 0 {
                            return Err(InterpreterError::NegativeListIndex { index: raw_index });
                        }
                        let index = raw_index as usize;
                        let list = environment.load_mut(name).ok_or_else(|| {
                            InterpreterError::UndefinedVariable {
                                name: name.to_string(),
                            }
                        })?;
                        match list {
                            Value::Object(object)
                                if matches!(object.borrow().kind, ObjectKind::List(_)) =>
                            {
                                let mut borrowed = object.borrow_mut();
                                let ObjectKind::List(values) = &mut borrowed.kind;
                                if index >= values.len() {
                                    return Err(InterpreterError::ListIndexOutOfBounds {
                                        index,
                                        len: values.len(),
                                    });
                                }
                                values[index] = value;
                            }
                            other => {
                                return Err(InterpreterError::ExpectedListType {
                                    got: format!("{other:?}"),
                                });
                            }
                        }
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
                    Value::None
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
            Expression::Integer(value) => Ok(Value::Integer(*value)),
            Expression::Boolean(value) => Ok(Value::Boolean(*value)),
            Expression::String(value) => Ok(Value::String(value.clone())),
            Expression::List(elements) => {
                let mut values = Vec::with_capacity(elements.len());
                for element in elements {
                    values.push(self.eval_expression(element, environment)?);
                }
                Ok(Value::list_object(values))
            }
            Expression::Identifier(name) => {
                if let Some(value) = environment.load(name) {
                    return Ok(value.clone());
                }
                Err(InterpreterError::UndefinedVariable {
                    name: name.to_string(),
                })
            }
            Expression::Index { object, index } => {
                let object = self.eval_expression(object, environment)?;
                let raw_index = self.eval_expression(index, environment)?.as_int()?;
                if raw_index < 0 {
                    return Err(InterpreterError::NegativeListIndex { index: raw_index });
                }
                let index = raw_index as usize;
                match object {
                    Value::Object(object)
                        if matches!(object.borrow().kind, ObjectKind::List(_)) =>
                    {
                        let borrowed = object.borrow();
                        let ObjectKind::List(values) = &borrowed.kind;
                        let value = values.get(index).cloned().ok_or(
                            InterpreterError::ListIndexOutOfBounds {
                                index,
                                len: values.len(),
                            },
                        )?;
                        Ok(value)
                    }
                    other => Err(InterpreterError::ExpectedListType {
                        got: format!("{other:?}"),
                    }),
                }
            }
            Expression::BinaryOp { left, op, right } => {
                let left = self.eval_expression(left, environment)?;
                let right = self.eval_expression(right, environment)?;
                match op {
                    BinaryOperator::Add => {
                        let left = left.as_int()?;
                        let right = right.as_int()?;
                        Ok(Value::Integer(left + right))
                    }
                    BinaryOperator::Sub => {
                        let left = left.as_int()?;
                        let right = right.as_int()?;
                        Ok(Value::Integer(left - right))
                    }
                    BinaryOperator::LessThan => {
                        let left = left.as_int()?;
                        let right = right.as_int()?;
                        Ok(Value::Boolean(left < right))
                    }
                }
            }
            Expression::Attribute { object, name } => {
                let object = self.eval_expression(object, environment)?;
                Self::load_attribute(object, name.to_string())
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
        let callee = self.resolve_callee(callee, environment)?;
        let mut evaluated_args = Vec::with_capacity(args.len());
        for arg in args {
            evaluated_args.push(self.eval_expression(arg, environment)?);
        }
        self.call_value(callee, evaluated_args, environment)
    }

    fn resolve_callee(
        &mut self,
        callee: &Expression,
        environment: &mut Environment<'_>,
    ) -> std::result::Result<Value, InterpreterError> {
        if let Expression::Identifier(name) = callee {
            if let Some(value) = environment.load(name) {
                return Ok(value);
            }
            if let Some(builtin) = BuiltinFunction::from_name(name) {
                return Ok(Value::BuiltinFunction(builtin));
            }
            if self.functions.contains_key(name) {
                return Ok(Value::Function(name.to_string()));
            }
            return Err(InterpreterError::UndefinedFunction {
                name: name.to_string(),
            });
        }
        self.eval_expression(callee, environment)
    }

    fn load_attribute(
        object: Value,
        attribute: String,
    ) -> std::result::Result<Value, InterpreterError> {
        if object.as_list_object().is_some() && attribute == "append" {
            return Ok(Value::BoundMethod {
                receiver: Box::new(object),
                method: attribute,
            });
        }
        Err(InterpreterError::UnknownAttribute {
            attribute,
            type_name: object.type_name().to_string(),
        })
    }

    fn call_value(
        &mut self,
        callee: Value,
        args: Vec<Value>,
        environment: &mut Environment<'_>,
    ) -> std::result::Result<Value, InterpreterError> {
        match callee {
            Value::BuiltinFunction(BuiltinFunction::Print) => {
                let outputs = args.iter().map(Value::to_output).collect::<Vec<_>>();
                self.output.push(outputs.join(" "));
                Ok(Value::None)
            }
            Value::BuiltinFunction(BuiltinFunction::Len) => {
                if args.len() != 1 {
                    return Err(InterpreterError::FunctionArityMismatch {
                        name: "len".to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }
                match &args[0] {
                    Value::Object(object)
                        if matches!(object.borrow().kind, ObjectKind::List(_)) =>
                    {
                        let borrowed = object.borrow();
                        let ObjectKind::List(values) = &borrowed.kind;
                        Ok(Value::Integer(values.len() as i64))
                    }
                    other => Err(InterpreterError::ExpectedListType {
                        got: format!("{other:?}"),
                    }),
                }
            }
            Value::Function(name) => {
                let function =
                    self.functions.get(&name).cloned().ok_or_else(|| {
                        InterpreterError::UndefinedFunction { name: name.clone() }
                    })?;
                if args.len() != function.params.len() {
                    return Err(InterpreterError::FunctionArityMismatch {
                        name,
                        expected: function.params.len(),
                        found: args.len(),
                    });
                }
                let mut local_scope = HashMap::new();
                for (param, value) in function.params.iter().zip(args) {
                    local_scope.insert(param.clone(), value);
                }
                let mut local_environment = environment.child_with_locals(&mut local_scope);
                match self.exec_block(&function.body, &mut local_environment)? {
                    ExecResult::Continue => Ok(Value::None),
                    ExecResult::Return(value) => Ok(value),
                }
            }
            Value::BoundMethod { receiver, method } => match *receiver {
                Value::Object(object) if matches!(object.borrow().kind, ObjectKind::List(_)) => {
                    match method.as_str() {
                        "append" => {
                            if args.len() != 1 {
                                return Err(InterpreterError::MethodArityMismatch {
                                    method: "append".to_string(),
                                    expected: 1,
                                    found: args.len(),
                                });
                            }
                            let mut borrowed = object.borrow_mut();
                            let ObjectKind::List(values) = &mut borrowed.kind;
                            values.push(args.into_iter().next().expect("len checked above"));
                            Ok(Value::None)
                        }
                        _ => Err(InterpreterError::UnknownMethod {
                            method,
                            type_name: "list".to_string(),
                        }),
                    }
                }
                other => Err(InterpreterError::UnknownMethod {
                    method,
                    type_name: other.type_name().to_string(),
                }),
            },
            other => Err(InterpreterError::ObjectNotCallable {
                type_name: other.type_name().to_string(),
            }),
        }
    }
}
