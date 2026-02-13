use anyhow::{Result, bail};
use std::collections::HashMap;

use crate::ast::{BinaryOperator, Expression, Program, Statement};
use crate::backend::{Backend, PreparedBackend};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    None,
}

impl Value {
    fn as_int(&self) -> Result<i64> {
        match self {
            Value::Integer(value) => Ok(*value),
            Value::Boolean(_) | Value::String(_) | Value::None => {
                bail!("Expected integer, got {self:?}")
            }
        }
    }

    fn to_output(&self) -> String {
        match self {
            Value::Integer(value) => value.to_string(),
            Value::Boolean(value) => {
                if *value {
                    "True".to_string()
                } else {
                    "False".to_string()
                }
            }
            Value::String(value) => value.clone(),
            Value::None => "None".to_string(),
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            Value::Integer(value) => *value != 0,
            Value::Boolean(value) => *value,
            Value::String(value) => !value.is_empty(),
            Value::None => false,
        }
    }
}

#[derive(Debug, Clone)]
struct Function {
    body: Vec<Statement>,
}

pub struct Interpreter;

pub struct PreparedInterpreter {
    functions: HashMap<String, Function>,
    main_statements: Vec<Statement>,
}

struct InterpreterRuntime<'a> {
    functions: &'a HashMap<String, Function>,
    output: Vec<String>,
}

struct Environment<'a> {
    globals: &'a mut HashMap<String, Value>,
    locals: Option<&'a mut HashMap<String, Value>>,
}

enum ExecResult {
    Continue,
    Return(Value),
}

impl Interpreter {
    pub fn new() -> Self {
        Self
    }

    pub fn run(&self, program: &Program) -> Result<String> {
        self.prepare(program)?.run()
    }
}

impl PreparedInterpreter {
    fn run_once(&self) -> Result<String> {
        let mut globals = HashMap::new();
        let mut environment = Environment::top_level(&mut globals);
        let mut runtime = InterpreterRuntime {
            functions: &self.functions,
            output: Vec::new(),
        };
        match runtime.exec_block(&self.main_statements, &mut environment)? {
            ExecResult::Continue => {}
            ExecResult::Return(_) => bail!("Return outside of function"),
        }
        Ok(runtime.output.join("\n"))
    }
}

impl<'a> Environment<'a> {
    fn top_level(globals: &'a mut HashMap<String, Value>) -> Self {
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
        if let Some(locals) = self.locals.as_deref() {
            if let Some(value) = locals.get(name) {
                return Some(value.clone());
            }
        }
        self.globals.get(name).cloned()
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

impl<'a> InterpreterRuntime<'a> {
    fn exec_block(
        &mut self,
        body: &[Statement],
        environment: &mut Environment<'_>,
    ) -> Result<ExecResult> {
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
    ) -> Result<ExecResult> {
        match statement {
            Statement::FunctionDef { .. } => bail!("Nested function definitions are not supported"),
            Statement::Assign { name, value } => {
                let value = self.eval_expression(value, environment)?;
                environment.store(name.to_string(), value);
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
    ) -> Result<Value> {
        match expr {
            Expression::Integer(value) => Ok(Value::Integer(*value)),
            Expression::Boolean(value) => Ok(Value::Boolean(*value)),
            Expression::String(value) => Ok(Value::String(value.clone())),
            Expression::Identifier(name) => {
                if let Some(value) = environment.load(name) {
                    return Ok(value.clone());
                }
                bail!("Undefined variable '{name}'")
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
            Expression::Call { callee, args } => self.eval_call(callee, args, environment),
        }
    }

    fn eval_call(
        &mut self,
        callee: &Expression,
        args: &[Expression],
        environment: &mut Environment<'_>,
    ) -> Result<Value> {
        let mut evaluated_args = Vec::with_capacity(args.len());
        for arg in args {
            let value = self.eval_expression(arg, environment)?;
            evaluated_args.push(value);
        }

        match callee {
            Expression::Identifier(name) => match name.as_str() {
                "print" => {
                    let outputs: Vec<String> =
                        evaluated_args.iter().map(Value::to_output).collect();
                    self.output.push(outputs.join(" "));
                    Ok(Value::None)
                }
                _ => {
                    let function = self
                        .functions
                        .get(name)
                        .cloned()
                        .ok_or_else(|| anyhow::anyhow!("Undefined function '{name}'"))?;
                    if !evaluated_args.is_empty() {
                        bail!("Function '{name}' does not accept arguments");
                    }
                    let mut local_scope = HashMap::new();
                    let mut local_environment = environment.child_with_locals(&mut local_scope);
                    match self.exec_block(&function.body, &mut local_environment)? {
                        ExecResult::Continue => Ok(Value::None),
                        ExecResult::Return(value) => Ok(value),
                    }
                }
            },
            _ => bail!("Can only call identifiers"),
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl PreparedBackend for PreparedInterpreter {
    fn run(&self) -> Result<String> {
        self.run_once()
    }
}

impl Backend for Interpreter {
    fn name(&self) -> &'static str {
        "interpreter"
    }

    fn prepare(&self, program: &Program) -> Result<Box<dyn PreparedBackend>> {
        let mut functions = HashMap::new();
        let mut main_statements = Vec::new();

        for statement in &program.statements {
            match statement {
                Statement::FunctionDef { name, body } => {
                    functions.insert(name.clone(), Function { body: body.clone() });
                }
                _ => main_statements.push(statement.clone()),
            }
        }

        Ok(Box::new(PreparedInterpreter {
            functions,
            main_statements,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOperator, Expression, Program, Statement};

    fn identifier(name: &str) -> Expression {
        Expression::Identifier(name.to_string())
    }

    fn int(value: i64) -> Expression {
        Expression::Integer(value)
    }

    fn call(name: &str, args: Vec<Expression>) -> Expression {
        Expression::Call {
            callee: Box::new(identifier(name)),
            args,
        }
    }

    fn print(args: Vec<Expression>) -> Statement {
        Statement::Expr(call("print", args))
    }

    #[test]
    fn evaluates_assignment_and_call() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    name: "n".to_string(),
                    value: Expression::BinaryOp {
                        left: Box::new(int(1)),
                        op: BinaryOperator::Add,
                        right: Box::new(int(2)),
                    },
                },
                print(vec![identifier("n")]),
            ],
        };

        let interpreter = Interpreter::new();
        let output = interpreter.run(&program).expect("run failed");
        assert_eq!(output, "3");
    }

    #[test]
    fn executes_if_else_branches() {
        let program = Program {
            statements: vec![
                Statement::If {
                    condition: Expression::Boolean(true),
                    then_body: vec![print(vec![Expression::String("then".to_string())])],
                    else_body: vec![print(vec![Expression::String("else".to_string())])],
                },
                Statement::If {
                    condition: Expression::Boolean(false),
                    then_body: vec![print(vec![Expression::String("then".to_string())])],
                    else_body: vec![print(vec![Expression::String("else".to_string())])],
                },
            ],
        };

        let interpreter = Interpreter::new();
        let output = interpreter.run(&program).expect("run failed");
        assert_eq!(output, "then\nelse");
    }

    #[test]
    fn executes_while_loop_until_condition_is_false() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    name: "n".to_string(),
                    value: int(0),
                },
                Statement::While {
                    condition: Expression::BinaryOp {
                        left: Box::new(identifier("n")),
                        op: BinaryOperator::LessThan,
                        right: Box::new(int(3)),
                    },
                    body: vec![Statement::Assign {
                        name: "n".to_string(),
                        value: Expression::BinaryOp {
                            left: Box::new(identifier("n")),
                            op: BinaryOperator::Add,
                            right: Box::new(int(1)),
                        },
                    }],
                },
                print(vec![identifier("n")]),
            ],
        };

        let interpreter = Interpreter::new();
        let output = interpreter.run(&program).expect("run failed");
        assert_eq!(output, "3");
    }

    #[test]
    fn returns_from_function_without_executing_remaining_body() {
        let program = Program {
            statements: vec![
                Statement::FunctionDef {
                    name: "f".to_string(),
                    body: vec![
                        Statement::Return(Some(int(7))),
                        Statement::Expr(call(
                            "print",
                            vec![Expression::String("unreachable".to_string())],
                        )),
                    ],
                },
                print(vec![call("f", vec![])]),
            ],
        };

        let interpreter = Interpreter::new();
        let output = interpreter.run(&program).expect("run failed");
        assert_eq!(output, "7");
    }

    #[test]
    fn function_locals_do_not_leak_into_globals() {
        let program = Program {
            statements: vec![
                Statement::FunctionDef {
                    name: "f".to_string(),
                    body: vec![
                        Statement::Assign {
                            name: "x".to_string(),
                            value: int(42),
                        },
                        Statement::Return(None),
                    ],
                },
                Statement::Expr(call("f", vec![])),
                print(vec![identifier("x")]),
            ],
        };

        let interpreter = Interpreter::new();
        let error = interpreter
            .run(&program)
            .expect_err("expected undefined variable");
        assert!(error.to_string().contains("Undefined variable 'x'"));
    }

    #[test]
    fn errors_on_return_outside_function() {
        let program = Program {
            statements: vec![Statement::Return(Some(int(1)))],
        };

        let interpreter = Interpreter::new();
        let error = interpreter
            .run(&program)
            .expect_err("expected return outside function");
        assert!(error.to_string().contains("Return outside of function"));
    }

    #[test]
    fn errors_on_invalid_call_and_undefined_function() {
        let invalid_callee_program = Program {
            statements: vec![Statement::Expr(Expression::Call {
                callee: Box::new(int(1)),
                args: vec![],
            })],
        };

        let interpreter = Interpreter::new();
        let error = interpreter
            .run(&invalid_callee_program)
            .expect_err("expected call target error");
        assert!(error.to_string().contains("Can only call identifiers"));

        let undefined_function_program = Program {
            statements: vec![Statement::Expr(call("missing", vec![]))],
        };
        let error = interpreter
            .run(&undefined_function_program)
            .expect_err("expected undefined function error");
        assert!(error.to_string().contains("Undefined function 'missing'"));
    }

    #[test]
    fn errors_when_function_called_with_arguments() {
        let program = Program {
            statements: vec![
                Statement::FunctionDef {
                    name: "f".to_string(),
                    body: vec![Statement::Pass],
                },
                Statement::Expr(call("f", vec![int(1)])),
            ],
        };

        let interpreter = Interpreter::new();
        let error = interpreter
            .run(&program)
            .expect_err("expected argument mismatch");
        assert!(
            error
                .to_string()
                .contains("Function 'f' does not accept arguments")
        );
    }

    #[test]
    fn formats_print_output_for_boolean_string_and_none() {
        let program = Program {
            statements: vec![
                Statement::FunctionDef {
                    name: "f".to_string(),
                    body: vec![Statement::Pass],
                },
                print(vec![
                    Expression::Boolean(true),
                    Expression::String("hello".to_string()),
                    call("f", vec![]),
                ]),
            ],
        };

        let interpreter = Interpreter::new();
        let output = interpreter.run(&program).expect("run failed");
        assert_eq!(output, "True hello None");
    }

    #[test]
    fn clears_state_between_runs() {
        let first_program = Program {
            statements: vec![
                Statement::Assign {
                    name: "x".to_string(),
                    value: int(1),
                },
                print(vec![identifier("x")]),
            ],
        };
        let second_program = Program {
            statements: vec![print(vec![identifier("x")])],
        };

        let interpreter = Interpreter::new();
        let output = interpreter.run(&first_program).expect("first run failed");
        assert_eq!(output, "1");

        let error = interpreter
            .run(&second_program)
            .expect_err("expected globals to be cleared between runs");
        assert!(error.to_string().contains("Undefined variable 'x'"));
    }
}
