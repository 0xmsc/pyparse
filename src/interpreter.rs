use anyhow::{Result, bail};
use std::collections::HashMap;

use crate::ast::{Program, Statement};
use crate::backend::{Backend, PreparedBackend};
use crate::runtime::error::RuntimeError;

mod error;
mod runtime;
mod value;

use error::InterpreterError;
use runtime::{Environment, ExecResult, InterpreterRuntime};
use value::Value;

#[derive(Debug, Clone)]
struct Function {
    params: Vec<String>,
    body: Vec<Statement>,
}

/// AST-walking backend that executes programs directly without compilation.
pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Self
    }
}

/// Prepared executable program for the tree-walking interpreter.
pub struct PreparedInterpreter {
    functions: HashMap<String, Function>,
    main_statements: Vec<Statement>,
}

impl PreparedInterpreter {
    fn run_once(&self) -> std::result::Result<String, InterpreterError> {
        // Execution pipeline:
        // run_once -> exec_block (top-level statements) -> exec_statement
        // -> eval_expression -> eval_call -> exec_block (function body).
        let mut globals = HashMap::new();
        let mut environment = Environment::top_level(&mut globals);
        let mut runtime = InterpreterRuntime {
            functions: &self.functions,
            output: Vec::new(),
        };
        match runtime.exec_block(&self.main_statements, &mut environment)? {
            ExecResult::Continue => {}
            ExecResult::Return(_) => return Err(RuntimeError::ReturnOutsideFunction.into()),
        }
        Ok(runtime.output.join("\n"))
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl PreparedBackend for PreparedInterpreter {
    fn run(&self) -> Result<String> {
        Ok(self.run_once()?)
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
                Statement::FunctionDef { name, params, body } => {
                    if functions.contains_key(name) {
                        bail!("Duplicate function definition '{name}'");
                    }
                    functions.insert(
                        name.clone(),
                        Function {
                            params: params.clone(),
                            body: body.clone(),
                        },
                    );
                    main_statements.push(statement.clone());
                }
                Statement::ClassDef { name, body } => {
                    for class_statement in body {
                        match class_statement {
                            Statement::FunctionDef {
                                name: method_name,
                                params,
                                body,
                            } => {
                                let symbol = class_method_symbol(name, method_name);
                                functions.insert(
                                    symbol,
                                    Function {
                                        params: params.clone(),
                                        body: body.clone(),
                                    },
                                );
                            }
                            Statement::Pass => {}
                            _ => {
                                bail!(
                                    "Unsupported class body statement in class '{name}': only method definitions and pass are allowed"
                                );
                            }
                        }
                    }
                    main_statements.push(statement.clone());
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

fn class_method_symbol(class_name: &str, method_name: &str) -> String {
    format!("__class_method::{class_name}::{method_name}")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{AssignTarget, BinaryOperator, Expression, Program, Statement};

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

    fn method_call(receiver: &str, method: &str, args: Vec<Expression>) -> Expression {
        Expression::Call {
            callee: Box::new(Expression::Attribute {
                object: Box::new(identifier(receiver)),
                name: method.to_string(),
            }),
            args,
        }
    }

    fn print(args: Vec<Expression>) -> Statement {
        Statement::Expr(call("print", args))
    }

    fn run_program(interpreter: &Interpreter, program: &Program) -> anyhow::Result<String> {
        interpreter.prepare(program)?.run()
    }

    fn expect_interpreter_error(error: anyhow::Error) -> InterpreterError {
        error
            .downcast::<InterpreterError>()
            .expect("expected InterpreterError")
    }

    #[test]
    fn evaluates_assignment_and_call() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("n".to_string()),
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
        let output = run_program(&interpreter, &program).expect("run failed");
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
        let output = run_program(&interpreter, &program).expect("run failed");
        assert_eq!(output, "then\nelse");
    }

    #[test]
    fn executes_while_loop_until_condition_is_false() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("n".to_string()),
                    value: int(0),
                },
                Statement::While {
                    condition: Expression::BinaryOp {
                        left: Box::new(identifier("n")),
                        op: BinaryOperator::LessThan,
                        right: Box::new(int(3)),
                    },
                    body: vec![Statement::Assign {
                        target: AssignTarget::Name("n".to_string()),
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
        let output = run_program(&interpreter, &program).expect("run failed");
        assert_eq!(output, "3");
    }

    #[test]
    fn returns_from_function_without_executing_remaining_body() {
        let program = Program {
            statements: vec![
                Statement::FunctionDef {
                    name: "f".to_string(),
                    params: vec![],
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
        let output = run_program(&interpreter, &program).expect("run failed");
        assert_eq!(output, "7");
    }

    #[test]
    fn function_locals_do_not_leak_into_globals() {
        let program = Program {
            statements: vec![
                Statement::FunctionDef {
                    name: "f".to_string(),
                    params: vec![],
                    body: vec![
                        Statement::Assign {
                            target: AssignTarget::Name("x".to_string()),
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
        let error = expect_interpreter_error(
            run_program(&interpreter, &program).expect_err("expected undefined variable"),
        );
        assert_eq!(
            error,
            InterpreterError::Runtime(RuntimeError::UndefinedVariable {
                name: "x".to_string()
            })
        );
    }

    #[test]
    fn errors_on_return_outside_function() {
        let program = Program {
            statements: vec![Statement::Return(Some(int(1)))],
        };

        let interpreter = Interpreter::new();
        let error = expect_interpreter_error(
            run_program(&interpreter, &program).expect_err("expected return outside function"),
        );
        assert_eq!(
            error,
            InterpreterError::Runtime(RuntimeError::ReturnOutsideFunction)
        );
    }

    #[test]
    fn errors_on_invalid_call_and_undefined_name() {
        let invalid_callee_program = Program {
            statements: vec![Statement::Expr(Expression::Call {
                callee: Box::new(int(1)),
                args: vec![],
            })],
        };

        let interpreter = Interpreter::new();
        let error = expect_interpreter_error(
            run_program(&interpreter, &invalid_callee_program)
                .expect_err("expected call target error"),
        );
        assert_eq!(
            error,
            InterpreterError::Runtime(RuntimeError::ObjectNotCallable {
                type_name: "int".to_string()
            })
        );

        let undefined_function_program = Program {
            statements: vec![Statement::Expr(call("missing", vec![]))],
        };
        let error = expect_interpreter_error(
            run_program(&interpreter, &undefined_function_program)
                .expect_err("expected undefined name error"),
        );
        assert_eq!(
            error,
            InterpreterError::Runtime(RuntimeError::UndefinedVariable {
                name: "missing".to_string()
            })
        );
    }

    #[test]
    fn errors_on_reading_undefined_name() {
        let program = Program {
            statements: vec![print(vec![identifier("missing")])],
        };

        let interpreter = Interpreter::new();
        let error = expect_interpreter_error(
            run_program(&interpreter, &program).expect_err("expected undefined name error"),
        );
        assert_eq!(
            error,
            InterpreterError::Runtime(RuntimeError::UndefinedVariable {
                name: "missing".to_string()
            })
        );
    }

    #[test]
    fn local_names_shadow_builtins_and_declared_functions() {
        let builtin_shadow_program = Program {
            statements: vec![
                Statement::FunctionDef {
                    name: "f".to_string(),
                    params: vec![],
                    body: vec![Statement::Return(Some(int(7)))],
                },
                Statement::Assign {
                    target: AssignTarget::Name("print".to_string()),
                    value: int(1),
                },
                Statement::Assign {
                    target: AssignTarget::Name("f".to_string()),
                    value: int(2),
                },
                Statement::Expr(call("print", vec![])),
            ],
        };

        let interpreter = Interpreter::new();
        let error = expect_interpreter_error(
            run_program(&interpreter, &builtin_shadow_program)
                .expect_err("expected object not callable error"),
        );
        assert_eq!(
            error,
            InterpreterError::Runtime(RuntimeError::ObjectNotCallable {
                type_name: "int".to_string()
            })
        );

        let function_shadow_program = Program {
            statements: vec![
                Statement::FunctionDef {
                    name: "f".to_string(),
                    params: vec![],
                    body: vec![Statement::Return(Some(int(7)))],
                },
                Statement::Assign {
                    target: AssignTarget::Name("f".to_string()),
                    value: int(2),
                },
                Statement::Expr(call("f", vec![])),
            ],
        };
        let error = expect_interpreter_error(
            run_program(&interpreter, &function_shadow_program)
                .expect_err("expected object not callable error"),
        );
        assert_eq!(
            error,
            InterpreterError::Runtime(RuntimeError::ObjectNotCallable {
                type_name: "int".to_string()
            })
        );
    }

    #[test]
    fn errors_when_function_called_with_wrong_arity() {
        let program = Program {
            statements: vec![
                Statement::FunctionDef {
                    name: "f".to_string(),
                    params: vec!["x".to_string()],
                    body: vec![Statement::Pass],
                },
                Statement::Expr(call("f", vec![])),
            ],
        };

        let interpreter = Interpreter::new();
        let error = expect_interpreter_error(
            run_program(&interpreter, &program).expect_err("expected argument mismatch"),
        );
        assert_eq!(
            error,
            InterpreterError::Runtime(RuntimeError::FunctionArityMismatch {
                name: "f".to_string(),
                expected: 1,
                found: 0,
            })
        );
    }

    #[test]
    fn formats_print_output_for_boolean_string_and_none() {
        let program = Program {
            statements: vec![
                Statement::FunctionDef {
                    name: "f".to_string(),
                    params: vec![],
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
        let output = run_program(&interpreter, &program).expect("run failed");
        assert_eq!(output, "True hello None");
    }

    #[test]
    fn clears_state_between_runs() {
        let first_program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("x".to_string()),
                    value: int(1),
                },
                print(vec![identifier("x")]),
            ],
        };
        let second_program = Program {
            statements: vec![print(vec![identifier("x")])],
        };

        let interpreter = Interpreter::new();
        let output = run_program(&interpreter, &first_program).expect("first run failed");
        assert_eq!(output, "1");

        let error = expect_interpreter_error(
            run_program(&interpreter, &second_program)
                .expect_err("expected globals to be cleared between runs"),
        );
        assert_eq!(
            error,
            InterpreterError::Runtime(RuntimeError::UndefinedVariable {
                name: "x".to_string()
            })
        );
    }

    #[test]
    fn binds_multiple_function_arguments() {
        let program = Program {
            statements: vec![
                Statement::FunctionDef {
                    name: "sum2".to_string(),
                    params: vec!["a".to_string(), "b".to_string()],
                    body: vec![Statement::Return(Some(Expression::BinaryOp {
                        left: Box::new(identifier("a")),
                        op: BinaryOperator::Add,
                        right: Box::new(identifier("b")),
                    }))],
                },
                print(vec![call("sum2", vec![int(4), int(5)])]),
            ],
        };

        let interpreter = Interpreter::new();
        let output = run_program(&interpreter, &program).expect("run failed");
        assert_eq!(output, "9");
    }

    #[test]
    fn supports_list_literals_and_truthiness() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("values".to_string()),
                    value: Expression::List(vec![int(1), int(2)]),
                },
                Statement::If {
                    condition: identifier("values"),
                    then_body: vec![print(vec![identifier("values")])],
                    else_body: vec![print(vec![Expression::String("empty".to_string())])],
                },
            ],
        };

        let interpreter = Interpreter::new();
        let output = run_program(&interpreter, &program).expect("run failed");
        assert_eq!(output, "[1, 2]");
    }

    #[test]
    fn supports_list_index_and_assignment() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("values".to_string()),
                    value: Expression::List(vec![int(1), int(2)]),
                },
                Statement::Assign {
                    target: AssignTarget::Index {
                        name: "values".to_string(),
                        index: int(1),
                    },
                    value: int(7),
                },
                print(vec![Expression::Index {
                    object: Box::new(identifier("values")),
                    index: Box::new(int(0)),
                }]),
                print(vec![identifier("values")]),
            ],
        };

        let interpreter = Interpreter::new();
        let output = run_program(&interpreter, &program).expect("run failed");
        assert_eq!(output, "1\n[1, 7]");
    }

    #[test]
    fn supports_len_builtin_for_lists() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("values".to_string()),
                    value: Expression::List(vec![int(1), int(2)]),
                },
                print(vec![call("len", vec![identifier("values")])]),
            ],
        };

        let interpreter = Interpreter::new();
        let output = run_program(&interpreter, &program).expect("run failed");
        assert_eq!(output, "2");
    }

    #[test]
    fn supports_list_append_method() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("values".to_string()),
                    value: Expression::List(vec![]),
                },
                Statement::Expr(method_call("values", "append", vec![int(3)])),
                print(vec![identifier("values")]),
            ],
        };

        let interpreter = Interpreter::new();
        let output = run_program(&interpreter, &program).expect("run failed");
        assert_eq!(output, "[3]");
    }

    #[test]
    fn supports_instance_attribute_assignment() {
        let value_attr = Expression::Attribute {
            object: Box::new(identifier("b")),
            name: "value".to_string(),
        };
        let program = Program {
            statements: vec![
                Statement::ClassDef {
                    name: "Box".to_string(),
                    body: vec![Statement::FunctionDef {
                        name: "__init__".to_string(),
                        params: vec!["self".to_string(), "value".to_string()],
                        body: vec![Statement::Assign {
                            target: AssignTarget::Attribute {
                                object: identifier("self"),
                                name: "value".to_string(),
                            },
                            value: identifier("value"),
                        }],
                    }],
                },
                Statement::Assign {
                    target: AssignTarget::Name("b".to_string()),
                    value: call("Box", vec![int(7)]),
                },
                print(vec![value_attr.clone()]),
                Statement::Assign {
                    target: AssignTarget::Attribute {
                        object: identifier("b"),
                        name: "value".to_string(),
                    },
                    value: int(9),
                },
                print(vec![value_attr]),
            ],
        };

        let interpreter = Interpreter::new();
        let output = run_program(&interpreter, &program).expect("run failed");
        assert_eq!(output, "7\n9");
    }
}
