//! Tree-walking backend.
//!
//! This path executes AST statements directly without a bytecode stage.
//! Top-level execution seeds builtins in globals, then delegates statement and
//! expression evaluation to `InterpreterRuntime`.

use anyhow::Result;

use crate::ast::{Program, Statement};
use crate::backend::{Backend, PreparedBackend};
use crate::runtime::error::RuntimeError;
use crate::runtime::execution::{Environment, seed_builtin_globals};

mod error;
mod runtime;
mod value;

use error::InterpreterError;
use runtime::{ExecResult, InterpreterRuntime};

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Self
    }
}

pub struct PreparedInterpreter {
    statements: Vec<Statement>,
}

impl PreparedInterpreter {
    fn run_once(&self) -> std::result::Result<String, InterpreterError> {
        // Execution pipeline:
        // run_once -> exec_block (top-level statements) -> exec_statement
        // -> eval_expression -> eval_call -> exec_block (function body).
        let mut globals = std::collections::HashMap::new();
        seed_builtin_globals(&mut globals);
        let mut environment = Environment::top_level(&mut globals);
        let mut runtime = InterpreterRuntime::new();
        match runtime.exec_block(&self.statements, &mut environment)? {
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
        Ok(Box::new(PreparedInterpreter {
            statements: program.statements.clone(),
        }))
    }
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
    fn executes_for_loop_over_list_and_range() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("total".to_string()),
                    value: int(0),
                },
                Statement::For {
                    target: "x".to_string(),
                    iterable: Expression::List(vec![int(1), int(2), int(3)]),
                    body: vec![Statement::Assign {
                        target: AssignTarget::Name("total".to_string()),
                        value: Expression::BinaryOp {
                            left: Box::new(identifier("total")),
                            op: BinaryOperator::Add,
                            right: Box::new(identifier("x")),
                        },
                    }],
                },
                print(vec![identifier("total")]),
                Statement::For {
                    target: "x".to_string(),
                    iterable: call("range", vec![int(3)]),
                    body: vec![print(vec![identifier("x")])],
                },
                print(vec![identifier("x")]),
            ],
        };

        let interpreter = Interpreter::new();
        let output = run_program(&interpreter, &program).expect("run failed");
        assert_eq!(output, "6\n0\n1\n2\n2");
    }

    #[test]
    fn handles_try_except_and_finally() {
        let program = Program {
            statements: vec![
                Statement::Try {
                    body: vec![Statement::Raise(Expression::Call {
                        callee: Box::new(identifier("Exception")),
                        args: vec![Expression::String("boom".to_string())],
                    })],
                    except_body: Some(vec![print(vec![Expression::String("caught".to_string())])]),
                    finally_body: Some(vec![print(vec![Expression::String(
                        "finally".to_string(),
                    )])]),
                },
                print(vec![Expression::String("after".to_string())]),
            ],
        };

        let interpreter = Interpreter::new();
        let output = run_program(&interpreter, &program).expect("run failed");
        assert_eq!(output, "caught\nfinally\nafter");
    }

    #[test]
    fn runs_finally_before_returning_from_function() {
        let program = Program {
            statements: vec![
                Statement::FunctionDef {
                    name: "f".to_string(),
                    params: vec![],
                    body: vec![Statement::Try {
                        body: vec![Statement::Return(Some(int(1)))],
                        except_body: None,
                        finally_body: Some(vec![print(vec![Expression::String(
                            "cleanup".to_string(),
                        )])]),
                    }],
                },
                print(vec![call("f", vec![])]),
            ],
        };

        let interpreter = Interpreter::new();
        let output = run_program(&interpreter, &program).expect("run failed");
        assert_eq!(output, "cleanup\n1");
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
    fn supports_dict_literals_index_assignment_and_len() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("values".to_string()),
                    value: Expression::Dict(vec![
                        (Expression::String("a".to_string()), int(1)),
                        (Expression::String("b".to_string()), int(2)),
                    ]),
                },
                print(vec![identifier("values")]),
                print(vec![Expression::Index {
                    object: Box::new(identifier("values")),
                    index: Box::new(Expression::String("a".to_string())),
                }]),
                Statement::Assign {
                    target: AssignTarget::Index {
                        name: "values".to_string(),
                        index: Expression::String("b".to_string()),
                    },
                    value: int(7),
                },
                print(vec![identifier("values")]),
                print(vec![call("len", vec![identifier("values")])]),
                Statement::If {
                    condition: identifier("values"),
                    then_body: vec![print(vec![int(1)])],
                    else_body: vec![print(vec![int(0)])],
                },
                Statement::Assign {
                    target: AssignTarget::Name("empty".to_string()),
                    value: Expression::Dict(vec![]),
                },
                Statement::If {
                    condition: identifier("empty"),
                    then_body: vec![print(vec![int(1)])],
                    else_body: vec![print(vec![int(0)])],
                },
            ],
        };

        let interpreter = Interpreter::new();
        let output = run_program(&interpreter, &program).expect("run failed");
        assert_eq!(
            output,
            "{\"a\": 1, \"b\": 2}\n1\n{\"a\": 1, \"b\": 7}\n2\n1\n0"
        );
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
