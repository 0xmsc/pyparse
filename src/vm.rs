//! Bytecode backend.
//!
//! Pipeline: `AST -> CompiledProgram -> stack-machine execution`.
//! `compile` lowers syntax into `Instruction`s, then `VmRuntime` interprets
//! those instructions with runtime object/call semantics shared via `Value`.

use anyhow::Result;
use std::collections::HashMap;

use crate::ast::Program;
use crate::backend::{Backend, PreparedBackend};
use crate::bytecode::{CompiledProgram, compile};
use crate::runtime::value::Value;

mod runtime;

use runtime::{VmResult, run_compiled_program};

/// Bytecode backend entry point.
///
/// Holds backend-global state used by direct `run_compiled` calls.
pub struct VM {
    globals: HashMap<String, Value>,
}

/// Prepared VM artifact containing compiled bytecode plus callable metadata.
pub struct PreparedVM {
    compiled: CompiledProgram,
}

impl VM {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
        }
    }

    fn run_compiled(&mut self, program: &CompiledProgram) -> VmResult<String> {
        run_compiled_program(program, &mut self.globals)
    }
}

impl Backend for VM {
    fn name(&self) -> &'static str {
        "vm"
    }

    fn prepare(&self, program: &Program) -> Result<Box<dyn PreparedBackend>> {
        Ok(Box::new(PreparedVM {
            compiled: compile(program)?,
        }))
    }
}

impl PreparedBackend for PreparedVM {
    fn run(&self) -> Result<String> {
        let mut vm = VM::new();
        Ok(vm.run_compiled(&self.compiled)?)
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::{VM, runtime::VmError};
    use crate::ast::{AssignTarget, Expression, Program, Statement};
    use crate::bytecode::{CompiledProgram, Instruction, compile};
    use crate::runtime::error::RuntimeError;
    use indoc::indoc;

    fn call(name: &str, args: Vec<Expression>) -> Expression {
        Expression::Call {
            callee: Box::new(Expression::Identifier(name.to_string())),
            args,
        }
    }

    fn method_call(receiver: &str, method: &str, args: Vec<Expression>) -> Expression {
        Expression::Call {
            callee: Box::new(Expression::Attribute {
                object: Box::new(Expression::Identifier(receiver.to_string())),
                name: method.to_string(),
            }),
            args,
        }
    }

    #[test]
    fn run_compiled_reports_stack_underflow() {
        let compiled = CompiledProgram {
            callables: vec![],
            main: vec![Instruction::Pop],
        };

        let mut vm = VM::new();
        let error = vm
            .run_compiled(&compiled)
            .expect_err("expected stack underflow");
        assert_eq!(error, VmError::StackUnderflow);
    }

    #[test]
    fn run_compiled_reports_invalid_jump_target() {
        let compiled = CompiledProgram {
            callables: vec![],
            main: vec![Instruction::Jump(10)],
        };

        let mut vm = VM::new();
        let error = vm
            .run_compiled(&compiled)
            .expect_err("expected invalid jump target");
        assert_eq!(error, VmError::InvalidJumpTarget);
    }

    #[test]
    fn function_locals_shadow_globals() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("x".to_string()),
                    value: Expression::Integer(1),
                },
                Statement::FunctionDef {
                    name: "f".to_string(),
                    params: vec!["x".to_string()],
                    body: vec![Statement::Return(Some(Expression::Identifier(
                        "x".to_string(),
                    )))],
                },
                Statement::Expr(call("print", vec![call("f", vec![Expression::Integer(2)])])),
                Statement::Expr(call("print", vec![Expression::Identifier("x".to_string())])),
            ],
        };

        let compiled = compile(&program).expect("compile should succeed");
        let mut vm = VM::new();
        let output = vm.run_compiled(&compiled).expect("run should succeed");
        assert_eq!(
            output,
            indoc! {"
                2
                1
            "}
            .trim_end()
        );
    }

    #[test]
    fn list_append_mutates_receiver_and_returns_none() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("values".to_string()),
                    value: Expression::List(vec![]),
                },
                Statement::Expr(call(
                    "print",
                    vec![method_call(
                        "values",
                        "append",
                        vec![Expression::Integer(3)],
                    )],
                )),
                Statement::Expr(call(
                    "print",
                    vec![Expression::Identifier("values".to_string())],
                )),
            ],
        };

        let compiled = compile(&program).expect("compile should succeed");
        let mut vm = VM::new();
        let output = vm.run_compiled(&compiled).expect("run should succeed");
        assert_eq!(
            output,
            indoc! {"
                None
                [3]
            "}
            .trim_end()
        );
    }

    #[test]
    fn supports_instance_attribute_assignment() {
        let value_attr = Expression::Attribute {
            object: Box::new(Expression::Identifier("b".to_string())),
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
                                object: Expression::Identifier("self".to_string()),
                                name: "value".to_string(),
                            },
                            value: Expression::Identifier("value".to_string()),
                        }],
                    }],
                },
                Statement::Assign {
                    target: AssignTarget::Name("b".to_string()),
                    value: call("Box", vec![Expression::Integer(7)]),
                },
                Statement::Expr(call("print", vec![value_attr.clone()])),
                Statement::Assign {
                    target: AssignTarget::Attribute {
                        object: Expression::Identifier("b".to_string()),
                        name: "value".to_string(),
                    },
                    value: Expression::Integer(9),
                },
                Statement::Expr(call("print", vec![value_attr])),
            ],
        };

        let compiled = compile(&program).expect("compile should succeed");
        let mut vm = VM::new();
        let output = vm.run_compiled(&compiled).expect("run should succeed");
        assert_eq!(
            output,
            indoc! {"
                7
                9
            "}
            .trim_end()
        );
    }

    #[test]
    fn errors_on_calling_or_reading_undefined_name() {
        let missing_call_program = Program {
            statements: vec![Statement::Expr(call("missing", vec![]))],
        };
        let compiled = compile(&missing_call_program).expect("compile should succeed");
        let mut vm = VM::new();
        let error = vm
            .run_compiled(&compiled)
            .expect_err("expected undefined name error");
        assert_eq!(
            error,
            VmError::Runtime(RuntimeError::UndefinedVariable {
                name: "missing".to_string()
            })
        );

        let missing_read_program = Program {
            statements: vec![Statement::Expr(call(
                "print",
                vec![Expression::Identifier("missing".to_string())],
            ))],
        };
        let compiled = compile(&missing_read_program).expect("compile should succeed");
        let mut vm = VM::new();
        let error = vm
            .run_compiled(&compiled)
            .expect_err("expected undefined name error");
        assert_eq!(
            error,
            VmError::Runtime(RuntimeError::UndefinedVariable {
                name: "missing".to_string()
            })
        );
    }

    #[test]
    fn names_shadow_builtins_and_declared_functions() {
        let builtin_shadow_program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("print".to_string()),
                    value: Expression::Integer(1),
                },
                Statement::Expr(call("print", vec![])),
            ],
        };
        let compiled = compile(&builtin_shadow_program).expect("compile should succeed");
        let mut vm = VM::new();
        let error = vm
            .run_compiled(&compiled)
            .expect_err("expected object not callable error");
        assert_eq!(
            error,
            VmError::Runtime(RuntimeError::ObjectNotCallable {
                type_name: "int".to_string()
            })
        );

        let function_shadow_program = Program {
            statements: vec![
                Statement::FunctionDef {
                    name: "f".to_string(),
                    params: vec![],
                    body: vec![Statement::Return(Some(Expression::Integer(7)))],
                },
                Statement::Assign {
                    target: AssignTarget::Name("f".to_string()),
                    value: Expression::Integer(2),
                },
                Statement::Expr(call("f", vec![])),
            ],
        };
        let compiled = compile(&function_shadow_program).expect("compile should succeed");
        let mut vm = VM::new();
        let error = vm
            .run_compiled(&compiled)
            .expect_err("expected object not callable error");
        assert_eq!(
            error,
            VmError::Runtime(RuntimeError::ObjectNotCallable {
                type_name: "int".to_string()
            })
        );
    }
}
