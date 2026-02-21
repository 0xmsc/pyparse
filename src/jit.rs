//! Cranelift JIT backend.
//!
//! Pipeline: `AST -> bytecode -> Cranelift IR -> machine code`.
//! Generated code does not implement Python object semantics directly; it calls
//! runtime hooks in `jit::runtime` for allocation, dispatch, and name access.

use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use anyhow::Result;
use cranelift_jit::JITModule;

use crate::ast::Program;
use crate::backend::{Backend, PreparedBackend};

mod codegen;
mod runtime;

use runtime::{CompiledFunctionPointer, EntryFunction};

static DUMP_CLIF: AtomicBool = AtomicBool::new(false);

pub struct JIT;

/// JIT-compiled executable state kept alive for function pointer validity.
///
/// The module owns generated machine code; dropping it invalidates `entry` and
/// `functions`.
pub struct PreparedProgram {
    _module: JITModule,
    entry: EntryFunction,
    functions: Arc<Vec<CompiledFunctionPointer>>,
}

pub struct PreparedJIT {
    prepared: PreparedProgram,
}

impl JIT {
    pub fn new() -> Self {
        Self
    }

    /// Compiles an AST program into executable JIT machine code and runtime metadata.
    pub fn prepare(&self, program: &Program) -> Result<PreparedProgram> {
        codegen::prepare_program(program)
    }

    /// Executes a previously prepared JIT program and returns captured stdout.
    pub fn run_prepared(&self, prepared: &PreparedProgram) -> Result<String> {
        runtime::run_prepared(prepared.entry, prepared.functions.clone())
    }
}

pub fn set_dump_clif(enabled: bool) {
    DUMP_CLIF.store(enabled, Ordering::Relaxed);
}

pub(super) fn dump_clif_enabled() -> bool {
    DUMP_CLIF.load(Ordering::Relaxed)
}

impl Default for JIT {
    fn default() -> Self {
        Self::new()
    }
}

impl Backend for JIT {
    fn name(&self) -> &'static str {
        "jit"
    }

    fn prepare(&self, program: &Program) -> Result<Box<dyn PreparedBackend>> {
        Ok(Box::new(PreparedJIT {
            prepared: JIT::prepare(self, program)?,
        }))
    }
}

impl PreparedBackend for PreparedJIT {
    fn run(&self) -> Result<String> {
        JIT::new().run_prepared(&self.prepared)
    }
}

#[cfg(test)]
mod tests {
    use super::JIT;
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

    fn print(args: Vec<Expression>) -> Statement {
        Statement::Expr(call("print", args))
    }

    fn run_program(program: &Program) -> anyhow::Result<String> {
        let jit = JIT::new();
        let prepared = jit.prepare(program)?;
        jit.run_prepared(&prepared)
    }

    #[test]
    fn executes_simple_assignment_and_print() {
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

        let output = run_program(&program).expect("run should succeed");
        assert_eq!(output, "3");
    }

    #[test]
    fn function_locals_shadow_globals() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("x".to_string()),
                    value: int(1),
                },
                Statement::FunctionDef {
                    name: "f".to_string(),
                    params: vec!["x".to_string()],
                    body: vec![Statement::Return(Some(identifier("x")))],
                },
                print(vec![call("f", vec![int(2)])]),
                print(vec![identifier("x")]),
            ],
        };

        let output = run_program(&program).expect("run should succeed");
        assert_eq!(output, "2\n1");
    }
}
