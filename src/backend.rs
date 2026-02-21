use anyhow::Result;

use crate::ast::Program;

pub use crate::interpreter;
pub use crate::jit;
pub use crate::vm;

/// Executable artifact produced by a backend `prepare` step.
///
/// This keeps compilation and execution separated so benchmarks and tests can
/// measure/validate prepare-vs-run phases independently.
pub trait PreparedBackend {
    fn run(&self) -> Result<String>;
}

/// Common interface implemented by each execution backend.
///
/// `prepare` translates AST into backend-owned executable state, while `run`
/// offers the convenience path for one-shot execution.
pub trait Backend {
    fn name(&self) -> &'static str;
    fn prepare(&self, program: &Program) -> Result<Box<dyn PreparedBackend>>;

    fn run(&self, program: &Program) -> Result<String> {
        self.prepare(program)?.run()
    }
}

pub fn backends() -> Vec<Box<dyn Backend>> {
    vec![
        Box::new(crate::interpreter::Interpreter::new()),
        Box::new(crate::vm::VM::new()),
        Box::new(crate::jit::JIT::new()),
    ]
}
