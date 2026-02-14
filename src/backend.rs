use anyhow::Result;

use crate::ast::Program;

pub use crate::interpreter;
pub use crate::jit;
pub use crate::transpiler;
pub use crate::vm;

pub trait PreparedBackend {
    fn run(&self) -> Result<String>;
}

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
        Box::new(crate::transpiler::Transpiler),
    ]
}
