use anyhow::Result;

use crate::ast::Program;

pub mod interpreter;
pub mod jit;
pub mod transpiler;
pub mod vm;

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
        Box::new(interpreter::Interpreter::new()),
        Box::new(vm::VM::new()),
        Box::new(jit::JIT::new()),
        Box::new(transpiler::Transpiler),
    ]
}
