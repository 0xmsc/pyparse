use anyhow::Result;

use crate::ast::Program;

pub mod interpreter;
pub mod transpiler;
pub mod vm;

pub trait Backend {
    fn name(&self) -> &'static str;
    fn run(&mut self, program: &Program) -> Result<String>;
}

pub fn backends() -> Vec<Box<dyn Backend>> {
    vec![Box::new(interpreter::Interpreter::new())]
}
