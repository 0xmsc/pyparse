use anyhow::Result;

use crate::ast::Program;
use crate::backend::interpreter::Interpreter;
use crate::backend::Backend;

pub struct VM;

impl Backend for VM {
    fn name(&self) -> &'static str {
        "vm"
    }

    fn run(&mut self, program: &Program) -> Result<String> {
        let mut interpreter = Interpreter::new();
        interpreter.run(program)
    }
}
