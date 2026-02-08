use anyhow::Result;

use crate::ast::Program;
use crate::backend::interpreter::Interpreter;
use crate::backend::Backend;

pub struct Transpiler;

impl Transpiler {
    pub fn transpile(&self, _program: &Program) -> String {
        String::new()
    }
}

impl Backend for Transpiler {
    fn name(&self) -> &'static str {
        "transpiler"
    }

    fn run(&mut self, program: &Program) -> Result<String> {
        let mut interpreter = Interpreter::new();
        interpreter.run(program)
    }
}
