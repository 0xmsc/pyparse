use crate::ast::Program;

pub struct VM;

impl VM {
    pub fn run(&self, _program: &Program) {
        println!("Running with Bytecode VM");
    }
}
