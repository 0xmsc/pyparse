use crate::ast::Program;

pub struct Interpreter;

impl Interpreter {
    pub fn run(&self, _program: &Program) {
        println!("Running with Tree Walker Interpreter");
    }
}
