use crate::ast::Program;

pub struct Transpiler;

impl Transpiler {
    pub fn transpile(&self, _program: &Program) -> String {
        println!("Running with Transpiler");
        String::new()
    }
}
