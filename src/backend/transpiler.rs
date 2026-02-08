use anyhow::{Result, bail};

use crate::ast::Program;
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

    fn run(&mut self, _program: &Program) -> Result<String> {
        bail!("Transpiler backend not implemented")
    }
}
