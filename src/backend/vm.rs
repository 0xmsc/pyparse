use anyhow::{Result, bail};

use crate::ast::Program;
use crate::backend::Backend;

pub struct VM;

impl Backend for VM {
    fn name(&self) -> &'static str {
        "vm"
    }

    fn run(&mut self, _program: &Program) -> Result<String> {
        bail!("VM backend not implemented")
    }
}
