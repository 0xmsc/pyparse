use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Result;
use cranelift_jit::JITModule;

use crate::ast::Program;
use crate::backend::{Backend, PreparedBackend};

mod codegen;
mod runtime;

use runtime::{CompiledFunctionPointer, EntryFunction};

pub struct JIT;

pub struct PreparedProgram {
    _module: JITModule,
    entry: EntryFunction,
    functions: Arc<HashMap<String, CompiledFunctionPointer>>,
}

pub struct PreparedJIT {
    prepared: PreparedProgram,
}

impl JIT {
    pub fn new() -> Self {
        Self
    }

    pub fn prepare(&self, program: &Program) -> Result<PreparedProgram> {
        codegen::prepare_program(program)
    }

    pub fn run_prepared(&self, prepared: &PreparedProgram) -> Result<String> {
        runtime::run_prepared(prepared.entry, prepared.functions.clone())
    }
}

impl Default for JIT {
    fn default() -> Self {
        Self::new()
    }
}

impl Backend for JIT {
    fn name(&self) -> &'static str {
        "jit"
    }

    fn prepare(&self, program: &Program) -> Result<Box<dyn PreparedBackend>> {
        Ok(Box::new(PreparedJIT {
            prepared: JIT::prepare(self, program)?,
        }))
    }
}

impl PreparedBackend for PreparedJIT {
    fn run(&self) -> Result<String> {
        JIT::new().run_prepared(&self.prepared)
    }
}
