use anyhow::{Result, bail};
use std::collections::HashMap;

use crate::ast::{Program, Statement};
use crate::backend::{Backend, PreparedBackend};

mod runtime;
mod value;

use runtime::{Environment, ExecResult, InterpreterRuntime};
use value::Value;

#[derive(Debug, Clone)]
struct Function {
    body: Vec<Statement>,
}

/// AST-walking backend that executes programs directly without compilation.
pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Self
    }

    pub fn run(&self, program: &Program) -> Result<String> {
        self.prepare(program)?.run()
    }
}

/// Prepared executable program for the tree-walking interpreter.
pub struct PreparedInterpreter {
    functions: HashMap<String, Function>,
    main_statements: Vec<Statement>,
}

impl PreparedInterpreter {
    fn run_once(&self) -> Result<String> {
        // Execution pipeline:
        // run_once -> exec_block (top-level statements) -> exec_statement
        // -> eval_expression -> eval_call -> exec_block (function body).
        let mut globals = HashMap::new();
        let mut environment = Environment::top_level(&mut globals);
        let mut runtime = InterpreterRuntime {
            functions: &self.functions,
            output: Vec::new(),
        };
        match runtime.exec_block(&self.main_statements, &mut environment)? {
            ExecResult::Continue => {}
            ExecResult::Return(_) => bail!("Return outside of function"),
        }
        Ok(runtime.output.join("\n"))
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl PreparedBackend for PreparedInterpreter {
    fn run(&self) -> Result<String> {
        self.run_once()
    }
}

impl Backend for Interpreter {
    fn name(&self) -> &'static str {
        "interpreter"
    }

    fn prepare(&self, program: &Program) -> Result<Box<dyn PreparedBackend>> {
        let mut functions = HashMap::new();
        let mut main_statements = Vec::new();

        for statement in &program.statements {
            match statement {
                Statement::FunctionDef { name, body } => {
                    functions.insert(name.clone(), Function { body: body.clone() });
                }
                _ => main_statements.push(statement.clone()),
            }
        }

        Ok(Box::new(PreparedInterpreter {
            functions,
            main_statements,
        }))
    }
}

#[cfg(test)]
mod tests;
