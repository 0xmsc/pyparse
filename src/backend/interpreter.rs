use anyhow::{Result, bail};
use std::collections::HashMap;

use crate::ast::{BinaryOperator, Expression, Program, Statement};
use crate::backend::Backend;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Integer(i64),
    None,
}

impl Value {
    fn as_int(&self) -> Result<i64> {
        match self {
            Value::Integer(value) => Ok(*value),
            Value::None => bail!("Expected integer, got None"),
        }
    }

    fn to_output(&self) -> String {
        match self {
            Value::Integer(value) => value.to_string(),
            Value::None => "None".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
struct Function {
    body: Vec<Statement>,
}

pub struct Interpreter {
    globals: HashMap<String, Value>,
    functions: HashMap<String, Function>,
    output: Vec<String>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            functions: HashMap::new(),
            output: Vec::new(),
        }
    }

    pub fn run(&mut self, program: &Program) -> Result<String> {
        self.globals.clear();
        self.functions.clear();
        self.output.clear();
        for statement in &program.statements {
            self.exec_statement(statement, None)?;
        }
        Ok(self.output.join("\n"))
    }

    fn exec_statement(
        &mut self,
        statement: &Statement,
        mut locals: Option<&mut HashMap<String, Value>>,
    ) -> Result<()> {
        match statement {
            Statement::FunctionDef { name, body } => {
                self.functions
                    .insert(name.to_string(), Function { body: body.clone() });
                Ok(())
            }
            Statement::Assign { name, value } => {
                let value = self.eval_expression(value, locals.as_deref_mut())?;
                if let Some(locals) = locals {
                    locals.insert(name.to_string(), value);
                } else {
                    self.globals.insert(name.to_string(), value);
                }
                Ok(())
            }
            Statement::Expr(expr) => {
                self.eval_expression(expr, locals.as_deref_mut())?;
                Ok(())
            }
        }
    }

    fn eval_expression(
        &mut self,
        expr: &Expression,
        mut locals: Option<&mut HashMap<String, Value>>,
    ) -> Result<Value> {
        match expr {
            Expression::Integer(value) => Ok(Value::Integer(*value)),
            Expression::Identifier(name) => {
                if let Some(locals) = locals.as_deref() {
                    if let Some(value) = locals.get(name) {
                        return Ok(value.clone());
                    }
                }
                if let Some(value) = self.globals.get(name) {
                    return Ok(value.clone());
                }
                bail!("Undefined variable '{name}'")
            }
            Expression::BinaryOp { left, op, right } => {
                let left = self.eval_expression(left, locals.as_deref_mut())?;
                let right = self.eval_expression(right, locals.as_deref_mut())?;
                let left = left.as_int()?;
                let right = right.as_int()?;
                match op {
                    BinaryOperator::Add => Ok(Value::Integer(left + right)),
                    BinaryOperator::Sub => Ok(Value::Integer(left - right)),
                }
            }
            Expression::Call { callee, args } => self.eval_call(callee, args, locals),
        }
    }

    fn eval_call(
        &mut self,
        callee: &Expression,
        args: &[Expression],
        mut locals: Option<&mut HashMap<String, Value>>,
    ) -> Result<Value> {
        let mut evaluated_args = Vec::with_capacity(args.len());
        for arg in args {
            let value = self.eval_expression(arg, locals.as_deref_mut())?;
            evaluated_args.push(value);
        }

        match callee {
            Expression::Identifier(name) => match name.as_str() {
                "print" => {
                    let outputs: Vec<String> =
                        evaluated_args.iter().map(Value::to_output).collect();
                    self.output.push(outputs.join(" "));
                    Ok(Value::None)
                }
                _ => {
                    let function = self
                        .functions
                        .get(name)
                        .cloned()
                        .ok_or_else(|| anyhow::anyhow!("Undefined function '{name}'"))?;
                    if !evaluated_args.is_empty() {
                        bail!("Function '{name}' does not accept arguments");
                    }
                    let mut local_scope = HashMap::new();
                    for statement in &function.body {
                        self.exec_statement(statement, Some(&mut local_scope))?;
                    }
                    Ok(Value::None)
                }
            },
            _ => bail!("Can only call identifiers"),
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Backend for Interpreter {
    fn name(&self) -> &'static str {
        "interpreter"
    }

    fn run(&mut self, program: &Program) -> Result<String> {
        Interpreter::run(self, program)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOperator, Expression, Program, Statement};

    #[test]
    fn evaluates_assignment_and_call() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    name: "n".to_string(),
                    value: Expression::BinaryOp {
                        left: Box::new(Expression::Integer(1)),
                        op: BinaryOperator::Add,
                        right: Box::new(Expression::Integer(2)),
                    },
                },
                Statement::Expr(Expression::Call {
                    callee: Box::new(Expression::Identifier("print".to_string())),
                    args: vec![Expression::Identifier("n".to_string())],
                }),
            ],
        };

        let mut interpreter = Interpreter::new();
        let output = interpreter.run(&program).expect("run failed");
        assert_eq!(output, "3");
        assert_eq!(interpreter.globals.get("n"), Some(&Value::Integer(3)));
    }
}
