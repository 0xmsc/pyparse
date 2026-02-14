use anyhow::{Result, bail};
use std::collections::HashMap;

use crate::ast::{BinaryOperator, Expression, Program, Statement};

#[derive(Debug, Clone)]
pub enum Instruction {
    PushInt(i64),
    PushBool(bool),
    PushString(String),
    PushNone,
    LoadName(String),
    StoreName(String),
    Add,
    Sub,
    LessThan,
    CallBuiltinPrint0,
    CallBuiltinPrint1,
    CallFunction(String),
    JumpIfFalse(usize),
    Jump(usize),
    Pop,
    Return,
    ReturnValue,
}

#[derive(Debug, Clone)]
pub struct CompiledFunction {
    pub code: Vec<Instruction>,
}

#[derive(Debug, Clone)]
pub struct CompiledProgram {
    pub functions: HashMap<String, CompiledFunction>,
    pub main: Vec<Instruction>,
}

pub fn compile(program: &Program) -> Result<CompiledProgram> {
    let mut functions = HashMap::new();
    let mut main = Vec::new();

    for statement in &program.statements {
        match statement {
            Statement::FunctionDef { name, body } => {
                if functions.contains_key(name) {
                    bail!("Duplicate function definition '{name}'");
                }
                let compiled = compile_function(body)?;
                functions.insert(name.to_string(), compiled);
            }
            _ => compile_statement(statement, &mut main, false)?,
        }
    }

    Ok(CompiledProgram { functions, main })
}

fn compile_function(body: &[Statement]) -> Result<CompiledFunction> {
    let mut code = Vec::new();
    for statement in body {
        compile_statement(statement, &mut code, true)?;
    }
    code.push(Instruction::Return);

    Ok(CompiledFunction { code })
}

fn compile_statement(
    statement: &Statement,
    code: &mut Vec<Instruction>,
    in_function: bool,
) -> Result<()> {
    match statement {
        Statement::Assign { name, value } => {
            compile_expression(value, code)?;
            code.push(Instruction::StoreName(name.to_string()));
        }
        Statement::If {
            condition,
            then_body,
            else_body,
        } => {
            compile_expression(condition, code)?;
            let jump_if_false_pos = code.len();
            code.push(Instruction::JumpIfFalse(0));
            for stmt in then_body {
                compile_statement(stmt, code, in_function)?;
            }
            if else_body.is_empty() {
                let end = code.len();
                code[jump_if_false_pos] = Instruction::JumpIfFalse(end);
            } else {
                let jump_pos = code.len();
                code.push(Instruction::Jump(0));
                let else_start = code.len();
                code[jump_if_false_pos] = Instruction::JumpIfFalse(else_start);
                for stmt in else_body {
                    compile_statement(stmt, code, in_function)?;
                }
                let end = code.len();
                code[jump_pos] = Instruction::Jump(end);
            }
        }
        Statement::While { condition, body } => {
            let loop_start = code.len();
            compile_expression(condition, code)?;
            let jump_if_false_pos = code.len();
            code.push(Instruction::JumpIfFalse(0));
            for stmt in body {
                compile_statement(stmt, code, in_function)?;
            }
            code.push(Instruction::Jump(loop_start));
            let loop_end = code.len();
            code[jump_if_false_pos] = Instruction::JumpIfFalse(loop_end);
        }
        Statement::Return(value) => {
            if !in_function {
                bail!("Return outside of function is not supported in the VM");
            }
            if let Some(value) = value {
                compile_expression(value, code)?;
            } else {
                code.push(Instruction::PushNone);
            }
            code.push(Instruction::ReturnValue);
        }
        Statement::Pass => {}
        Statement::Expr(expr) => {
            compile_expression(expr, code)?;
            code.push(Instruction::Pop);
        }
        Statement::FunctionDef { .. } => {
            if in_function {
                bail!("Nested function definitions are not supported in the VM");
            } else {
                bail!("Unexpected function definition during compilation");
            }
        }
    }
    Ok(())
}

fn compile_expression(expr: &Expression, code: &mut Vec<Instruction>) -> Result<()> {
    match expr {
        Expression::Integer(value) => {
            code.push(Instruction::PushInt(*value));
        }
        Expression::Boolean(value) => {
            code.push(Instruction::PushBool(*value));
        }
        Expression::String(value) => {
            code.push(Instruction::PushString(value.clone()));
        }
        Expression::Identifier(name) => {
            code.push(Instruction::LoadName(name.to_string()));
        }
        Expression::BinaryOp { left, op, right } => {
            compile_expression(left, code)?;
            compile_expression(right, code)?;
            match op {
                BinaryOperator::Add => code.push(Instruction::Add),
                BinaryOperator::Sub => code.push(Instruction::Sub),
                BinaryOperator::LessThan => code.push(Instruction::LessThan),
            }
        }
        Expression::Call { callee, args } => {
            if args.len() > 1 {
                bail!("VM only supports zero or one call argument");
            }
            match callee.as_ref() {
                Expression::Identifier(name) if name == "print" => {
                    if let Some(arg) = args.first() {
                        compile_expression(arg, code)?;
                        code.push(Instruction::CallBuiltinPrint1);
                    } else {
                        code.push(Instruction::CallBuiltinPrint0);
                    }
                }
                Expression::Identifier(name) => {
                    if !args.is_empty() {
                        bail!("Function '{name}' does not accept arguments");
                    }
                    code.push(Instruction::CallFunction(name.to_string()));
                }
                _ => bail!("Can only call identifiers in the VM"),
            }
        }
    }
    Ok(())
}
