use anyhow::{Result, bail};
use std::collections::HashMap;

use crate::ast::{BinaryOperator, Expression, Program, Statement};
use crate::backend::Backend;

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    None,
}

impl Value {
    fn as_int(&self) -> Result<i64> {
        match self {
            Value::Integer(value) => Ok(*value),
            Value::Boolean(_) | Value::String(_) | Value::None => {
                bail!("Expected integer, got {self:?}")
            }
        }
    }

    fn to_output(&self) -> String {
        match self {
            Value::Integer(value) => value.to_string(),
            Value::Boolean(value) => {
                if *value { "True".to_string() } else { "False".to_string() }
            }
            Value::String(value) => value.clone(),
            Value::None => "None".to_string(),
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            Value::Integer(value) => *value != 0,
            Value::Boolean(value) => *value,
            Value::String(value) => !value.is_empty(),
            Value::None => false,
        }
    }
}

#[derive(Debug, Clone)]
enum Instruction {
    PushInt(i64),
    PushBool(bool),
    PushString(String),
    PushNone,
    LoadName(String),
    StoreName(String),
    Add,
    Sub,
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
struct CompiledFunction {
    code: Vec<Instruction>,
}

#[derive(Debug, Clone)]
struct CompiledProgram {
    functions: HashMap<String, CompiledFunction>,
    main: Vec<Instruction>,
}

pub struct VM {
    globals: HashMap<String, Value>,
    output: Vec<String>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            output: Vec::new(),
        }
    }

    fn compile(&self, program: &Program) -> Result<CompiledProgram> {
        let mut functions = HashMap::new();
        let mut main = Vec::new();

        for statement in &program.statements {
            match statement {
                Statement::FunctionDef { name, body } => {
                    if functions.contains_key(name) {
                        bail!("Duplicate function definition '{name}'");
                    }
                    let compiled = self.compile_function(body)?;
                    functions.insert(name.to_string(), compiled);
                }
                _ => self.compile_statement(statement, &mut main, false)?,
            }
        }

        Ok(CompiledProgram { functions, main })
    }

    fn compile_function(&self, body: &[Statement]) -> Result<CompiledFunction> {
        let mut code = Vec::new();
        for statement in body {
            self.compile_statement(statement, &mut code, true)?;
        }
        code.push(Instruction::Return);

        Ok(CompiledFunction { code })
    }

    fn compile_statement(
        &self,
        statement: &Statement,
        code: &mut Vec<Instruction>,
        in_function: bool,
    ) -> Result<()> {
        match statement {
            Statement::Assign { name, value } => {
                self.compile_expression(value, code)?;
                code.push(Instruction::StoreName(name.to_string()));
            }
            Statement::If {
                condition,
                then_body,
                else_body,
            } => {
                self.compile_expression(condition, code)?;
                let jump_if_false_pos = code.len();
                code.push(Instruction::JumpIfFalse(0));
                for stmt in then_body {
                    self.compile_statement(stmt, code, in_function)?;
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
                        self.compile_statement(stmt, code, in_function)?;
                    }
                    let end = code.len();
                    code[jump_pos] = Instruction::Jump(end);
                }
            }
            Statement::Return(value) => {
                if !in_function {
                    bail!("Return outside of function is not supported in the VM");
                }
                if let Some(value) = value {
                    self.compile_expression(value, code)?;
                } else {
                    code.push(Instruction::PushNone);
                }
                code.push(Instruction::ReturnValue);
            }
            Statement::Pass => {}
            Statement::Expr(expr) => {
                self.compile_expression(expr, code)?;
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

    fn compile_expression(&self, expr: &Expression, code: &mut Vec<Instruction>) -> Result<()> {
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
                self.compile_expression(left, code)?;
                self.compile_expression(right, code)?;
                match op {
                    BinaryOperator::Add => code.push(Instruction::Add),
                    BinaryOperator::Sub => code.push(Instruction::Sub),
                }
            }
            Expression::Call { callee, args } => {
                if args.len() > 1 {
                    bail!("VM only supports zero or one call argument");
                }
                match callee.as_ref() {
                    Expression::Identifier(name) if name == "print" => {
                        if let Some(arg) = args.get(0) {
                            self.compile_expression(arg, code)?;
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

    fn execute_program(&mut self, program: &CompiledProgram) -> Result<()> {
        let mut stack = Vec::new();
        self.execute_code(&program.main, &mut stack, None, program)?;
        Ok(())
    }

    fn execute_code(
        &mut self,
        code: &[Instruction],
        stack: &mut Vec<Value>,
        mut locals: Option<&mut HashMap<String, Value>>,
        program: &CompiledProgram,
    ) -> Result<Value> {
        let mut ip = 0;
        loop {
            let instruction = match code.get(ip) {
                Some(instruction) => instruction.clone(),
                None => return Ok(Value::None),
            };
            ip += 1;
            match instruction {
                Instruction::PushInt(value) => stack.push(Value::Integer(value)),
                Instruction::PushBool(value) => stack.push(Value::Boolean(value)),
                Instruction::PushString(value) => stack.push(Value::String(value)),
                Instruction::PushNone => stack.push(Value::None),
                Instruction::LoadName(name) => {
                    if let Some(locals) = locals.as_ref() {
                        if let Some(value) = locals.get(&name) {
                            stack.push(value.clone());
                            continue;
                        }
                    }
                    let value = self
                        .globals
                        .get(&name)
                        .cloned()
                        .ok_or_else(|| anyhow::anyhow!("Undefined variable '{name}'"))?;
                    stack.push(value);
                }
                Instruction::StoreName(name) => {
                    let value = stack
                        .pop()
                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                    if let Some(locals) = locals.as_mut() {
                        locals.insert(name, value);
                    } else {
                        self.globals.insert(name, value);
                    }
                }
                Instruction::Add => {
                    let right = stack
                        .pop()
                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                    let left = stack
                        .pop()
                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                    let result = left.as_int()? + right.as_int()?;
                    stack.push(Value::Integer(result));
                }
                Instruction::Sub => {
                    let right = stack
                        .pop()
                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                    let left = stack
                        .pop()
                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                    let result = left.as_int()? - right.as_int()?;
                    stack.push(Value::Integer(result));
                }
                Instruction::CallBuiltinPrint0 => {
                    self.output.push(String::new());
                    stack.push(Value::None);
                }
                Instruction::CallBuiltinPrint1 => {
                    let value = stack
                        .pop()
                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                    self.output.push(value.to_output());
                    stack.push(Value::None);
                }
                Instruction::CallFunction(name) => {
                    let function = program
                        .functions
                        .get(&name)
                        .ok_or_else(|| anyhow::anyhow!("Undefined function '{name}'"))?
                        .clone();
                    let mut locals_map = HashMap::new();
                    let return_value = self.execute_code(
                        &function.code,
                        &mut Vec::new(),
                        Some(&mut locals_map),
                        program,
                    )?;
                    stack.push(return_value);
                }
                Instruction::JumpIfFalse(target) => {
                    let value = stack
                        .pop()
                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                    if !value.is_truthy() {
                        ip = target;
                    }
                }
                Instruction::Jump(target) => {
                    ip = target;
                }
                Instruction::Pop => {
                    stack
                        .pop()
                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                }
                Instruction::Return => return Ok(Value::None),
                Instruction::ReturnValue => {
                    let value = stack
                        .pop()
                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                    return Ok(value);
                }
            }
        }
    }
}

impl Backend for VM {
    fn name(&self) -> &'static str {
        "vm"
    }

    fn run(&mut self, program: &Program) -> Result<String> {
        self.globals.clear();
        self.output.clear();
        let compiled = self.compile(program)?;
        self.execute_program(&compiled)?;
        Ok(self.output.join("\n"))
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}
