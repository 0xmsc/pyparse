use anyhow::{Result, bail};
use std::collections::HashMap;

use crate::ast::Program;
use crate::backend::{Backend, PreparedBackend};
use crate::builtins::BuiltinFunction;
use crate::bytecode::{CompiledProgram, Instruction, compile};

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    List(Vec<Value>),
    None,
}

impl Value {
    fn as_int(&self) -> Result<i64> {
        match self {
            Value::Integer(value) => Ok(*value),
            Value::Boolean(_) | Value::String(_) | Value::List(_) | Value::None => {
                bail!("Expected integer, got {self:?}")
            }
        }
    }

    fn as_list(&self) -> Result<&Vec<Value>> {
        match self {
            Value::List(values) => Ok(values),
            other => bail!("Expected list, got {other:?}"),
        }
    }

    fn as_list_mut(&mut self) -> Result<&mut Vec<Value>> {
        match self {
            Value::List(values) => Ok(values),
            other => bail!("Expected list, got {other:?}"),
        }
    }

    fn to_output(&self) -> String {
        match self {
            Value::Integer(value) => value.to_string(),
            Value::Boolean(value) => {
                if *value {
                    "True".to_string()
                } else {
                    "False".to_string()
                }
            }
            Value::String(value) => value.clone(),
            Value::List(values) => {
                let rendered = values
                    .iter()
                    .map(Value::to_output)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{rendered}]")
            }
            Value::None => "None".to_string(),
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            Value::Integer(value) => *value != 0,
            Value::Boolean(value) => *value,
            Value::String(value) => !value.is_empty(),
            Value::List(values) => !values.is_empty(),
            Value::None => false,
        }
    }
}

pub struct VM {
    globals: HashMap<String, Value>,
    output: Vec<String>,
}

pub struct PreparedVM {
    compiled: CompiledProgram,
}

impl VM {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            output: Vec::new(),
        }
    }

    fn execute_program(&mut self, program: &CompiledProgram) -> Result<()> {
        let mut stack = Vec::new();
        self.execute_code(&program.main, &mut stack, None, program)?;
        Ok(())
    }

    pub fn run_compiled(&mut self, program: &CompiledProgram) -> Result<String> {
        self.globals.clear();
        self.output.clear();
        self.execute_program(program)?;
        Ok(self.output.join("\n"))
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
                Instruction::BuildList(count) => {
                    let mut values = Vec::with_capacity(count);
                    for _ in 0..count {
                        let value = stack
                            .pop()
                            .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                        values.push(value);
                    }
                    values.reverse();
                    stack.push(Value::List(values));
                }
                Instruction::PushNone => stack.push(Value::None),
                Instruction::LoadName(name) => {
                    if let Some(locals) = locals.as_ref()
                        && let Some(value) = locals.get(&name)
                    {
                        stack.push(value.clone());
                        continue;
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
                Instruction::LessThan => {
                    let right = stack
                        .pop()
                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                    let left = stack
                        .pop()
                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                    let result = left.as_int()? < right.as_int()?;
                    stack.push(Value::Boolean(result));
                }
                Instruction::LoadIndex => {
                    let index_value = stack
                        .pop()
                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                    let object = stack
                        .pop()
                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                    let index_raw = index_value.as_int()?;
                    if index_raw < 0 {
                        bail!("List index must be non-negative, got {index_raw}");
                    }
                    let index = index_raw as usize;
                    let values = object.as_list()?;
                    let value = values.get(index).cloned().ok_or_else(|| {
                        anyhow::anyhow!(
                            "List index out of bounds: index {index}, len {}",
                            values.len()
                        )
                    })?;
                    stack.push(value);
                }
                Instruction::StoreIndex(name) => {
                    let value = stack
                        .pop()
                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                    let index_value = stack
                        .pop()
                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                    let index_raw = index_value.as_int()?;
                    if index_raw < 0 {
                        bail!("List index must be non-negative, got {index_raw}");
                    }
                    let index = index_raw as usize;
                    let target = if let Some(locals) = locals.as_mut() {
                        if locals.contains_key(&name) {
                            locals.get_mut(&name)
                        } else {
                            self.globals.get_mut(&name)
                        }
                    } else {
                        self.globals.get_mut(&name)
                    }
                    .ok_or_else(|| anyhow::anyhow!("Undefined variable '{name}'"))?;
                    let values = target.as_list_mut()?;
                    if index >= values.len() {
                        bail!(
                            "List index out of bounds: index {index}, len {}",
                            values.len()
                        );
                    }
                    values[index] = value;
                }
                Instruction::CallFunction { name, argc } => {
                    if let Some(builtin) = BuiltinFunction::from_name(&name) {
                        match builtin {
                            BuiltinFunction::Print => {
                                let mut values = Vec::with_capacity(argc);
                                for _ in 0..argc {
                                    let value = stack
                                        .pop()
                                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                                    values.push(value.to_output());
                                }
                                values.reverse();
                                self.output.push(values.join(" "));
                                stack.push(Value::None);
                                continue;
                            }
                            BuiltinFunction::Len => {
                                if argc != 1 {
                                    bail!("Function 'len' expected 1 arguments, got {argc}");
                                }
                                let value = stack
                                    .pop()
                                    .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                                let values = value.as_list()?;
                                stack.push(Value::Integer(values.len() as i64));
                                continue;
                            }
                        }
                    }
                    let function = program
                        .functions
                        .get(&name)
                        .ok_or_else(|| anyhow::anyhow!("Undefined function '{name}'"))?
                        .clone();
                    if argc != function.params.len() {
                        bail!(
                            "Function '{name}' expected {} arguments, got {argc}",
                            function.params.len()
                        );
                    }
                    let mut args = Vec::with_capacity(argc);
                    for _ in 0..argc {
                        let value = stack
                            .pop()
                            .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                        args.push(value);
                    }
                    args.reverse();
                    let mut locals_map = HashMap::new();
                    for (param, value) in function.params.iter().zip(args) {
                        locals_map.insert(param.to_string(), value);
                    }
                    let return_value = self.execute_code(
                        &function.code,
                        &mut Vec::new(),
                        Some(&mut locals_map),
                        program,
                    )?;
                    stack.push(return_value);
                }
                Instruction::CallMethod {
                    receiver,
                    method,
                    argc,
                } => {
                    let mut args = Vec::with_capacity(argc);
                    for _ in 0..argc {
                        let value = stack
                            .pop()
                            .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                        args.push(value);
                    }
                    args.reverse();
                    let target = if let Some(locals) = locals.as_mut() {
                        if locals.contains_key(&receiver) {
                            locals.get_mut(&receiver)
                        } else {
                            self.globals.get_mut(&receiver)
                        }
                    } else {
                        self.globals.get_mut(&receiver)
                    }
                    .ok_or_else(|| anyhow::anyhow!("Undefined variable '{receiver}'"))?;
                    match target {
                        Value::List(values) => match method.as_str() {
                            "append" => {
                                if argc != 1 {
                                    bail!("Method 'append' expected 1 arguments, got {argc}");
                                }
                                values.push(args.pop().expect("argc checked above"));
                                stack.push(Value::None);
                            }
                            _ => bail!("Unknown method '{method}' for type list"),
                        },
                        Value::Integer(_) => bail!("Unknown method '{method}' for type int"),
                        Value::Boolean(_) => bail!("Unknown method '{method}' for type bool"),
                        Value::String(_) => bail!("Unknown method '{method}' for type str"),
                        Value::None => bail!("Unknown method '{method}' for type NoneType"),
                    }
                }
                Instruction::JumpIfFalse(target) => {
                    let value = stack
                        .pop()
                        .ok_or_else(|| anyhow::anyhow!("Stack underflow"))?;
                    if !value.is_truthy() {
                        let next_ip = (ip as isize) + target;
                        if next_ip < 0 || (next_ip as usize) > code.len() {
                            bail!("Invalid jump target");
                        }
                        ip = next_ip as usize;
                    }
                }
                Instruction::Jump(target) => {
                    let next_ip = (ip as isize) + target;
                    if next_ip < 0 || (next_ip as usize) > code.len() {
                        bail!("Invalid jump target");
                    }
                    ip = next_ip as usize;
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

    fn prepare(&self, program: &Program) -> Result<Box<dyn PreparedBackend>> {
        Ok(Box::new(PreparedVM {
            compiled: compile(program)?,
        }))
    }
}

impl PreparedBackend for PreparedVM {
    fn run(&self) -> Result<String> {
        let mut vm = VM::new();
        vm.run_compiled(&self.compiled)
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}
