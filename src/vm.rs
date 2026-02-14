use anyhow::Result;
use std::collections::HashMap;
use thiserror::Error;

use crate::ast::Program;
use crate::backend::{Backend, PreparedBackend};
use crate::builtins::BuiltinFunction;
use crate::bytecode::{CompiledProgram, Instruction, compile};

type VmResult<T> = std::result::Result<T, VmError>;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
enum VmError {
    #[error("Stack underflow")]
    StackUnderflow,
    #[error("Expected integer, got {got}")]
    ExpectedIntegerType { got: String },
    #[error("Expected list, got {got}")]
    ExpectedListType { got: String },
    #[error("Undefined variable '{name}'")]
    UndefinedVariable { name: String },
    #[error("Undefined function '{name}'")]
    UndefinedFunction { name: String },
    #[error("Function '{name}' expected {expected} arguments, got {found}")]
    FunctionArityMismatch {
        name: String,
        expected: usize,
        found: usize,
    },
    #[error("List index must be non-negative, got {index}")]
    NegativeListIndex { index: i64 },
    #[error("List index out of bounds: index {index}, len {len}")]
    ListIndexOutOfBounds { index: usize, len: usize },
    #[error("Method '{method}' expected {expected} arguments, got {found}")]
    MethodArityMismatch {
        method: String,
        expected: usize,
        found: usize,
    },
    #[error("Unknown method '{method}' for type {type_name}")]
    UnknownMethod { method: String, type_name: String },
    #[error("Invalid jump target")]
    InvalidJumpTarget,
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    List(Vec<Value>),
    None,
}

impl Value {
    fn as_int(&self) -> VmResult<i64> {
        match self {
            Value::Integer(value) => Ok(*value),
            Value::Boolean(_) | Value::String(_) | Value::List(_) | Value::None => {
                Err(VmError::ExpectedIntegerType {
                    got: format!("{self:?}"),
                })
            }
        }
    }

    fn as_list(&self) -> VmResult<&Vec<Value>> {
        match self {
            Value::List(values) => Ok(values),
            other => Err(VmError::ExpectedListType {
                got: format!("{other:?}"),
            }),
        }
    }

    fn as_list_mut(&mut self) -> VmResult<&mut Vec<Value>> {
        match self {
            Value::List(values) => Ok(values),
            other => Err(VmError::ExpectedListType {
                got: format!("{other:?}"),
            }),
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

    fn run_compiled(&mut self, program: &CompiledProgram) -> VmResult<String> {
        let mut stack = Vec::new();
        self.execute_code(&program.main, &mut stack, None, program)?;
        Ok(self.output.join("\n"))
    }

    fn execute_code(
        &mut self,
        code: &[Instruction],
        stack: &mut Vec<Value>,
        mut locals: Option<&mut HashMap<String, Value>>,
        program: &CompiledProgram,
    ) -> VmResult<Value> {
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
                        let value = Self::pop_stack(stack)?;
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
                        .ok_or_else(|| VmError::UndefinedVariable { name: name.clone() })?;
                    stack.push(value);
                }
                Instruction::StoreName(name) => {
                    let value = Self::pop_stack(stack)?;
                    Self::store_name(&mut locals, &mut self.globals, name, value);
                }
                Instruction::Add => {
                    let right = Self::pop_stack(stack)?;
                    let left = Self::pop_stack(stack)?;
                    let result = left.as_int()? + right.as_int()?;
                    stack.push(Value::Integer(result));
                }
                Instruction::Sub => {
                    let right = Self::pop_stack(stack)?;
                    let left = Self::pop_stack(stack)?;
                    let result = left.as_int()? - right.as_int()?;
                    stack.push(Value::Integer(result));
                }
                Instruction::LessThan => {
                    let right = Self::pop_stack(stack)?;
                    let left = Self::pop_stack(stack)?;
                    let result = left.as_int()? < right.as_int()?;
                    stack.push(Value::Boolean(result));
                }
                Instruction::LoadIndex => {
                    let index_value = Self::pop_stack(stack)?;
                    let object = Self::pop_stack(stack)?;
                    let index_raw = index_value.as_int()?;
                    if index_raw < 0 {
                        return Err(VmError::NegativeListIndex { index: index_raw });
                    }
                    let index = index_raw as usize;
                    let values = object.as_list()?;
                    let value = values.get(index).cloned().ok_or({
                        VmError::ListIndexOutOfBounds {
                            index,
                            len: values.len(),
                        }
                    })?;
                    stack.push(value);
                }
                Instruction::StoreIndex(name) => {
                    let value = Self::pop_stack(stack)?;
                    let index_value = Self::pop_stack(stack)?;
                    let index_raw = index_value.as_int()?;
                    if index_raw < 0 {
                        return Err(VmError::NegativeListIndex { index: index_raw });
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
                    .ok_or_else(|| VmError::UndefinedVariable { name: name.clone() })?;
                    let values = target.as_list_mut()?;
                    if index >= values.len() {
                        return Err(VmError::ListIndexOutOfBounds {
                            index,
                            len: values.len(),
                        });
                    }
                    values[index] = value;
                }
                Instruction::CallFunction { name, argc } => {
                    if let Some(builtin) = BuiltinFunction::from_name(&name) {
                        match builtin {
                            BuiltinFunction::Print => {
                                let mut values = Vec::with_capacity(argc);
                                for _ in 0..argc {
                                    let value = Self::pop_stack(stack)?;
                                    values.push(value.to_output());
                                }
                                values.reverse();
                                self.output.push(values.join(" "));
                                stack.push(Value::None);
                                continue;
                            }
                            BuiltinFunction::Len => {
                                if argc != 1 {
                                    return Err(VmError::FunctionArityMismatch {
                                        name: "len".to_string(),
                                        expected: 1,
                                        found: argc,
                                    });
                                }
                                let value = Self::pop_stack(stack)?;
                                let values = value.as_list()?;
                                stack.push(Value::Integer(values.len() as i64));
                                continue;
                            }
                        }
                    }
                    let function = program
                        .functions
                        .get(&name)
                        .ok_or_else(|| VmError::UndefinedFunction { name: name.clone() })?
                        .clone();
                    if argc != function.params.len() {
                        return Err(VmError::FunctionArityMismatch {
                            name,
                            expected: function.params.len(),
                            found: argc,
                        });
                    }
                    let mut args = Vec::with_capacity(argc);
                    for _ in 0..argc {
                        let value = Self::pop_stack(stack)?;
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
                        let value = Self::pop_stack(stack)?;
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
                    .ok_or_else(|| VmError::UndefinedVariable {
                        name: receiver.clone(),
                    })?;
                    match target {
                        Value::List(values) => match method.as_str() {
                            "append" => {
                                if argc != 1 {
                                    return Err(VmError::MethodArityMismatch {
                                        method: "append".to_string(),
                                        expected: 1,
                                        found: argc,
                                    });
                                }
                                values.push(args.pop().expect("argc checked above"));
                                stack.push(Value::None);
                            }
                            _ => {
                                return Err(VmError::UnknownMethod {
                                    method,
                                    type_name: "list".to_string(),
                                });
                            }
                        },
                        Value::Integer(_) => {
                            return Err(VmError::UnknownMethod {
                                method,
                                type_name: "int".to_string(),
                            });
                        }
                        Value::Boolean(_) => {
                            return Err(VmError::UnknownMethod {
                                method,
                                type_name: "bool".to_string(),
                            });
                        }
                        Value::String(_) => {
                            return Err(VmError::UnknownMethod {
                                method,
                                type_name: "str".to_string(),
                            });
                        }
                        Value::None => {
                            return Err(VmError::UnknownMethod {
                                method,
                                type_name: "NoneType".to_string(),
                            });
                        }
                    }
                }
                Instruction::JumpIfFalse(target) => {
                    let value = Self::pop_stack(stack)?;
                    if !value.is_truthy() {
                        let next_ip = (ip as isize) + target;
                        if next_ip < 0 || (next_ip as usize) > code.len() {
                            return Err(VmError::InvalidJumpTarget);
                        }
                        ip = next_ip as usize;
                    }
                }
                Instruction::Jump(target) => {
                    let next_ip = (ip as isize) + target;
                    if next_ip < 0 || (next_ip as usize) > code.len() {
                        return Err(VmError::InvalidJumpTarget);
                    }
                    ip = next_ip as usize;
                }
                Instruction::Pop => {
                    Self::pop_stack(stack)?;
                }
                Instruction::Return => return Ok(Value::None),
                Instruction::ReturnValue => {
                    let value = Self::pop_stack(stack)?;
                    return Ok(value);
                }
            }
        }
    }

    fn pop_stack(stack: &mut Vec<Value>) -> VmResult<Value> {
        stack.pop().ok_or(VmError::StackUnderflow)
    }

    fn store_name(
        locals: &mut Option<&mut HashMap<String, Value>>,
        globals: &mut HashMap<String, Value>,
        name: String,
        value: Value,
    ) {
        if let Some(locals) = locals.as_mut() {
            locals.insert(name, value);
        } else {
            globals.insert(name, value);
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
        Ok(vm.run_compiled(&self.compiled)?)
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}
