use anyhow::Result;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
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
    #[error("Unknown attribute '{attribute}' for type {type_name}")]
    UnknownAttribute {
        attribute: String,
        type_name: String,
    },
    #[error("Object of type {type_name} is not callable")]
    ObjectNotCallable { type_name: String },
    #[error("Invalid jump target")]
    InvalidJumpTarget,
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    List(Rc<RefCell<Vec<Value>>>),
    BuiltinFunction(BuiltinFunction),
    Function(String),
    BoundMethod {
        receiver: Box<Value>,
        method: String,
    },
    None,
}

impl Value {
    fn as_int(&self) -> VmResult<i64> {
        match self {
            Value::Integer(value) => Ok(*value),
            Value::Boolean(_)
            | Value::String(_)
            | Value::List(_)
            | Value::BuiltinFunction(_)
            | Value::Function(_)
            | Value::BoundMethod { .. }
            | Value::None => Err(VmError::ExpectedIntegerType {
                got: format!("{self:?}"),
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
                    .borrow()
                    .iter()
                    .map(Value::to_output)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{rendered}]")
            }
            Value::BuiltinFunction(_) => "<built-in function>".to_string(),
            Value::Function(name) => format!("<function {name}>"),
            Value::BoundMethod { .. } => "<bound method>".to_string(),
            Value::None => "None".to_string(),
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            Value::Integer(value) => *value != 0,
            Value::Boolean(value) => *value,
            Value::String(value) => !value.is_empty(),
            Value::List(values) => !values.borrow().is_empty(),
            Value::BuiltinFunction(_) | Value::Function(_) | Value::BoundMethod { .. } => true,
            Value::None => false,
        }
    }

    fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "int",
            Value::Boolean(_) => "bool",
            Value::String(_) => "str",
            Value::List(_) => "list",
            Value::BuiltinFunction(_) => "builtin_function_or_method",
            Value::Function(_) => "function",
            Value::BoundMethod { .. } => "method",
            Value::None => "NoneType",
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

struct Environment<'a> {
    globals: &'a mut HashMap<String, Value>,
    locals: Option<&'a mut HashMap<String, Value>>,
}

impl<'a> Environment<'a> {
    fn top_level(globals: &'a mut HashMap<String, Value>) -> Self {
        Self {
            globals,
            locals: None,
        }
    }

    fn with_locals(
        globals: &'a mut HashMap<String, Value>,
        locals: &'a mut HashMap<String, Value>,
    ) -> Self {
        Self {
            globals,
            locals: Some(locals),
        }
    }

    fn load(&self, name: &str) -> Option<&Value> {
        if let Some(locals) = self.locals.as_deref()
            && let Some(value) = locals.get(name)
        {
            return Some(value);
        }
        self.globals.get(name)
    }

    fn load_mut(&mut self, name: &str) -> Option<&mut Value> {
        if let Some(locals) = self.locals.as_deref_mut()
            && locals.contains_key(name)
        {
            return locals.get_mut(name);
        }
        self.globals.get_mut(name)
    }

    fn store(&mut self, name: String, value: Value) {
        if let Some(locals) = self.locals.as_deref_mut() {
            locals.insert(name, value);
        } else {
            self.globals.insert(name, value);
        }
    }

    fn child_with_locals<'b>(
        &'b mut self,
        locals: &'b mut HashMap<String, Value>,
    ) -> Environment<'b> {
        Environment::with_locals(self.globals, locals)
    }
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
        let mut environment = Environment::top_level(&mut self.globals);
        Self::execute_code(
            &program.main,
            &mut stack,
            &mut environment,
            &mut self.output,
            program,
        )?;
        Ok(self.output.join("\n"))
    }

    fn execute_code(
        code: &[Instruction],
        stack: &mut Vec<Value>,
        environment: &mut Environment<'_>,
        output: &mut Vec<String>,
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
                    stack.push(Value::List(Rc::new(RefCell::new(values))));
                }
                Instruction::PushNone => stack.push(Value::None),
                Instruction::LoadName(name) => {
                    let value = if let Some(value) = environment.load(&name).cloned() {
                        value
                    } else if let Some(builtin) = BuiltinFunction::from_name(&name) {
                        Value::BuiltinFunction(builtin)
                    } else if program.functions.contains_key(&name) {
                        Value::Function(name.clone())
                    } else {
                        return Err(VmError::UndefinedVariable { name: name.clone() });
                    };
                    stack.push(value);
                }
                Instruction::StoreName(name) => {
                    let value = Self::pop_stack(stack)?;
                    environment.store(name, value);
                }
                Instruction::LoadAttr(attribute) => {
                    let object = Self::pop_stack(stack)?;
                    let value = Self::load_attribute(object, attribute)?;
                    stack.push(value);
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
                    let values = match object {
                        Value::List(values) => values,
                        other => {
                            return Err(VmError::ExpectedListType {
                                got: format!("{other:?}"),
                            });
                        }
                    };
                    let borrowed = values.borrow();
                    let value =
                        borrowed
                            .get(index)
                            .cloned()
                            .ok_or(VmError::ListIndexOutOfBounds {
                                index,
                                len: borrowed.len(),
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
                    let target = environment
                        .load_mut(&name)
                        .ok_or_else(|| VmError::UndefinedVariable { name: name.clone() })?;
                    let values = match target {
                        Value::List(values) => values,
                        other => {
                            return Err(VmError::ExpectedListType {
                                got: format!("{other:?}"),
                            });
                        }
                    };
                    let mut borrowed = values.borrow_mut();
                    if index >= borrowed.len() {
                        return Err(VmError::ListIndexOutOfBounds {
                            index,
                            len: borrowed.len(),
                        });
                    }
                    borrowed[index] = value;
                }
                Instruction::Call { argc } => {
                    let mut args = Vec::with_capacity(argc);
                    for _ in 0..argc {
                        let value = Self::pop_stack(stack)?;
                        args.push(value);
                    }
                    args.reverse();
                    let callee = Self::pop_stack(stack)?;
                    let value = Self::call_value(callee, args, environment, output, program)?;
                    stack.push(value);
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

    fn load_attribute(object: Value, attribute: String) -> VmResult<Value> {
        match object {
            Value::List(_) if attribute == "append" => Ok(Value::BoundMethod {
                receiver: Box::new(object),
                method: attribute,
            }),
            other => Err(VmError::UnknownAttribute {
                attribute,
                type_name: other.type_name().to_string(),
            }),
        }
    }

    fn call_value(
        callee: Value,
        args: Vec<Value>,
        environment: &mut Environment<'_>,
        output: &mut Vec<String>,
        program: &CompiledProgram,
    ) -> VmResult<Value> {
        match callee {
            Value::BuiltinFunction(BuiltinFunction::Print) => {
                let rendered = args.iter().map(Value::to_output).collect::<Vec<_>>();
                output.push(rendered.join(" "));
                Ok(Value::None)
            }
            Value::BuiltinFunction(BuiltinFunction::Len) => {
                if args.len() != 1 {
                    return Err(VmError::FunctionArityMismatch {
                        name: "len".to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }
                match &args[0] {
                    Value::List(values) => Ok(Value::Integer(values.borrow().len() as i64)),
                    other => Err(VmError::ExpectedListType {
                        got: format!("{other:?}"),
                    }),
                }
            }
            Value::Function(name) => {
                let function = program
                    .functions
                    .get(&name)
                    .ok_or_else(|| VmError::UndefinedFunction { name: name.clone() })?
                    .clone();
                if args.len() != function.params.len() {
                    return Err(VmError::FunctionArityMismatch {
                        name,
                        expected: function.params.len(),
                        found: args.len(),
                    });
                }
                let mut locals_map = HashMap::new();
                for (param, value) in function.params.iter().zip(args) {
                    locals_map.insert(param.to_string(), value);
                }
                let mut child_environment = environment.child_with_locals(&mut locals_map);
                Self::execute_code(
                    &function.code,
                    &mut Vec::new(),
                    &mut child_environment,
                    output,
                    program,
                )
            }
            Value::BoundMethod { receiver, method } => {
                Self::call_bound_method(*receiver, method, args)
            }
            other => Err(VmError::ObjectNotCallable {
                type_name: other.type_name().to_string(),
            }),
        }
    }

    fn call_bound_method(receiver: Value, method: String, mut args: Vec<Value>) -> VmResult<Value> {
        match receiver {
            Value::List(values) => match method.as_str() {
                "append" => {
                    if args.len() != 1 {
                        return Err(VmError::MethodArityMismatch {
                            method: "append".to_string(),
                            expected: 1,
                            found: args.len(),
                        });
                    }
                    values
                        .borrow_mut()
                        .push(args.pop().expect("len checked above"));
                    Ok(Value::None)
                }
                _ => Err(VmError::UnknownMethod {
                    method,
                    type_name: "list".to_string(),
                }),
            },
            other => Err(VmError::UnknownMethod {
                method,
                type_name: other.type_name().to_string(),
            }),
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

#[cfg(test)]
mod tests {
    use super::{VM, VmError};
    use crate::ast::{AssignTarget, Expression, Program, Statement};
    use crate::bytecode::{CompiledFunction, CompiledProgram, Instruction, compile};
    use indoc::indoc;
    use std::collections::HashMap;

    fn call(name: &str, args: Vec<Expression>) -> Expression {
        Expression::Call {
            callee: Box::new(Expression::Identifier(name.to_string())),
            args,
        }
    }

    fn method_call(receiver: &str, method: &str, args: Vec<Expression>) -> Expression {
        Expression::Call {
            callee: Box::new(Expression::Attribute {
                object: Box::new(Expression::Identifier(receiver.to_string())),
                name: method.to_string(),
            }),
            args,
        }
    }

    #[test]
    fn run_compiled_reports_stack_underflow() {
        let compiled = CompiledProgram {
            functions: HashMap::<String, CompiledFunction>::new(),
            main: vec![Instruction::Pop],
        };

        let mut vm = VM::new();
        let error = vm
            .run_compiled(&compiled)
            .expect_err("expected stack underflow");
        assert_eq!(error, VmError::StackUnderflow);
    }

    #[test]
    fn run_compiled_reports_invalid_jump_target() {
        let compiled = CompiledProgram {
            functions: HashMap::<String, CompiledFunction>::new(),
            main: vec![Instruction::Jump(10)],
        };

        let mut vm = VM::new();
        let error = vm
            .run_compiled(&compiled)
            .expect_err("expected invalid jump target");
        assert_eq!(error, VmError::InvalidJumpTarget);
    }

    #[test]
    fn function_locals_shadow_globals() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("x".to_string()),
                    value: Expression::Integer(1),
                },
                Statement::FunctionDef {
                    name: "f".to_string(),
                    params: vec!["x".to_string()],
                    body: vec![Statement::Return(Some(Expression::Identifier(
                        "x".to_string(),
                    )))],
                },
                Statement::Expr(call("print", vec![call("f", vec![Expression::Integer(2)])])),
                Statement::Expr(call("print", vec![Expression::Identifier("x".to_string())])),
            ],
        };

        let compiled = compile(&program).expect("compile should succeed");
        let mut vm = VM::new();
        let output = vm.run_compiled(&compiled).expect("run should succeed");
        assert_eq!(
            output,
            indoc! {"
                2
                1
            "}
            .trim_end()
        );
    }

    #[test]
    fn list_append_mutates_receiver_and_returns_none() {
        let program = Program {
            statements: vec![
                Statement::Assign {
                    target: AssignTarget::Name("values".to_string()),
                    value: Expression::List(vec![]),
                },
                Statement::Expr(call(
                    "print",
                    vec![method_call(
                        "values",
                        "append",
                        vec![Expression::Integer(3)],
                    )],
                )),
                Statement::Expr(call(
                    "print",
                    vec![Expression::Identifier("values".to_string())],
                )),
            ],
        };

        let compiled = compile(&program).expect("compile should succeed");
        let mut vm = VM::new();
        let output = vm.run_compiled(&compiled).expect("run should succeed");
        assert_eq!(
            output,
            indoc! {"
                None
                [3]
            "}
            .trim_end()
        );
    }
}
