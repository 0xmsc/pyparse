use anyhow::Result;
use std::collections::HashMap;
use thiserror::Error;

use crate::ast::Program;
use crate::backend::{Backend, PreparedBackend};
use crate::builtins::BuiltinFunction;
use crate::bytecode::{CompiledProgram, Instruction, compile};
use crate::runtime::list::ListError;
use crate::runtime::object::{
    AttributeError, MethodError, ObjectKind, ObjectRef, ObjectWrapper, new_list_object,
};

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
    Object(ObjectRef<Value>),
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
            | Value::Object(_)
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
            Value::Object(object) => {
                let borrowed = object.borrow();
                match &borrowed.kind {
                    ObjectKind::List(list) => {
                        let rendered = list
                            .iter()
                            .map(Value::to_output)
                            .collect::<Vec<_>>()
                            .join(", ");
                        format!("[{rendered}]")
                    }
                }
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
            Value::Object(object) => object.borrow().is_truthy(),
            Value::BuiltinFunction(_) | Value::Function(_) | Value::BoundMethod { .. } => true,
            Value::None => false,
        }
    }

    fn type_name(&self) -> &'static str {
        match self {
            Value::Integer(_) => "int",
            Value::Boolean(_) => "bool",
            Value::String(_) => "str",
            Value::Object(object) => object.borrow().type_name(),
            Value::BuiltinFunction(_) => "builtin_function_or_method",
            Value::Function(_) => "function",
            Value::BoundMethod { .. } => "method",
            Value::None => "NoneType",
        }
    }

    fn list_object(values: Vec<Value>) -> Self {
        Value::Object(new_list_object(values))
    }
}

pub struct VM {
    globals: HashMap<String, Value>,
}

pub struct PreparedVM {
    compiled: CompiledProgram,
}

struct VmRuntime<'a> {
    program: &'a CompiledProgram,
    output: Vec<String>,
    stack: Vec<Value>,
}

impl<'a> VmRuntime<'a> {
    fn new(program: &'a CompiledProgram) -> Self {
        Self {
            program,
            output: Vec::new(),
            stack: Vec::new(),
        }
    }

    fn output_string(self) -> String {
        self.output.join("\n")
    }
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
        }
    }

    fn run_compiled(&mut self, program: &CompiledProgram) -> VmResult<String> {
        let mut runtime = VmRuntime::new(program);
        let mut environment = Environment::top_level(&mut self.globals);
        runtime.execute_code(&program.main, &mut environment)?;
        Ok(runtime.output_string())
    }
}

impl VmRuntime<'_> {
    fn execute_code(
        &mut self,
        code: &[Instruction],
        environment: &mut Environment<'_>,
    ) -> VmResult<Value> {
        let mut ip = 0;
        loop {
            let instruction = match code.get(ip) {
                Some(instruction) => instruction.clone(),
                None => return Ok(Value::None),
            };
            ip += 1;
            match instruction {
                Instruction::PushInt(value) => self.stack.push(Value::Integer(value)),
                Instruction::PushBool(value) => self.stack.push(Value::Boolean(value)),
                Instruction::PushString(value) => self.stack.push(Value::String(value)),
                Instruction::BuildList(count) => {
                    let mut values = Vec::with_capacity(count);
                    for _ in 0..count {
                        let value = self.pop_stack()?;
                        values.push(value);
                    }
                    values.reverse();
                    self.stack.push(Value::list_object(values));
                }
                Instruction::PushNone => self.stack.push(Value::None),
                Instruction::LoadName(name) => {
                    let value = if let Some(value) = environment.load(&name).cloned() {
                        value
                    } else if let Some(builtin) = BuiltinFunction::from_name(&name) {
                        Value::BuiltinFunction(builtin)
                    } else if self.program.functions.contains_key(&name) {
                        Value::Function(name.clone())
                    } else {
                        return Err(VmError::UndefinedVariable { name: name.clone() });
                    };
                    self.stack.push(value);
                }
                Instruction::StoreName(name) => {
                    let value = self.pop_stack()?;
                    environment.store(name, value);
                }
                Instruction::LoadAttr(attribute) => {
                    let object = self.pop_stack()?;
                    let value = self.load_attribute(object, attribute)?;
                    self.stack.push(value);
                }
                Instruction::Add => {
                    let right = self.pop_stack()?;
                    let left = self.pop_stack()?;
                    let result = left.as_int()? + right.as_int()?;
                    self.stack.push(Value::Integer(result));
                }
                Instruction::Sub => {
                    let right = self.pop_stack()?;
                    let left = self.pop_stack()?;
                    let result = left.as_int()? - right.as_int()?;
                    self.stack.push(Value::Integer(result));
                }
                Instruction::LessThan => {
                    let right = self.pop_stack()?;
                    let left = self.pop_stack()?;
                    let result = left.as_int()? < right.as_int()?;
                    self.stack.push(Value::Boolean(result));
                }
                Instruction::LoadIndex => {
                    let index_value = self.pop_stack()?;
                    let object = self.pop_stack()?;
                    let index_raw = index_value.as_int()?;
                    let object = match object {
                        Value::Object(object) => object,
                        other => {
                            return Err(VmError::ExpectedListType {
                                got: format!("{other:?}"),
                            });
                        }
                    };
                    let value = ObjectWrapper::new(object)
                        .get_item(index_raw)
                        .map_err(Self::map_list_error)?;
                    self.stack.push(value);
                }
                Instruction::StoreIndex(name) => {
                    let value = self.pop_stack()?;
                    let index_value = self.pop_stack()?;
                    let index_raw = index_value.as_int()?;
                    let target = environment
                        .load_mut(&name)
                        .ok_or_else(|| VmError::UndefinedVariable { name: name.clone() })?;
                    let object = match target {
                        Value::Object(object) => object.clone(),
                        other => {
                            return Err(VmError::ExpectedListType {
                                got: format!("{other:?}"),
                            });
                        }
                    };
                    ObjectWrapper::new(object)
                        .set_item(index_raw, value)
                        .map_err(Self::map_list_error)?;
                }
                Instruction::Call { argc } => {
                    let mut args = Vec::with_capacity(argc);
                    for _ in 0..argc {
                        let value = self.pop_stack()?;
                        args.push(value);
                    }
                    args.reverse();
                    let callee = self.pop_stack()?;
                    let value = self.call_value(callee, args, environment)?;
                    self.stack.push(value);
                }
                Instruction::JumpIfFalse(target) => {
                    let value = self.pop_stack()?;
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
                    self.pop_stack()?;
                }
                Instruction::Return => return Ok(Value::None),
                Instruction::ReturnValue => {
                    let value = self.pop_stack()?;
                    return Ok(value);
                }
            }
        }
    }

    fn pop_stack(&mut self) -> VmResult<Value> {
        self.stack.pop().ok_or(VmError::StackUnderflow)
    }

    fn load_attribute(&self, object: Value, attribute: String) -> VmResult<Value> {
        if let Value::Object(object_ref) = &object {
            let method = ObjectWrapper::new(object_ref.clone())
                .get_attribute_method_name(&attribute)
                .map_err(Self::map_attribute_error)?;
            return Ok(Value::BoundMethod {
                receiver: Box::new(object),
                method,
            });
        }
        Err(VmError::UnknownAttribute {
            attribute,
            type_name: object.type_name().to_string(),
        })
    }

    fn call_value(
        &mut self,
        callee: Value,
        args: Vec<Value>,
        environment: &mut Environment<'_>,
    ) -> VmResult<Value> {
        match callee {
            Value::BuiltinFunction(BuiltinFunction::Print) => {
                let rendered = args.iter().map(Value::to_output).collect::<Vec<_>>();
                self.output.push(rendered.join(" "));
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
                    Value::Object(object) => Ok(Value::Integer(
                        ObjectWrapper::new(object.clone()).len() as i64,
                    )),
                    other => Err(VmError::ExpectedListType {
                        got: format!("{other:?}"),
                    }),
                }
            }
            Value::Function(name) => {
                let function = self
                    .program
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
                let parent_stack = std::mem::take(&mut self.stack);
                let result = self.execute_code(&function.code, &mut child_environment);
                self.stack = parent_stack;
                result
            }
            Value::BoundMethod { receiver, method } => match *receiver {
                Value::Object(object) => {
                    ObjectWrapper::new(object.clone())
                        .call_method(&method, args)
                        .map_err(Self::map_method_error)?;
                    Ok(Value::None)
                }
                other => Err(VmError::UnknownMethod {
                    method,
                    type_name: other.type_name().to_string(),
                }),
            },
            other => Err(VmError::ObjectNotCallable {
                type_name: other.type_name().to_string(),
            }),
        }
    }

    fn map_list_error(error: ListError) -> VmError {
        match error {
            ListError::NegativeIndex { index } => VmError::NegativeListIndex { index },
            ListError::OutOfBounds { index, len } => VmError::ListIndexOutOfBounds { index, len },
        }
    }

    fn map_attribute_error(error: AttributeError) -> VmError {
        match error {
            AttributeError::UnknownAttribute {
                attribute,
                type_name,
            } => VmError::UnknownAttribute {
                attribute,
                type_name,
            },
        }
    }

    fn map_method_error(error: MethodError) -> VmError {
        match error {
            MethodError::ArityMismatch {
                method,
                expected,
                found,
            } => VmError::MethodArityMismatch {
                method,
                expected,
                found,
            },
            MethodError::UnknownMethod { method, type_name } => {
                VmError::UnknownMethod { method, type_name }
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
