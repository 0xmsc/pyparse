use crate::builtins::BuiltinFunction;
use crate::runtime::object::{ObjectKind, ObjectRef, new_list_object};

use super::InterpreterError;

#[derive(Debug, Clone, PartialEq)]
pub(super) enum Value {
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
    pub(super) fn as_int(&self) -> std::result::Result<i64, InterpreterError> {
        match self {
            Value::Integer(value) => Ok(*value),
            Value::Boolean(_)
            | Value::String(_)
            | Value::Object(_)
            | Value::BuiltinFunction(_)
            | Value::Function(_)
            | Value::BoundMethod { .. }
            | Value::None => Err(InterpreterError::ExpectedIntegerType {
                got: format!("{self:?}"),
            }),
        }
    }

    pub(super) fn to_output(&self) -> String {
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

    pub(super) fn is_truthy(&self) -> bool {
        match self {
            Value::Integer(value) => *value != 0,
            Value::Boolean(value) => *value,
            Value::String(value) => !value.is_empty(),
            Value::Object(object) => object.borrow().is_truthy(),
            Value::BuiltinFunction(_) | Value::Function(_) | Value::BoundMethod { .. } => true,
            Value::None => false,
        }
    }

    pub(super) fn type_name(&self) -> &'static str {
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

    pub(super) fn list_object(values: Vec<Value>) -> Self {
        Value::Object(new_list_object(values))
    }
}
