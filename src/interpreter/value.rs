use std::cell::RefCell;
use std::rc::Rc;

use crate::builtins::BuiltinFunction;

use super::InterpreterError;

/// Runtime value model used by the tree-walking interpreter.
#[derive(Debug, Clone, PartialEq)]
pub(super) enum ObjectKind {
    List(Vec<Value>),
}

#[derive(Debug, Clone, PartialEq)]
pub(super) struct Object {
    pub(super) kind: ObjectKind,
}

impl Object {
    fn list(values: Vec<Value>) -> Self {
        Self {
            kind: ObjectKind::List(values),
        }
    }

    fn to_output(&self) -> String {
        match &self.kind {
            ObjectKind::List(values) => {
                let rendered = values
                    .iter()
                    .map(Value::to_output)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{rendered}]")
            }
        }
    }

    fn is_truthy(&self) -> bool {
        match &self.kind {
            ObjectKind::List(values) => !values.is_empty(),
        }
    }

    fn type_name(&self) -> &'static str {
        match &self.kind {
            ObjectKind::List(_) => "list",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(super) enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Object(Rc<RefCell<Object>>),
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
            Value::Object(object) => object.borrow().to_output(),
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
        Value::Object(Rc::new(RefCell::new(Object::list(values))))
    }

    pub(super) fn as_list_object(&self) -> Option<Rc<RefCell<Object>>> {
        match self {
            Value::Object(object) if matches!(object.borrow().kind, ObjectKind::List(_)) => {
                Some(Rc::clone(object))
            }
            _ => None,
        }
    }
}
