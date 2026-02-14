use crate::builtins::BuiltinFunction;
use crate::runtime::int::IntObject;
use crate::runtime::object::{BinaryOpError, ObjectRef, ObjectWrapper, new_list_object};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Boolean(bool),
    String(String),
    Object(ObjectRef),
    BuiltinFunction(BuiltinFunction),
    Function(String),
    BoundMethod {
        receiver: Box<Value>,
        method: String,
    },
    None,
}

impl Value {
    pub(crate) fn to_output(&self) -> String {
        match self {
            Value::Boolean(value) => {
                if *value {
                    "True".to_string()
                } else {
                    "False".to_string()
                }
            }
            Value::String(value) => value.clone(),
            Value::Object(object) => object.borrow().to_output(&Value::to_output),
            Value::BuiltinFunction(_) => "<built-in function>".to_string(),
            Value::Function(name) => format!("<function {name}>"),
            Value::BoundMethod { .. } => "<bound method>".to_string(),
            Value::None => "None".to_string(),
        }
    }

    pub(crate) fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(value) => *value,
            Value::String(value) => !value.is_empty(),
            Value::Object(object) => object.borrow().is_truthy(),
            Value::BuiltinFunction(_) | Value::Function(_) | Value::BoundMethod { .. } => true,
            Value::None => false,
        }
    }

    pub(crate) fn type_name(&self) -> &'static str {
        match self {
            Value::Boolean(_) => "bool",
            Value::String(_) => "str",
            Value::Object(object) => object.borrow().type_name(),
            Value::BuiltinFunction(_) => "builtin_function_or_method",
            Value::Function(_) => "function",
            Value::BoundMethod { .. } => "method",
            Value::None => "NoneType",
        }
    }

    pub(crate) fn list_object(values: Vec<Value>) -> Self {
        Value::Object(new_list_object(values))
    }

    pub(crate) fn int_object(value: i64) -> Self {
        Value::Object(Rc::new(RefCell::new(Box::new(IntObject::new(value)))))
    }

    pub(crate) fn add(&self, rhs: &Value) -> Result<Value, BinaryOpError> {
        match self {
            Value::Object(object) => ObjectWrapper::new(object.clone()).add(rhs),
            _ => Err(BinaryOpError::ExpectedIntegerType {
                got: format!("{self:?}"),
            }),
        }
    }

    pub(crate) fn sub(&self, rhs: &Value) -> Result<Value, BinaryOpError> {
        match self {
            Value::Object(object) => ObjectWrapper::new(object.clone()).sub(rhs),
            _ => Err(BinaryOpError::ExpectedIntegerType {
                got: format!("{self:?}"),
            }),
        }
    }

    pub(crate) fn lt(&self, rhs: &Value) -> Result<Value, BinaryOpError> {
        match self {
            Value::Object(object) => ObjectWrapper::new(object.clone()).lt(rhs),
            _ => Err(BinaryOpError::ExpectedIntegerType {
                got: format!("{self:?}"),
            }),
        }
    }

    pub(crate) fn as_i64(&self) -> Option<i64> {
        match self {
            Value::Object(object) => ObjectWrapper::new(object.clone()).as_i64(),
            _ => None,
        }
    }
}
