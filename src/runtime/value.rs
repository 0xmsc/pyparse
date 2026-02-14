use crate::builtins::BuiltinFunction;
use crate::runtime::bool::BoolObject;
use crate::runtime::callable::{BoundMethodObject, BuiltinFunctionObject, FunctionObject};
use crate::runtime::int::IntObject;
use crate::runtime::none::NoneObject;
use crate::runtime::object::{BinaryOpError, ObjectRef, ObjectWrapper, new_list_object};
use crate::runtime::string::StringObject;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Object(ObjectRef),
}

impl Value {
    pub(crate) fn to_output(&self) -> String {
        match self {
            Value::Object(object) => object.borrow().to_output(&Value::to_output),
        }
    }

    pub(crate) fn is_truthy(&self) -> bool {
        match self {
            Value::Object(object) => object.borrow().is_truthy(),
        }
    }

    pub(crate) fn list_object(values: Vec<Value>) -> Self {
        Value::Object(new_list_object(values))
    }

    pub(crate) fn int_object(value: i64) -> Self {
        Value::Object(Rc::new(RefCell::new(Box::new(IntObject::new(value)))))
    }

    pub(crate) fn bool_object(value: bool) -> Self {
        Value::Object(Rc::new(RefCell::new(Box::new(BoolObject::new(value)))))
    }

    pub(crate) fn string_object(value: String) -> Self {
        Value::Object(Rc::new(RefCell::new(Box::new(StringObject::new(value)))))
    }

    pub(crate) fn none_object() -> Self {
        Value::Object(Rc::new(RefCell::new(Box::new(NoneObject::new()))))
    }

    pub(crate) fn builtin_function_object(builtin: BuiltinFunction) -> Self {
        Value::Object(Rc::new(RefCell::new(Box::new(BuiltinFunctionObject::new(
            builtin,
        )))))
    }

    pub(crate) fn function_object(name: String) -> Self {
        Value::Object(Rc::new(RefCell::new(Box::new(FunctionObject::new(name)))))
    }

    pub(crate) fn bound_method_object(receiver: ObjectRef, method: String) -> Self {
        Value::Object(Rc::new(RefCell::new(Box::new(BoundMethodObject::new(
            receiver, method,
        )))))
    }

    pub(crate) fn add(&self, rhs: &Value) -> Result<Value, BinaryOpError> {
        match self {
            Value::Object(object) => ObjectWrapper::new(object.clone()).add(rhs),
        }
    }

    pub(crate) fn sub(&self, rhs: &Value) -> Result<Value, BinaryOpError> {
        match self {
            Value::Object(object) => ObjectWrapper::new(object.clone()).sub(rhs),
        }
    }

    pub(crate) fn lt(&self, rhs: &Value) -> Result<Value, BinaryOpError> {
        match self {
            Value::Object(object) => ObjectWrapper::new(object.clone()).lt(rhs),
        }
    }

    pub(crate) fn as_i64(&self) -> Option<i64> {
        match self {
            Value::Object(object) => ObjectWrapper::new(object.clone()).as_i64(),
        }
    }
}
