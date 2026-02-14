use crate::builtins::BuiltinFunction;
use crate::runtime::bool::BoolObject;
use crate::runtime::callable::{BoundMethodObject, BuiltinFunctionObject, FunctionObject};
use crate::runtime::int::IntObject;
use crate::runtime::list::ListError;
use crate::runtime::none::NoneObject;
use crate::runtime::object::{
    AttributeError, BinaryOpError, CallTarget, MethodError, ObjectRef, new_list_object,
};
use crate::runtime::string::StringObject;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub(crate) struct Value(ObjectRef);

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CallTargetError {
    pub(crate) type_name: String,
}

impl Value {
    pub(crate) fn from_object(object: ObjectRef) -> Self {
        Self(object)
    }

    pub(crate) fn object_ref(&self) -> ObjectRef {
        self.0.clone()
    }

    pub(crate) fn to_output(&self) -> String {
        self.0.borrow().to_output(&Value::to_output)
    }

    pub(crate) fn is_truthy(&self) -> bool {
        self.0.borrow().is_truthy()
    }

    pub(crate) fn call_target(&self) -> Result<CallTarget, CallTargetError> {
        self.0
            .borrow()
            .call_target()
            .ok_or_else(|| CallTargetError {
                type_name: self.0.borrow().type_name().to_string(),
            })
    }

    pub(crate) fn get_attribute_method_name(
        &self,
        attribute: &str,
    ) -> Result<String, AttributeError> {
        self.0.borrow().get_attribute_method_name(attribute)
    }

    pub(crate) fn len(&self) -> Result<usize, ListError> {
        self.0.borrow().len()
    }

    pub(crate) fn get_item(&self, index: Value) -> Result<Value, ListError> {
        self.0.borrow().get_item(index)
    }

    pub(crate) fn set_item(&self, index: Value, value: Value) -> Result<(), ListError> {
        self.0.borrow_mut().set_item(index, value)
    }

    pub(crate) fn call_method(&self, method: &str, args: Vec<Value>) -> Result<(), MethodError> {
        self.0.borrow_mut().call_method(method, args)
    }

    pub(crate) fn list_object(values: Vec<Value>) -> Self {
        Self(new_list_object(values))
    }

    pub(crate) fn int_object(value: i64) -> Self {
        Self(Rc::new(RefCell::new(Box::new(IntObject::new(value)))))
    }

    pub(crate) fn bool_object(value: bool) -> Self {
        Self(Rc::new(RefCell::new(Box::new(BoolObject::new(value)))))
    }

    pub(crate) fn string_object(value: String) -> Self {
        Self(Rc::new(RefCell::new(Box::new(StringObject::new(value)))))
    }

    pub(crate) fn none_object() -> Self {
        Self(Rc::new(RefCell::new(Box::new(NoneObject::new()))))
    }

    pub(crate) fn builtin_function_object(builtin: BuiltinFunction) -> Self {
        Self(Rc::new(RefCell::new(Box::new(BuiltinFunctionObject::new(
            builtin,
        )))))
    }

    pub(crate) fn function_object(name: String) -> Self {
        Self(Rc::new(RefCell::new(Box::new(FunctionObject::new(name)))))
    }

    pub(crate) fn bound_method_object(receiver: ObjectRef, method: String) -> Self {
        Self(Rc::new(RefCell::new(Box::new(BoundMethodObject::new(
            receiver, method,
        )))))
    }

    pub(crate) fn add(&self, rhs: &Value) -> Result<Value, BinaryOpError> {
        self.0.borrow().add(rhs)
    }

    pub(crate) fn sub(&self, rhs: &Value) -> Result<Value, BinaryOpError> {
        self.0.borrow().sub(rhs)
    }

    pub(crate) fn lt(&self, rhs: &Value) -> Result<Value, BinaryOpError> {
        self.0.borrow().lt(rhs)
    }
}
