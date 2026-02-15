use crate::builtins::BuiltinFunction;
use crate::runtime::bool::{self, BoolObject};
use crate::runtime::callable::{BoundMethodObject, BuiltinFunctionObject, FunctionObject};
use crate::runtime::error::RuntimeError;
use crate::runtime::int::{self, IntObject};
use crate::runtime::none::NoneObject;
use crate::runtime::object::{BoundMethodCallable, CallTarget, ObjectRef, new_list_object};
use crate::runtime::string::{self, StringObject};
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub(crate) struct Value {
    object: ObjectRef,
    call_target: Option<CallTarget>,
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Value({})", self.type_name())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CallTargetError {
    pub(crate) type_name: String,
}

impl Value {
    fn new(object: ObjectRef, call_target: Option<CallTarget>) -> Self {
        Self {
            object,
            call_target,
        }
    }

    pub(crate) fn object_ref(&self) -> ObjectRef {
        self.object.clone()
    }

    pub(crate) fn type_name(&self) -> &'static str {
        self.object.borrow().type_name()
    }

    pub(crate) fn to_output(&self) -> String {
        if let Some(str_value) = self.try_call_magic_method("__str__", "__str__") {
            return string::downcast_string(&str_value).expect("__str__ must return str");
        }

        if let Some(repr_value) = self.try_call_magic_method("__repr__", "__repr__") {
            return string::downcast_string(&repr_value).expect("__repr__ must return str");
        }

        panic!("missing __str__ and __repr__ for {}", self.type_name());
    }

    pub(crate) fn is_truthy(&self) -> bool {
        if let Some(bool_value) = self.try_call_magic_method("__bool__", "__bool__") {
            return bool::downcast_bool(&bool_value).expect("__bool__ must return bool");
        }

        if let Some(length_value) = self.try_call_magic_method("__len__", "__len__") {
            return int::downcast_i64(&length_value).expect("__len__ must return int") != 0;
        }

        true
    }

    pub(crate) fn call_target(&self) -> Result<CallTarget, CallTargetError> {
        self.call_target.clone().ok_or_else(|| CallTargetError {
            type_name: self.type_name().to_string(),
        })
    }

    pub(crate) fn get_attribute(&self, attribute: &str) -> Result<Value, RuntimeError> {
        self.object
            .borrow()
            .get_attribute(self.object_ref(), attribute)
    }

    pub(crate) fn get_item(&self, index: Value) -> Result<Value, RuntimeError> {
        let callee = self
            .get_attribute("__getitem__")
            .map_err(|error| map_unknown_attribute_to_unsupported(error, "__getitem__"))?;
        self.call_magic_method(callee, vec![index], "__getitem__")
    }

    pub(crate) fn set_item(&self, index: Value, value: Value) -> Result<(), RuntimeError> {
        let callee = self
            .get_attribute("__setitem__")
            .map_err(|error| map_unknown_attribute_to_unsupported(error, "__setitem__"))?;
        self.call_magic_method(callee, vec![index, value], "__setitem__")?;
        Ok(())
    }

    pub(crate) fn len(&self) -> Result<Value, RuntimeError> {
        let callee = self
            .get_attribute("__len__")
            .map_err(|error| map_unknown_attribute_to_unsupported(error, "len"))?;
        self.call_magic_method(callee, Vec::new(), "len")
    }

    pub(crate) fn list_object(values: Vec<Value>) -> Self {
        Self::new(new_list_object(values), None)
    }

    pub(crate) fn int_object(value: i64) -> Self {
        Self::new(Rc::new(RefCell::new(Box::new(IntObject::new(value)))), None)
    }

    pub(crate) fn bool_object(value: bool) -> Self {
        Self::new(
            Rc::new(RefCell::new(Box::new(BoolObject::new(value)))),
            None,
        )
    }

    pub(crate) fn string_object(value: String) -> Self {
        Self::new(
            Rc::new(RefCell::new(Box::new(StringObject::new(value)))),
            None,
        )
    }

    pub(crate) fn none_object() -> Self {
        Self::new(Rc::new(RefCell::new(Box::new(NoneObject::new()))), None)
    }

    pub(crate) fn builtin_function_object(builtin: BuiltinFunction) -> Self {
        let builtin_object = BuiltinFunctionObject::new(builtin);
        let call_target = builtin_object.call_target();
        Self::new(
            Rc::new(RefCell::new(Box::new(builtin_object))),
            Some(call_target),
        )
    }

    pub(crate) fn function_object(name: String) -> Self {
        let function_object = FunctionObject::new(name);
        let call_target = function_object.call_target();
        Self::new(
            Rc::new(RefCell::new(Box::new(function_object))),
            Some(call_target),
        )
    }

    pub(crate) fn bound_method_object(callable: BoundMethodCallable) -> Self {
        let bound_method_object = BoundMethodObject::new(callable);
        let call_target = bound_method_object.call_target();
        Self::new(
            Rc::new(RefCell::new(Box::new(bound_method_object))),
            Some(call_target),
        )
    }

    fn call_magic_method(
        &self,
        callee: Value,
        args: Vec<Value>,
        operation: &str,
    ) -> Result<Value, RuntimeError> {
        let type_name = callee.type_name().to_string();
        let call_target = callee
            .call_target()
            .map_err(|_| RuntimeError::UnsupportedOperation {
                operation: operation.to_string(),
                type_name: type_name.clone(),
            })?;
        match call_target {
            CallTarget::BoundMethod(callable) => callable(args),
            _ => Err(RuntimeError::UnsupportedOperation {
                operation: operation.to_string(),
                type_name,
            }),
        }
    }

    fn try_call_magic_method(&self, attribute: &str, operation: &str) -> Option<Value> {
        let callee = match self.get_attribute(attribute) {
            Ok(callee) => callee,
            Err(RuntimeError::UnknownAttribute { .. }) => return None,
            Err(error) => panic!("failed to resolve {attribute}: {error}"),
        };
        Some(
            self.call_magic_method(callee, Vec::new(), operation)
                .unwrap_or_else(|error| panic!("failed to call {attribute}: {error}")),
        )
    }
}

fn map_unknown_attribute_to_unsupported(error: RuntimeError, operation: &str) -> RuntimeError {
    match error {
        RuntimeError::UnknownAttribute {
            attribute: _,
            type_name,
        } => RuntimeError::UnsupportedOperation {
            operation: operation.to_string(),
            type_name,
        },
        other => other,
    }
}
