use crate::runtime::error::RuntimeError;
use crate::runtime::method::{zero_arg_string_method, zero_arg_value_method};
use crate::runtime::object::{ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct NoneObject;

impl NoneObject {
    pub(crate) fn new() -> Self {
        Self
    }
}

impl RuntimeObject for NoneObject {
    fn type_name(&self) -> &'static str {
        "NoneType"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        if attribute == "__bool__" {
            return Ok(zero_arg_value_method("__bool__", || {
                Value::bool_object(false)
            }));
        }
        if attribute == "__str__" || attribute == "__repr__" {
            return Ok(zero_arg_string_method(attribute, "None"));
        }
        Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "NoneType".to_string(),
        })
    }
}
