use crate::runtime::error::RuntimeError;
use crate::runtime::method::{zero_arg_string_method, zero_arg_value_method};
use crate::runtime::object::{ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BoolObject {
    value: bool,
}

impl BoolObject {
    pub(crate) fn new(value: bool) -> Self {
        Self { value }
    }

    pub(crate) fn value(&self) -> bool {
        self.value
    }
}

impl RuntimeObject for BoolObject {
    fn type_name(&self) -> &'static str {
        "bool"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        if attribute == "__bool__" {
            let value = self.value;
            return Ok(zero_arg_value_method("__bool__", move || {
                Value::bool_object(value)
            }));
        }
        if attribute == "__str__" || attribute == "__repr__" {
            let rendered = if self.value { "True" } else { "False" }.to_string();
            return Ok(zero_arg_string_method(attribute, rendered));
        }
        Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "bool".to_string(),
        })
    }
}

pub(crate) fn downcast_bool(value: &Value) -> Option<bool> {
    let object_ref = value.object_ref();
    let object = object_ref.borrow();
    object
        .as_any()
        .downcast_ref::<BoolObject>()
        .map(BoolObject::value)
}
