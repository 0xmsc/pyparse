use crate::runtime::error::RuntimeError;
use crate::runtime::object::{ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct StringObject {
    value: String,
}

impl StringObject {
    pub(crate) fn new(value: String) -> Self {
        Self { value }
    }

    pub(crate) fn value(&self) -> &str {
        &self.value
    }
}

impl RuntimeObject for StringObject {
    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "str".to_string(),
        })
    }
}

fn downcast_string(value: &Value) -> Option<String> {
    let object_ref = value.object_ref();
    let object = object_ref.borrow();
    let any = &**object as &dyn Any;
    any.downcast_ref::<StringObject>()
        .map(|string| string.value().to_string())
}

pub(crate) fn try_to_output(value: &Value) -> Option<String> {
    downcast_string(value)
}

pub(crate) fn try_is_truthy(value: &Value) -> Option<bool> {
    downcast_string(value).map(|string| !string.is_empty())
}
