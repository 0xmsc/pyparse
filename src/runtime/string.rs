use crate::runtime::object::{AttributeError, ObjectRef, RuntimeObject};
use crate::runtime::value::Value;

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
    fn get_attribute(
        &self,
        _receiver: ObjectRef,
        attribute: &str,
    ) -> Result<Value, AttributeError> {
        Err(AttributeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "str".to_string(),
        })
    }
}
