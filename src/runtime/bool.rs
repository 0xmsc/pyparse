use crate::runtime::error::RuntimeError;
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
    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "bool".to_string(),
        })
    }
}

fn downcast_bool(value: &Value) -> Option<bool> {
    let object_ref = value.object_ref();
    let object = object_ref.borrow();
    let any = &**object as &dyn Any;
    any.downcast_ref::<BoolObject>().map(BoolObject::value)
}

pub(crate) fn try_to_output(value: &Value) -> Option<String> {
    downcast_bool(value).map(|boolean| {
        if boolean {
            "True".to_string()
        } else {
            "False".to_string()
        }
    })
}

pub(crate) fn try_is_truthy(value: &Value) -> Option<bool> {
    downcast_bool(value)
}
