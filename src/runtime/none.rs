use crate::runtime::error::RuntimeError;
use crate::runtime::object::{ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct NoneObject;

impl NoneObject {
    pub(crate) fn new() -> Self {
        Self
    }
}

impl RuntimeObject for NoneObject {
    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        if attribute == "__bool__" {
            return Ok(Value::bound_method_object(Rc::new(move |args| {
                if !args.is_empty() {
                    return Err(RuntimeError::ArityMismatch {
                        method: "__bool__".to_string(),
                        expected: 0,
                        found: args.len(),
                    });
                }
                Ok(Value::bool_object(false))
            })));
        }
        Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "NoneType".to_string(),
        })
    }
}
