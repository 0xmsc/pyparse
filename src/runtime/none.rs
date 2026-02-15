use crate::runtime::error::RuntimeError;
use crate::runtime::object::{ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;
use std::rc::Rc;

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
            return Ok(Value::bound_method_object(Rc::new(
                move |_context, args| {
                    RuntimeError::expect_method_arity("__bool__", 0, args.len())?;
                    Ok(Value::bool_object(false))
                },
            )));
        }
        if attribute == "__str__" || attribute == "__repr__" {
            let method = attribute.to_string();
            return Ok(Value::bound_method_object(Rc::new(
                move |_context, args| {
                    RuntimeError::expect_method_arity(&method, 0, args.len())?;
                    Ok(Value::string_object("None".to_string()))
                },
            )));
        }
        Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "NoneType".to_string(),
        })
    }
}
