use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
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
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "str"
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match attribute {
            "__bool__" => {
                let is_non_empty = !self.value.is_empty();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity("__bool__", 0, args.len())?;
                    Ok(Value::bool_object(is_non_empty))
                }))
            }
            "__str__" => {
                let value = self.value.clone();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity("__str__", 0, args.len())?;
                    Ok(Value::string_object(value.clone()))
                }))
            }
            "__repr__" => {
                let rendered = format!("{:?}", self.value);
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity("__repr__", 0, args.len())?;
                    Ok(Value::string_object(rendered.clone()))
                }))
            }
            _ => Err(RuntimeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: self.type_name().to_string(),
            }),
        }
    }
}

pub(crate) fn downcast_string(value: &Value) -> Option<String> {
    let Value::Object(object_ref) = value else {
        return None;
    };
    let object = object_ref.borrow();
    object
        .as_any()
        .downcast_ref::<StringObject>()
        .map(|string| string.value().to_string())
}
