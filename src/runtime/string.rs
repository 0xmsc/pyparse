use crate::runtime::error::RuntimeError;
use crate::runtime::object::{ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;
use std::rc::Rc;

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
    fn type_name(&self) -> &'static str {
        "str"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        if attribute == "__bool__" {
            let is_non_empty = !self.value.is_empty();
            return Ok(Value::bound_method_object(Rc::new(
                move |_context, args| {
                    if !args.is_empty() {
                        return Err(RuntimeError::ArityMismatch {
                            method: "__bool__".to_string(),
                            expected: 0,
                            found: args.len(),
                        });
                    }
                    Ok(Value::bool_object(is_non_empty))
                },
            )));
        }
        if attribute == "__str__" {
            let value = self.value.clone();
            return Ok(Value::bound_method_object(Rc::new(
                move |_context, args| {
                    if !args.is_empty() {
                        return Err(RuntimeError::ArityMismatch {
                            method: "__str__".to_string(),
                            expected: 0,
                            found: args.len(),
                        });
                    }
                    Ok(Value::string_object(value.clone()))
                },
            )));
        }
        if attribute == "__repr__" {
            let value = format!("{:?}", self.value);
            return Ok(Value::bound_method_object(Rc::new(
                move |_context, args| {
                    if !args.is_empty() {
                        return Err(RuntimeError::ArityMismatch {
                            method: "__repr__".to_string(),
                            expected: 0,
                            found: args.len(),
                        });
                    }
                    Ok(Value::string_object(value.clone()))
                },
            )));
        }
        Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "str".to_string(),
        })
    }
}

pub(crate) fn downcast_string(value: &Value) -> Option<String> {
    let object_ref = value.object_ref();
    let object = object_ref.borrow();
    object
        .as_any()
        .downcast_ref::<StringObject>()
        .map(|string| string.value().to_string())
}
