use crate::runtime::error::RuntimeError;
use crate::runtime::object::{ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;
use std::rc::Rc;

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
        if attribute == "__bool__" {
            let value = self.value;
            return Ok(Value::bound_method_object(Rc::new(move |args| {
                if !args.is_empty() {
                    return Err(RuntimeError::ArityMismatch {
                        method: "__bool__".to_string(),
                        expected: 0,
                        found: args.len(),
                    });
                }
                Ok(Value::bool_object(value))
            })));
        }
        if attribute == "__str__" || attribute == "__repr__" {
            let rendered = if self.value { "True" } else { "False" }.to_string();
            let method = attribute.to_string();
            return Ok(Value::bound_method_object(Rc::new(move |args| {
                if !args.is_empty() {
                    return Err(RuntimeError::ArityMismatch {
                        method: method.clone(),
                        expected: 0,
                        found: args.len(),
                    });
                }
                Ok(Value::string_object(rendered.clone()))
            })));
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
    let any = &**object as &dyn Any;
    any.downcast_ref::<BoolObject>().map(BoolObject::value)
}
