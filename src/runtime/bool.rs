use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
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
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "bool"
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match attribute {
            "__bool__" => {
                let value = self.value;
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity("__bool__", 0, args.len())?;
                    Ok(Value::bool_object(value))
                }))
            }
            "__str__" | "__repr__" => {
                let rendered = if self.value { "True" } else { "False" }.to_string();
                let method = attribute.to_string();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity(&method, 0, args.len())?;
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

pub(crate) fn downcast_bool(value: &Value) -> Option<bool> {
    let object_ref = value.object_ref();
    let object = object_ref.borrow();
    object
        .as_any()
        .downcast_ref::<BoolObject>()
        .map(BoolObject::value)
}
