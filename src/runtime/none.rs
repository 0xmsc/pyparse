use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
use crate::runtime::object::{ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;

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
        match attribute {
            "__bool__" => Ok(bound_method(move |_context, args| {
                RuntimeError::expect_method_arity("__bool__", 0, args.len())?;
                Ok(Value::bool_object(false))
            })),
            "__str__" | "__repr__" => {
                let method = attribute.to_string();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity(&method, 0, args.len())?;
                    Ok(Value::string_object("None".to_string()))
                }))
            }
            _ => Err(RuntimeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: "NoneType".to_string(),
            }),
        }
    }

    fn invoke(
        &self,
        _receiver: ObjectRef,
        _context: &mut dyn crate::runtime::object::CallContext,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        Err(RuntimeError::ObjectNotCallable {
            type_name: self.type_name().to_string(),
        })
    }
}
