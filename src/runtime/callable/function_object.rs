use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
use crate::runtime::object::{CallContext, CallableId, ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;

/// Runtime wrapper for a user-defined function symbol.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionObject {
    name: String,
    callable_id: CallableId,
}

impl FunctionObject {
    pub(crate) fn new(name: String, callable_id: CallableId) -> Self {
        Self { name, callable_id }
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }
}

impl RuntimeObject for FunctionObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "function"
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match attribute {
            "__call__" => {
                let callable_id = self.callable_id;
                Ok(bound_method(move |context, args| {
                    context.call_callable(&callable_id, args)
                }))
            }
            "__str__" | "__repr__" => {
                let method = attribute.to_string();
                let rendered = format!("<function {}>", self.name());
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

    fn call(
        &self,
        _receiver: ObjectRef,
        context: &mut dyn CallContext,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        context.call_callable(&self.callable_id, args)
    }
}
