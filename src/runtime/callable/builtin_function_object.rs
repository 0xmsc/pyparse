use crate::builtins::BuiltinFunction;
use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
use crate::runtime::object::{CallContext, CallableId, ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;

/// Runtime wrapper for a built-in function symbol (e.g. `print`, `len`).
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BuiltinFunctionObject {
    builtin: BuiltinFunction,
}

impl BuiltinFunctionObject {
    pub(crate) fn new(builtin: BuiltinFunction) -> Self {
        Self { builtin }
    }
}

impl RuntimeObject for BuiltinFunctionObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "builtin_function_or_method"
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match attribute {
            "__call__" => {
                let builtin = self.builtin;
                let callable_id = CallableId(builtin.callable_id());
                Ok(bound_method(move |context, args| {
                    context.call_callable(&callable_id, args)
                }))
            }
            "__str__" | "__repr__" => {
                let method = attribute.to_string();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity(&method, 0, args.len())?;
                    Ok(Value::string_object("<built-in function>".to_string()))
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
        context.call_callable(&CallableId(self.builtin.callable_id()), args)
    }
}
