use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
use crate::runtime::object::{BoundMethodCallable, CallContext, ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;
use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CallableObjectKind {
    BoundMethod,
    MethodWrapper,
}

/// Heap object for closures used as bound methods or method-wrapper objects.
#[derive(Clone)]
pub(crate) struct CallableObject {
    kind: CallableObjectKind,
    callable: BoundMethodCallable,
}

impl CallableObject {
    pub(crate) fn bound_method(callable: BoundMethodCallable) -> Self {
        Self {
            kind: CallableObjectKind::BoundMethod,
            callable,
        }
    }

    pub(crate) fn method_wrapper(callable: BoundMethodCallable) -> Self {
        Self {
            kind: CallableObjectKind::MethodWrapper,
            callable,
        }
    }
}

impl fmt::Debug for CallableObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            CallableObjectKind::BoundMethod => f.write_str("CallableObject(method, <callable>)"),
            CallableObjectKind::MethodWrapper => {
                f.write_str("CallableObject(method-wrapper, <callable>)")
            }
        }
    }
}

impl RuntimeObject for CallableObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        match self.kind {
            CallableObjectKind::BoundMethod => "method",
            CallableObjectKind::MethodWrapper => "method-wrapper",
        }
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match self.kind {
            CallableObjectKind::BoundMethod => match attribute {
                "__call__" => Ok(Value::method_wrapper_object(self.callable.clone())),
                "__str__" | "__repr__" => {
                    let method = attribute.to_string();
                    Ok(bound_method(move |_context, args| {
                        RuntimeError::expect_method_arity(&method, 0, args.len())?;
                        Ok(Value::string_object("<bound method>".to_string()))
                    }))
                }
                _ => Err(RuntimeError::UnknownAttribute {
                    attribute: attribute.to_string(),
                    type_name: self.type_name().to_string(),
                }),
            },
            CallableObjectKind::MethodWrapper => match attribute {
                "__call__" => Ok(Value::method_wrapper_object(self.callable.clone())),
                "__str__" | "__repr__" => {
                    let method = attribute.to_string();
                    Ok(bound_method(move |_context, args| {
                        RuntimeError::expect_method_arity(&method, 0, args.len())?;
                        Ok(Value::string_object(
                            "<method-wrapper '__call__'>".to_string(),
                        ))
                    }))
                }
                _ => Err(RuntimeError::UnknownAttribute {
                    attribute: attribute.to_string(),
                    type_name: self.type_name().to_string(),
                }),
            },
        }
    }

    fn call(
        &self,
        _receiver: ObjectRef,
        context: &mut dyn CallContext,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        (self.callable)(context, args)
    }
}
