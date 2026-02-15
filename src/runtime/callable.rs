use crate::builtins::BuiltinFunction;
use crate::runtime::error::RuntimeError;
use crate::runtime::method::{bound_method, zero_arg_string_method};
use crate::runtime::object::{BoundMethodCallable, ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BuiltinFunctionObject {
    builtin: BuiltinFunction,
}

impl BuiltinFunctionObject {
    pub(crate) fn new(builtin: BuiltinFunction) -> Self {
        Self { builtin }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionObject {
    name: String,
}

impl FunctionObject {
    pub(crate) fn new(name: String) -> Self {
        Self { name }
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone)]
pub(crate) struct BoundMethodObject {
    callable: BoundMethodCallable,
}

impl BoundMethodObject {
    pub(crate) fn new(callable: BoundMethodCallable) -> Self {
        Self { callable }
    }

    pub(crate) fn callable(&self) -> BoundMethodCallable {
        self.callable.clone()
    }
}

impl fmt::Debug for BoundMethodObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("BoundMethodObject(<callable>)")
    }
}

impl RuntimeObject for BuiltinFunctionObject {
    fn type_name(&self) -> &'static str {
        "builtin_function_or_method"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        if attribute == "__call__" {
            let builtin = self.builtin;
            return Ok(bound_method(move |context, args| {
                context.call_builtin(builtin, args)
            }));
        }

        if attribute == "__str__" || attribute == "__repr__" {
            return Ok(zero_arg_string_method(attribute, "<built-in function>"));
        }
        Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "builtin_function_or_method".to_string(),
        })
    }
}

impl RuntimeObject for FunctionObject {
    fn type_name(&self) -> &'static str {
        "function"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        if attribute == "__call__" {
            let function_name = self.name.clone();
            return Ok(bound_method(move |context, args| {
                context.call_function_named(&function_name, args)
            }));
        }

        if attribute == "__str__" || attribute == "__repr__" {
            return Ok(zero_arg_string_method(
                attribute,
                format!("<function {}>", self.name()),
            ));
        }
        Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "function".to_string(),
        })
    }
}

impl RuntimeObject for BoundMethodObject {
    fn type_name(&self) -> &'static str {
        "method"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        if attribute == "__call__" {
            let callable = self.callable();
            return Ok(bound_method(move |context, args| callable(context, args)));
        }

        if attribute == "__str__" || attribute == "__repr__" {
            return Ok(zero_arg_string_method(attribute, "<bound method>"));
        }
        Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "method".to_string(),
        })
    }
}
