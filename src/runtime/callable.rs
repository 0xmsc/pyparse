use crate::builtins::BuiltinFunction;
use crate::runtime::error::RuntimeError;
use crate::runtime::object::{BoundMethodCallable, CallTarget, ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BuiltinFunctionObject {
    builtin: BuiltinFunction,
}

impl BuiltinFunctionObject {
    pub(crate) fn new(builtin: BuiltinFunction) -> Self {
        Self { builtin }
    }

    pub(crate) fn call_target(&self) -> CallTarget {
        CallTarget::Builtin(self.builtin)
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

    pub(crate) fn call_target(&self) -> CallTarget {
        CallTarget::Function(self.name.clone())
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

    pub(crate) fn call_target(&self) -> CallTarget {
        CallTarget::BoundMethod(self.callable.clone())
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
        if attribute == "__str__" || attribute == "__repr__" {
            let method = attribute.to_string();
            return Ok(Value::bound_method_object(Rc::new(move |args| {
                if !args.is_empty() {
                    return Err(RuntimeError::ArityMismatch {
                        method: method.clone(),
                        expected: 0,
                        found: args.len(),
                    });
                }
                Ok(Value::string_object("<built-in function>".to_string()))
            })));
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
        if attribute == "__str__" || attribute == "__repr__" {
            let rendered = format!("<function {}>", self.name());
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
        if attribute == "__str__" || attribute == "__repr__" {
            let method = attribute.to_string();
            return Ok(Value::bound_method_object(Rc::new(move |args| {
                if !args.is_empty() {
                    return Err(RuntimeError::ArityMismatch {
                        method: method.clone(),
                        expected: 0,
                        found: args.len(),
                    });
                }
                Ok(Value::string_object("<bound method>".to_string()))
            })));
        }
        Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "method".to_string(),
        })
    }
}
