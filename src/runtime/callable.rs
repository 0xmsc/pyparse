use crate::builtins::BuiltinFunction;
use crate::runtime::error::RuntimeError;
use crate::runtime::object::{CallTarget, ObjectRef, RuntimeObject};
use crate::runtime::value::Value;

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

#[derive(Debug, Clone)]
pub(crate) struct BoundMethodObject {
    receiver: ObjectRef,
    method: String,
}

impl BoundMethodObject {
    pub(crate) fn new(receiver: ObjectRef, method: String) -> Self {
        Self { receiver, method }
    }

    pub(crate) fn call_target(&self) -> CallTarget {
        CallTarget::BoundMethod {
            receiver: self.receiver.clone(),
            method: self.method.clone(),
        }
    }
}

macro_rules! impl_callable_runtime_object {
    ($type_name:expr) => {
        fn get_attribute(
            &self,
            _receiver: ObjectRef,
            attribute: &str,
        ) -> Result<Value, RuntimeError> {
            Err(RuntimeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: $type_name.to_string(),
            })
        }
    };
}

impl RuntimeObject for BuiltinFunctionObject {
    impl_callable_runtime_object!("builtin_function_or_method");
}

impl RuntimeObject for FunctionObject {
    impl_callable_runtime_object!("function");
}

impl RuntimeObject for BoundMethodObject {
    impl_callable_runtime_object!("method");
}
