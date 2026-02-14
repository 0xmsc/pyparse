use crate::builtins::BuiltinFunction;
use crate::runtime::error::RuntimeError;
use crate::runtime::object::{BoundMethodCallable, CallTarget, ObjectRef, RuntimeObject};
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

pub(crate) fn function_to_output(value: &Value) -> String {
    let object_ref = value.object_ref();
    let object = object_ref.borrow();
    let any = &**object as &dyn Any;
    let function = any
        .downcast_ref::<FunctionObject>()
        .expect("function behavior must wrap FunctionObject");
    format!("<function {}>", function.name())
}
