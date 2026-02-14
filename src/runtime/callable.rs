use crate::builtins::BuiltinFunction;
use crate::runtime::list::ListError;
use crate::runtime::object::{
    AttributeError, BinaryOpError, CallTarget, MethodError, ObjectRef, RuntimeObject,
};
use crate::runtime::value::Value;
use std::any::Any;

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
}

macro_rules! impl_callable_runtime_object {
    ($type_name:expr, $call_target:expr, $to_output:expr) => {
        fn as_any(&self) -> &dyn Any {
            self
        }

        fn type_name(&self) -> &'static str {
            $type_name
        }

        fn is_truthy(&self) -> bool {
            true
        }

        fn to_output(&self, _render_value: &dyn Fn(&Value) -> String) -> String {
            $to_output(self)
        }

        fn get_attribute_method_name(&self, attribute: &str) -> Result<String, AttributeError> {
            Err(AttributeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: $type_name.to_string(),
            })
        }

        fn len(&self) -> usize {
            0
        }

        fn get_item(&self, _index: i64) -> Result<Value, ListError> {
            Err(ListError::OutOfBounds { index: 0, len: 0 })
        }

        fn set_item(&mut self, _index: i64, _value: Value) -> Result<(), ListError> {
            Err(ListError::OutOfBounds { index: 0, len: 0 })
        }

        fn call_method(&mut self, method: &str, _args: Vec<Value>) -> Result<(), MethodError> {
            Err(MethodError::UnknownMethod {
                method: method.to_string(),
                type_name: $type_name.to_string(),
            })
        }

        fn add(&self, _rhs: &Value) -> Result<Value, BinaryOpError> {
            Err(BinaryOpError::ExpectedIntegerType {
                got: $type_name.to_string(),
            })
        }

        fn sub(&self, _rhs: &Value) -> Result<Value, BinaryOpError> {
            Err(BinaryOpError::ExpectedIntegerType {
                got: $type_name.to_string(),
            })
        }

        fn lt(&self, _rhs: &Value) -> Result<Value, BinaryOpError> {
            Err(BinaryOpError::ExpectedIntegerType {
                got: $type_name.to_string(),
            })
        }

        fn call_target(&self) -> Option<CallTarget> {
            Some($call_target(self))
        }
    };
}

impl RuntimeObject for BuiltinFunctionObject {
    impl_callable_runtime_object!(
        "builtin_function_or_method",
        |this: &BuiltinFunctionObject| CallTarget::Builtin(this.builtin),
        |_this: &BuiltinFunctionObject| "<built-in function>".to_string()
    );
}

impl RuntimeObject for FunctionObject {
    impl_callable_runtime_object!(
        "function",
        |this: &FunctionObject| CallTarget::Function(this.name.clone()),
        |this: &FunctionObject| format!("<function {}>", this.name)
    );
}

impl RuntimeObject for BoundMethodObject {
    impl_callable_runtime_object!(
        "method",
        |this: &BoundMethodObject| CallTarget::BoundMethod {
            receiver: this.receiver.clone(),
            method: this.method.clone(),
        },
        |_this: &BoundMethodObject| "<bound method>".to_string()
    );
}
