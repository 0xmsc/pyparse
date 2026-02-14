use crate::runtime::list::ListError;
use crate::runtime::object::{AttributeError, BinaryOpError, MethodError, RuntimeObject};
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
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "NoneType"
    }

    fn is_truthy(&self) -> bool {
        false
    }

    fn to_output(&self, _render_value: &dyn Fn(&Value) -> String) -> String {
        "None".to_string()
    }

    fn get_attribute_method_name(&self, attribute: &str) -> Result<String, AttributeError> {
        Err(AttributeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "NoneType".to_string(),
        })
    }

    fn len(&self) -> Result<usize, ListError> {
        Err(ListError::ExpectedListType {
            got: "NoneType".to_string(),
        })
    }

    fn get_item(&self, _index: Value) -> Result<Value, ListError> {
        Err(ListError::ExpectedListType {
            got: "NoneType".to_string(),
        })
    }

    fn set_item(&mut self, _index: Value, _value: Value) -> Result<(), ListError> {
        Err(ListError::ExpectedListType {
            got: "NoneType".to_string(),
        })
    }

    fn call_method(&mut self, method: &str, _args: Vec<Value>) -> Result<(), MethodError> {
        Err(MethodError::UnknownMethod {
            method: method.to_string(),
            type_name: "NoneType".to_string(),
        })
    }

    fn add(&self, _rhs: &Value) -> Result<Value, BinaryOpError> {
        Err(BinaryOpError::ExpectedIntegerType {
            got: "NoneType".to_string(),
        })
    }

    fn sub(&self, _rhs: &Value) -> Result<Value, BinaryOpError> {
        Err(BinaryOpError::ExpectedIntegerType {
            got: "NoneType".to_string(),
        })
    }

    fn lt(&self, _rhs: &Value) -> Result<Value, BinaryOpError> {
        Err(BinaryOpError::ExpectedIntegerType {
            got: "NoneType".to_string(),
        })
    }
}
