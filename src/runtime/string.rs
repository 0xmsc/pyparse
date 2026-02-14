use crate::runtime::list::ListError;
use crate::runtime::object::{AttributeError, BinaryOpError, MethodError, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct StringObject {
    value: String,
}

impl StringObject {
    pub(crate) fn new(value: String) -> Self {
        Self { value }
    }
}

impl RuntimeObject for StringObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "str"
    }

    fn is_truthy(&self) -> bool {
        !self.value.is_empty()
    }

    fn to_output(&self, _render_value: &dyn Fn(&Value) -> String) -> String {
        self.value.clone()
    }

    fn get_attribute_method_name(&self, attribute: &str) -> Result<String, AttributeError> {
        Err(AttributeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "str".to_string(),
        })
    }

    fn len(&self) -> Result<usize, ListError> {
        Err(ListError::ExpectedListType {
            got: "str".to_string(),
        })
    }

    fn get_item(&self, _index: Value) -> Result<Value, ListError> {
        Err(ListError::ExpectedListType {
            got: "str".to_string(),
        })
    }

    fn set_item(&mut self, _index: Value, _value: Value) -> Result<(), ListError> {
        Err(ListError::ExpectedListType {
            got: "str".to_string(),
        })
    }

    fn call_method(&mut self, method: &str, _args: Vec<Value>) -> Result<(), MethodError> {
        Err(MethodError::UnknownMethod {
            method: method.to_string(),
            type_name: "str".to_string(),
        })
    }

    fn add(&self, _rhs: &Value) -> Result<Value, BinaryOpError> {
        Err(BinaryOpError::ExpectedIntegerType {
            got: "str".to_string(),
        })
    }

    fn sub(&self, _rhs: &Value) -> Result<Value, BinaryOpError> {
        Err(BinaryOpError::ExpectedIntegerType {
            got: "str".to_string(),
        })
    }

    fn lt(&self, _rhs: &Value) -> Result<Value, BinaryOpError> {
        Err(BinaryOpError::ExpectedIntegerType {
            got: "str".to_string(),
        })
    }
}
