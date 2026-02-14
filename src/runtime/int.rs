use crate::runtime::list::ListError;
use crate::runtime::object::{AttributeError, BinaryOpError, MethodError, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct IntObject {
    value: i64,
}

impl IntObject {
    pub(crate) fn new(value: i64) -> Self {
        Self { value }
    }

    pub(crate) fn value(&self) -> i64 {
        self.value
    }
}

pub(crate) fn downcast_i64(value: &Value) -> Option<i64> {
    let object_ref = value.object_ref();
    let object = object_ref.borrow();
    object
        .as_any()
        .downcast_ref::<IntObject>()
        .map(IntObject::value)
}

impl RuntimeObject for IntObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "int"
    }

    fn is_truthy(&self) -> bool {
        self.value != 0
    }

    fn to_output(&self, _render_value: &dyn Fn(&Value) -> String) -> String {
        self.value.to_string()
    }

    fn get_attribute_method_name(&self, attribute: &str) -> Result<String, AttributeError> {
        Err(AttributeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "int".to_string(),
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
            type_name: "int".to_string(),
        })
    }

    fn add(&self, rhs: &Value) -> Result<Value, BinaryOpError> {
        if let Some(rhs_int) = downcast_i64(rhs) {
            Ok(Value::int_object(self.value + rhs_int))
        } else {
            Err(BinaryOpError::ExpectedIntegerType {
                got: format!("{rhs:?}"),
            })
        }
    }

    fn sub(&self, rhs: &Value) -> Result<Value, BinaryOpError> {
        if let Some(rhs_int) = downcast_i64(rhs) {
            Ok(Value::int_object(self.value - rhs_int))
        } else {
            Err(BinaryOpError::ExpectedIntegerType {
                got: format!("{rhs:?}"),
            })
        }
    }

    fn lt(&self, rhs: &Value) -> Result<Value, BinaryOpError> {
        if let Some(rhs_int) = downcast_i64(rhs) {
            Ok(Value::bool_object(self.value < rhs_int))
        } else {
            Err(BinaryOpError::ExpectedIntegerType {
                got: format!("{rhs:?}"),
            })
        }
    }
}
