use crate::runtime::error::RuntimeError;
use crate::runtime::object::{ObjectRef, RuntimeObject};
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

    pub(crate) fn call_method(
        &mut self,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::ArityMismatch {
                method: method.to_string(),
                expected: 1,
                found: args.len(),
            });
        }
        let rhs = args.first().expect("len checked above");
        let Some(rhs_int) = downcast_i64(rhs) else {
            return Err(RuntimeError::InvalidArgumentType {
                operation: method.to_string(),
                argument: "rhs".to_string(),
                expected: "int".to_string(),
                got: rhs.type_name().to_string(),
            });
        };
        match method {
            "__add__" => Ok(Value::int_object(self.value + rhs_int)),
            "__sub__" => Ok(Value::int_object(self.value - rhs_int)),
            "__lt__" => Ok(Value::bool_object(self.value < rhs_int)),
            _ => Err(RuntimeError::UnknownMethod {
                method: method.to_string(),
                type_name: "int".to_string(),
            }),
        }
    }
}

pub(crate) fn downcast_i64(value: &Value) -> Option<i64> {
    let object_ref = value.object_ref();
    let object = object_ref.borrow();
    let any = &**object as &dyn Any;
    any.downcast_ref::<IntObject>().map(IntObject::value)
}

impl RuntimeObject for IntObject {
    fn get_attribute(&self, receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match attribute {
            "__add__" | "__sub__" | "__lt__" => {
                Ok(Value::bound_method_object(receiver, attribute.to_string()))
            }
            _ => Err(RuntimeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: "int".to_string(),
            }),
        }
    }
}
