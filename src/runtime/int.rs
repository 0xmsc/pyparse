use crate::runtime::error::RuntimeError;
use crate::runtime::object::{ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;
use std::rc::Rc;

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
        if method == "__bool__" {
            if !args.is_empty() {
                return Err(RuntimeError::ArityMismatch {
                    method: method.to_string(),
                    expected: 0,
                    found: args.len(),
                });
            }
            return Ok(Value::bool_object(self.value != 0));
        }
        if method == "__str__" || method == "__repr__" {
            if !args.is_empty() {
                return Err(RuntimeError::ArityMismatch {
                    method: method.to_string(),
                    expected: 0,
                    found: args.len(),
                });
            }
            return Ok(Value::string_object(self.value.to_string()));
        }
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
    object
        .as_any()
        .downcast_ref::<IntObject>()
        .map(IntObject::value)
}

fn call_method_on_receiver(
    receiver: &ObjectRef,
    method: &str,
    args: Vec<Value>,
) -> Result<Value, RuntimeError> {
    let mut object = receiver.borrow_mut();
    let int = object
        .as_any_mut()
        .downcast_mut::<IntObject>()
        .expect("int get_attribute receiver must be IntObject");
    int.call_method(method, args)
}

impl RuntimeObject for IntObject {
    fn type_name(&self) -> &'static str {
        "int"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn get_attribute(&self, receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        if matches!(
            attribute,
            "__add__" | "__sub__" | "__lt__" | "__bool__" | "__str__" | "__repr__"
        ) {
            let receiver = receiver.clone();
            let method = attribute.to_string();
            return Ok(Value::bound_method_object(Rc::new(move |args| {
                call_method_on_receiver(&receiver, &method, args)
            })));
        }
        Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "int".to_string(),
        })
    }
}
