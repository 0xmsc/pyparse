use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
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
}

pub(crate) fn downcast_i64(value: &Value) -> Option<i64> {
    let object_ref = value.object_ref();
    let object = object_ref.borrow();
    object
        .as_any()
        .downcast_ref::<IntObject>()
        .map(IntObject::value)
}

fn with_int<R>(receiver: &ObjectRef, f: impl FnOnce(&IntObject) -> R) -> R {
    let object = receiver.borrow();
    let int = object
        .as_any()
        .downcast_ref::<IntObject>()
        .expect("int get_attribute receiver must be IntObject");
    f(int)
}

fn expect_rhs_int(operation: &str, args: &[Value]) -> Result<i64, RuntimeError> {
    RuntimeError::expect_method_arity(operation, 1, args.len())?;
    let rhs = args.first().expect("len checked above");
    let Some(rhs_int) = downcast_i64(rhs) else {
        return Err(RuntimeError::InvalidArgumentType {
            operation: operation.to_string(),
            argument: "rhs".to_string(),
            expected: "int".to_string(),
            got: rhs.type_name().to_string(),
        });
    };
    Ok(rhs_int)
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
        match attribute {
            "__add__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |_context, args| {
                    let rhs_int = expect_rhs_int("__add__", &args)?;
                    Ok(with_int(&receiver, |int| {
                        Value::int_object(int.value + rhs_int)
                    }))
                }))
            }
            "__sub__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |_context, args| {
                    let rhs_int = expect_rhs_int("__sub__", &args)?;
                    Ok(with_int(&receiver, |int| {
                        Value::int_object(int.value - rhs_int)
                    }))
                }))
            }
            "__lt__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |_context, args| {
                    let rhs_int = expect_rhs_int("__lt__", &args)?;
                    Ok(with_int(&receiver, |int| {
                        Value::bool_object(int.value < rhs_int)
                    }))
                }))
            }
            "__bool__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity("__bool__", 0, args.len())?;
                    Ok(with_int(&receiver, |int| {
                        Value::bool_object(int.value != 0)
                    }))
                }))
            }
            "__str__" | "__repr__" => {
                let receiver = receiver.clone();
                let method = attribute.to_string();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity(&method, 0, args.len())?;
                    Ok(with_int(&receiver, |int| {
                        Value::string_object(int.value.to_string())
                    }))
                }))
            }
            _ => Err(RuntimeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: "int".to_string(),
            }),
        }
    }

    fn invoke(
        &self,
        _receiver: ObjectRef,
        _context: &mut dyn crate::runtime::object::CallContext,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        Err(RuntimeError::ObjectNotCallable {
            type_name: self.type_name().to_string(),
        })
    }
}
