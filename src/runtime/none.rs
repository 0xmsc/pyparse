use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
use crate::runtime::object::{
    ObjectRef, RuntimeObject, TypeObject, object_not_callable, unknown_attribute,
    unsupported_attribute_assignment,
};
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

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_object(&self) -> &'static TypeObject {
        &NONE_TYPE
    }
}

static NONE_TYPE: TypeObject = TypeObject {
    name: "NoneType",
    get_attribute: none_get_attribute,
    set_attribute: unsupported_attribute_assignment,
    call: object_not_callable,
};

fn none_get_attribute(receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
    match attribute {
        "__bool__" => Ok(bound_method(move |_context, args| {
            RuntimeError::expect_method_arity("__bool__", 0, args.len())?;
            Ok(Value::bool_object(false))
        })),
        "__str__" | "__repr__" => {
            let method = attribute.to_string();
            Ok(bound_method(move |_context, args| {
                RuntimeError::expect_method_arity(&method, 0, args.len())?;
                Ok(Value::string_object("None".to_string()))
            }))
        }
        _ => unknown_attribute(receiver, attribute),
    }
}
