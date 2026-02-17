use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
use crate::runtime::object::{
    ObjectRef, RuntimeObject, TypeObject, object_not_callable, unknown_attribute,
    unsupported_attribute_assignment,
};
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

    pub(crate) fn value(&self) -> &str {
        &self.value
    }
}

impl RuntimeObject for StringObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_object(&self) -> &'static TypeObject {
        &STRING_TYPE
    }
}

static STRING_TYPE: TypeObject = TypeObject {
    name: "str",
    get_attribute: string_get_attribute,
    set_attribute: unsupported_attribute_assignment,
    call: object_not_callable,
};

fn string_get_attribute(receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
    let value = {
        let object = receiver.borrow();
        object
            .as_any()
            .downcast_ref::<StringObject>()
            .expect("str get_attribute receiver must be StringObject")
            .value
            .clone()
    };
    match attribute {
        "__bool__" => {
            let is_non_empty = !value.is_empty();
            Ok(bound_method(move |_context, args| {
                RuntimeError::expect_method_arity("__bool__", 0, args.len())?;
                Ok(Value::bool_object(is_non_empty))
            }))
        }
        "__str__" => Ok(bound_method(move |_context, args| {
            RuntimeError::expect_method_arity("__str__", 0, args.len())?;
            Ok(Value::string_object(value.clone()))
        })),
        "__repr__" => {
            let rendered = format!("{:?}", value);
            Ok(bound_method(move |_context, args| {
                RuntimeError::expect_method_arity("__repr__", 0, args.len())?;
                Ok(Value::string_object(rendered.clone()))
            }))
        }
        _ => unknown_attribute(receiver, attribute),
    }
}

pub(crate) fn downcast_string(value: &Value) -> Option<String> {
    let object_ref = value.object_ref();
    let object = object_ref.borrow();
    object
        .as_any()
        .downcast_ref::<StringObject>()
        .map(|string| string.value().to_string())
}
