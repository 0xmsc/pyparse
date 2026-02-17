use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
use crate::runtime::object::{
    ObjectRef, RuntimeObject, TypeObject, object_not_callable, unknown_attribute,
    unsupported_attribute_assignment,
};
use crate::runtime::value::Value;
use std::any::Any;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BoolObject {
    value: bool,
}

impl BoolObject {
    pub(crate) fn new(value: bool) -> Self {
        Self { value }
    }

    pub(crate) fn value(&self) -> bool {
        self.value
    }
}

impl RuntimeObject for BoolObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_object(&self) -> &'static TypeObject {
        &BOOL_TYPE
    }
}

static BOOL_TYPE: TypeObject = TypeObject {
    name: "bool",
    get_attribute: bool_get_attribute,
    set_attribute: unsupported_attribute_assignment,
    call: object_not_callable,
};

fn bool_get_attribute(receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
    let value = {
        let object = receiver.borrow();
        object
            .as_any()
            .downcast_ref::<BoolObject>()
            .expect("bool get_attribute receiver must be BoolObject")
            .value
    };
    match attribute {
        "__bool__" => Ok(bound_method(move |_context, args| {
            RuntimeError::expect_method_arity("__bool__", 0, args.len())?;
            Ok(Value::bool_object(value))
        })),
        "__str__" | "__repr__" => {
            let rendered = if value { "True" } else { "False" }.to_string();
            let method = attribute.to_string();
            Ok(bound_method(move |_context, args| {
                RuntimeError::expect_method_arity(&method, 0, args.len())?;
                Ok(Value::string_object(rendered.clone()))
            }))
        }
        _ => unknown_attribute(receiver, attribute),
    }
}

pub(crate) fn downcast_bool(value: &Value) -> Option<bool> {
    let object_ref = value.object_ref();
    let object = object_ref.borrow();
    object
        .as_any()
        .downcast_ref::<BoolObject>()
        .map(BoolObject::value)
}
