use crate::runtime::class::ClassObject;
use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
use crate::runtime::object::{ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;
use std::collections::HashMap;

/// Runtime representation of a class instance with per-instance attributes.
#[derive(Debug, Clone)]
pub(crate) struct InstanceObject {
    class: ObjectRef,
    attributes: HashMap<String, Value>,
}

impl InstanceObject {
    pub(crate) fn new(class: ObjectRef) -> Self {
        Self {
            class,
            attributes: HashMap::new(),
        }
    }

    pub(crate) fn class_ref(&self) -> ObjectRef {
        self.class.clone()
    }
}

fn class_method(class: ObjectRef, attribute: &str) -> Option<Value> {
    let class_borrow = class.borrow();
    let class_object = class_borrow
        .as_any()
        .downcast_ref::<ClassObject>()
        .expect("instance class must be ClassObject");
    class_object.method(attribute)
}

fn class_name(class: ObjectRef) -> String {
    let class_borrow = class.borrow();
    let class_object = class_borrow
        .as_any()
        .downcast_ref::<ClassObject>()
        .expect("instance class must be ClassObject");
    class_object.name().to_string()
}

impl RuntimeObject for InstanceObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "instance"
    }

    fn get_attribute(&self, receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        if let Some(value) = self.attributes.get(attribute).cloned() {
            return Ok(value);
        }

        if let Some(method) = class_method(self.class.clone(), attribute) {
            let receiver = receiver.clone();
            return Ok(bound_method(move |context, mut args| {
                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.push(Value::from_object_ref(receiver.clone()));
                call_args.append(&mut args);
                method.call(context, call_args)
            }));
        }

        match attribute {
            "__str__" | "__repr__" => {
                let rendered = format!("<{} object>", class_name(self.class.clone()));
                let method = attribute.to_string();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity(&method, 0, args.len())?;
                    Ok(Value::string_object(rendered.clone()))
                }))
            }
            _ => Err(RuntimeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: self.type_name().to_string(),
            }),
        }
    }

    fn set_attribute(&mut self, attribute: &str, value: Value) -> Result<(), RuntimeError> {
        self.attributes.insert(attribute.to_string(), value);
        Ok(())
    }
}
