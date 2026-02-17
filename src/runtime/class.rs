use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
use crate::runtime::object::{
    CallContext, ObjectRef, RuntimeObject, TypeObject, object_not_callable, unknown_attribute,
    unsupported_attribute_assignment,
};
use crate::runtime::value::Value;
use std::any::Any;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub(crate) struct ClassObject {
    name: String,
    methods: HashMap<String, Value>,
}

impl ClassObject {
    pub(crate) fn new(name: String, methods: HashMap<String, Value>) -> Self {
        Self { name, methods }
    }

    pub(crate) fn method(&self, attribute: &str) -> Option<Value> {
        self.methods.get(attribute).cloned()
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }
}

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
}

impl RuntimeObject for ClassObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_object(&self) -> &'static TypeObject {
        &CLASS_TYPE
    }
}

impl RuntimeObject for InstanceObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_object(&self) -> &'static TypeObject {
        &INSTANCE_TYPE
    }
}

static CLASS_TYPE: TypeObject = TypeObject {
    name: "type",
    get_attribute: class_get_attribute,
    set_attribute: unsupported_attribute_assignment,
    call: class_call,
};

static INSTANCE_TYPE: TypeObject = TypeObject {
    name: "instance",
    get_attribute: instance_get_attribute,
    set_attribute: instance_set_attribute,
    call: object_not_callable,
};

fn class_get_attribute(receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
    match attribute {
        "__call__" => {
            let receiver = receiver.clone();
            Ok(bound_method(move |context, args| {
                let type_object = { receiver.borrow().type_object() };
                type_object.call(receiver.clone(), context, args)
            }))
        }
        "__str__" | "__repr__" => {
            let rendered = with_class(&receiver, |class_object| {
                format!("<class '{}'>", class_object.name())
            });
            let method = attribute.to_string();
            Ok(bound_method(move |_context, args| {
                RuntimeError::expect_method_arity(&method, 0, args.len())?;
                Ok(Value::string_object(rendered.clone()))
            }))
        }
        _ => unknown_attribute(receiver, attribute),
    }
}

fn class_call(
    receiver: ObjectRef,
    context: &mut dyn CallContext,
    args: Vec<Value>,
) -> Result<Value, RuntimeError> {
    let instance = Value::instance_object(receiver.clone());
    let has_init = with_class(&receiver, |class_object| {
        class_object.method("__init__").is_some()
    });
    if has_init {
        let init = instance.get_attribute("__init__")?;
        init.call(context, args)?;
        return Ok(instance);
    }
    RuntimeError::expect_function_arity("__init__", 0, args.len())?;
    Ok(instance)
}

fn instance_get_attribute(receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
    if let Some(value) = with_instance(&receiver, |instance_object| {
        instance_object.attributes.get(attribute).cloned()
    }) {
        return Ok(value);
    }

    if let Some(method) = with_instance(&receiver, |instance_object| {
        class_method(instance_object.class.clone(), attribute)
    }) {
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
            let rendered = with_instance(&receiver, |instance_object| {
                format!("<{} object>", class_name(instance_object.class.clone()))
            });
            let method = attribute.to_string();
            Ok(bound_method(move |_context, args| {
                RuntimeError::expect_method_arity(&method, 0, args.len())?;
                Ok(Value::string_object(rendered.clone()))
            }))
        }
        _ => unknown_attribute(receiver, attribute),
    }
}

fn instance_set_attribute(
    receiver: ObjectRef,
    attribute: &str,
    value: Value,
) -> Result<(), RuntimeError> {
    with_instance_mut(&receiver, |instance_object| {
        instance_object
            .attributes
            .insert(attribute.to_string(), value);
    });
    Ok(())
}

fn with_class<R>(receiver: &ObjectRef, f: impl FnOnce(&ClassObject) -> R) -> R {
    let object = receiver.borrow();
    let class_object = object
        .as_any()
        .downcast_ref::<ClassObject>()
        .expect("class receiver must be ClassObject");
    f(class_object)
}

fn with_instance<R>(receiver: &ObjectRef, f: impl FnOnce(&InstanceObject) -> R) -> R {
    let object = receiver.borrow();
    let instance_object = object
        .as_any()
        .downcast_ref::<InstanceObject>()
        .expect("instance receiver must be InstanceObject");
    f(instance_object)
}

fn with_instance_mut<R>(receiver: &ObjectRef, f: impl FnOnce(&mut InstanceObject) -> R) -> R {
    let mut object = receiver.borrow_mut();
    let instance_object = object
        .as_any_mut()
        .downcast_mut::<InstanceObject>()
        .expect("instance receiver must be InstanceObject");
    f(instance_object)
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
