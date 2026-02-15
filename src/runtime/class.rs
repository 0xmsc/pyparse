use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
use crate::runtime::object::{CallContext, ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;
use std::collections::HashMap;

pub(crate) fn mangle_class_method_name(class_name: &str, method_name: &str) -> String {
    format!("__class__{class_name}__{method_name}")
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ClassObject {
    name: String,
    methods: HashMap<String, String>,
}

impl ClassObject {
    pub(crate) fn new(name: String, methods: HashMap<String, String>) -> Self {
        Self { name, methods }
    }

    pub(crate) fn method_name(&self, attribute: &str) -> Option<&str> {
        self.methods.get(attribute).map(String::as_str)
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
    fn type_name(&self) -> &'static str {
        "type"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn get_attribute(&self, receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match attribute {
            "__call__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |context, args| {
                    receiver.borrow().invoke(receiver.clone(), context, args)
                }))
            }
            "__str__" | "__repr__" => {
                let rendered = format!("<class '{}'>", self.name());
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

    fn invoke(
        &self,
        receiver: ObjectRef,
        context: &mut dyn CallContext,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let instance = Value::instance_object(receiver.clone());
        if self.method_name("__init__").is_some() {
            let init = instance.get_attribute("__init__")?;
            init.call(context, args)?;
            return Ok(instance);
        }

        RuntimeError::expect_function_arity("__init__", 0, args.len())?;
        Ok(instance)
    }
}

impl RuntimeObject for InstanceObject {
    fn type_name(&self) -> &'static str {
        "instance"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn get_attribute(&self, receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        if let Some(value) = self.attributes.get(attribute) {
            return Ok(value.clone());
        }

        if let Some(method_name) = class_method_name(self.class.clone(), attribute) {
            let receiver = receiver.clone();
            let method_name = method_name.to_string();
            return Ok(bound_method(move |context, mut args| {
                let mut call_args = Vec::with_capacity(args.len() + 1);
                call_args.push(Value::from_object_ref(receiver.clone()));
                call_args.append(&mut args);
                context.call_function_named(&method_name, call_args)
            }));
        }

        match attribute {
            "__str__" | "__repr__" => {
                let class_name = class_name(self.class.clone());
                let rendered = format!("<{} object>", class_name);
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

    fn invoke(
        &self,
        _receiver: ObjectRef,
        _context: &mut dyn CallContext,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        Err(RuntimeError::ObjectNotCallable {
            type_name: self.type_name().to_string(),
        })
    }
}

fn class_method_name(class: ObjectRef, attribute: &str) -> Option<String> {
    let class_borrow = class.borrow();
    let class_object = class_borrow
        .as_any()
        .downcast_ref::<ClassObject>()
        .expect("instance class must be ClassObject");
    class_object.method_name(attribute).map(str::to_string)
}

fn class_name(class: ObjectRef) -> String {
    let class_borrow = class.borrow();
    let class_object = class_borrow
        .as_any()
        .downcast_ref::<ClassObject>()
        .expect("instance class must be ClassObject");
    class_object.name().to_string()
}
