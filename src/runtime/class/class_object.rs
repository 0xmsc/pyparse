use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
use crate::runtime::object::{CallContext, ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;
use std::collections::HashMap;

/// Runtime representation of a Python-like class object.
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

impl RuntimeObject for ClassObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "type"
    }

    fn get_attribute(&self, receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match attribute {
            "__call__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |context, args| {
                    let object = receiver.borrow();
                    object.call(receiver.clone(), context, args)
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

    fn call(
        &self,
        receiver: ObjectRef,
        context: &mut dyn CallContext,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let instance = Value::instance_object(receiver.clone());
        if self.method("__init__").is_some() {
            let init = instance.get_attribute("__init__")?;
            init.call(context, args)?;
            return Ok(instance);
        }
        RuntimeError::expect_function_arity("__init__", 0, args.len())?;
        Ok(instance)
    }
}
