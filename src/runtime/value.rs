use crate::builtins::BuiltinFunction;
use crate::runtime::bool::{self, BoolObject};
use crate::runtime::callable::{
    BoundMethodObject, BuiltinFunctionObject, FunctionObject, MethodWrapperObject,
};
use crate::runtime::class::{ClassObject, InstanceObject};
use crate::runtime::error::RuntimeError;
use crate::runtime::int::{self, IntObject};
use crate::runtime::none::NoneObject;
use crate::runtime::object::{BoundMethodCallable, CallContext, ObjectRef, new_list_object};
use crate::runtime::string::{self, StringObject};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub(crate) struct Value {
    object: ObjectRef,
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Value({})", self.type_name())
    }
}

impl Value {
    fn new(object: ObjectRef) -> Self {
        Self { object }
    }

    pub(crate) fn from_object_ref(object: ObjectRef) -> Self {
        Self::new(object)
    }

    pub(crate) fn object_ref(&self) -> ObjectRef {
        self.object.clone()
    }

    pub(crate) fn type_name(&self) -> &'static str {
        self.object.borrow().type_name()
    }

    pub(crate) fn to_output(&self) -> String {
        if let Some(str_value) = self.try_call_magic_method("__str__", "__str__") {
            return string::downcast_string(&str_value).expect("__str__ must return str");
        }

        if let Some(repr_value) = self.try_call_magic_method("__repr__", "__repr__") {
            return string::downcast_string(&repr_value).expect("__repr__ must return str");
        }

        panic!("missing __str__ and __repr__ for {}", self.type_name());
    }

    pub(crate) fn is_truthy(&self) -> bool {
        if let Some(bool_value) = self.try_call_magic_method("__bool__", "__bool__") {
            return bool::downcast_bool(&bool_value).expect("__bool__ must return bool");
        }

        if let Some(length_value) = self.try_call_magic_method("__len__", "__len__") {
            return int::downcast_i64(&length_value).expect("__len__ must return int") != 0;
        }

        true
    }

    pub(crate) fn get_attribute(&self, attribute: &str) -> Result<Value, RuntimeError> {
        self.object
            .borrow()
            .get_attribute(self.object_ref(), attribute)
    }

    pub(crate) fn call(
        &self,
        context: &mut dyn CallContext,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let callee = self
            .get_attribute("__call__")
            .map_err(|error| match error {
                RuntimeError::UnknownAttribute { .. } => RuntimeError::ObjectNotCallable {
                    type_name: self.type_name().to_string(),
                },
                other => other,
            })?;
        self.call_bound_method(callee, context, args, "__call__")
    }

    pub(crate) fn add(
        &self,
        context: &mut dyn CallContext,
        rhs: Value,
    ) -> Result<Value, RuntimeError> {
        if let (Some(left_int), Some(right_int)) =
            (int::downcast_i64(self), int::downcast_i64(&rhs))
        {
            return Ok(Value::int_object(left_int + right_int));
        }
        self.call_binary_operator(context, "__add__", rhs)
    }

    pub(crate) fn sub(
        &self,
        context: &mut dyn CallContext,
        rhs: Value,
    ) -> Result<Value, RuntimeError> {
        if let (Some(left_int), Some(right_int)) =
            (int::downcast_i64(self), int::downcast_i64(&rhs))
        {
            return Ok(Value::int_object(left_int - right_int));
        }
        self.call_binary_operator(context, "__sub__", rhs)
    }

    pub(crate) fn less_than(
        &self,
        context: &mut dyn CallContext,
        rhs: Value,
    ) -> Result<Value, RuntimeError> {
        if let (Some(left_int), Some(right_int)) =
            (int::downcast_i64(self), int::downcast_i64(&rhs))
        {
            return Ok(Value::bool_object(left_int < right_int));
        }
        self.call_binary_operator(context, "__lt__", rhs)
    }

    pub(crate) fn get_item_with_context(
        &self,
        context: &mut dyn CallContext,
        index: Value,
    ) -> Result<Value, RuntimeError> {
        let callee = self
            .get_attribute("__getitem__")
            .map_err(|error| map_unknown_attribute_to_unsupported(error, "__getitem__"))?;
        self.call_bound_method(callee, context, vec![index], "__getitem__")
    }

    pub(crate) fn set_item_with_context(
        &self,
        context: &mut dyn CallContext,
        index: Value,
        value: Value,
    ) -> Result<(), RuntimeError> {
        let callee = self
            .get_attribute("__setitem__")
            .map_err(|error| map_unknown_attribute_to_unsupported(error, "__setitem__"))?;
        self.call_bound_method(callee, context, vec![index, value], "__setitem__")?;
        Ok(())
    }

    pub(crate) fn len(&self) -> Result<Value, RuntimeError> {
        let callee = self
            .get_attribute("__len__")
            .map_err(|error| map_unknown_attribute_to_unsupported(error, "len"))?;
        self.call_magic_method(callee, Vec::new(), "len")
    }

    pub(crate) fn list_object(values: Vec<Value>) -> Self {
        Self::new(new_list_object(values))
    }

    pub(crate) fn int_object(value: i64) -> Self {
        Self::new(Rc::new(RefCell::new(Box::new(IntObject::new(value)))))
    }

    pub(crate) fn bool_object(value: bool) -> Self {
        Self::new(Rc::new(RefCell::new(Box::new(BoolObject::new(value)))))
    }

    pub(crate) fn string_object(value: String) -> Self {
        Self::new(Rc::new(RefCell::new(Box::new(StringObject::new(value)))))
    }

    pub(crate) fn none_object() -> Self {
        Self::new(Rc::new(RefCell::new(Box::new(NoneObject::new()))))
    }

    pub(crate) fn builtin_function_object(builtin: BuiltinFunction) -> Self {
        let builtin_object = BuiltinFunctionObject::new(builtin);
        Self::new(Rc::new(RefCell::new(Box::new(builtin_object))))
    }

    pub(crate) fn function_object(name: String) -> Self {
        let function_object = FunctionObject::new(name);
        Self::new(Rc::new(RefCell::new(Box::new(function_object))))
    }

    pub(crate) fn class_object(name: String, methods: HashMap<String, Value>) -> Self {
        let class_object = ClassObject::new(name, methods);
        Self::new(Rc::new(RefCell::new(Box::new(class_object))))
    }

    pub(crate) fn instance_object(class: ObjectRef) -> Self {
        let instance_object = InstanceObject::new(class);
        Self::new(Rc::new(RefCell::new(Box::new(instance_object))))
    }

    pub(crate) fn bound_method_object(callable: BoundMethodCallable) -> Self {
        let bound_method_object = BoundMethodObject::new(callable);
        Self::new(Rc::new(RefCell::new(Box::new(bound_method_object))))
    }

    pub(crate) fn method_wrapper_object(callable: BoundMethodCallable) -> Self {
        let method_wrapper_object = MethodWrapperObject::new(callable);
        Self::new(Rc::new(RefCell::new(Box::new(method_wrapper_object))))
    }

    fn call_magic_method(
        &self,
        callee: Value,
        args: Vec<Value>,
        operation: &str,
    ) -> Result<Value, RuntimeError> {
        let mut context = NoopCallContext;
        self.call_bound_method(callee, &mut context, args, operation)
    }

    fn call_binary_operator(
        &self,
        context: &mut dyn CallContext,
        operation: &str,
        rhs: Value,
    ) -> Result<Value, RuntimeError> {
        let callee = self
            .get_attribute(operation)
            .map_err(|error| map_unknown_attribute_to_unsupported(error, operation))?;
        self.call_bound_method(callee, context, vec![rhs], operation)
    }

    fn call_bound_method(
        &self,
        callee: Value,
        context: &mut dyn CallContext,
        args: Vec<Value>,
        operation: &str,
    ) -> Result<Value, RuntimeError> {
        let callee_type_name = callee.type_name().to_string();
        let object_ref = callee.object_ref();
        object_ref
            .borrow()
            .invoke(object_ref.clone(), context, args)
            .map_err(|error| match error {
                RuntimeError::ObjectNotCallable { .. } if operation != "__call__" => {
                    RuntimeError::UnsupportedOperation {
                        operation: operation.to_string(),
                        type_name: callee_type_name,
                    }
                }
                other => other,
            })
    }

    fn try_call_magic_method(&self, attribute: &str, operation: &str) -> Option<Value> {
        let callee = match self.get_attribute(attribute) {
            Ok(callee) => callee,
            Err(RuntimeError::UnknownAttribute { .. }) => return None,
            Err(error) => panic!("failed to resolve {attribute}: {error}"),
        };
        Some(
            self.call_magic_method(callee, Vec::new(), operation)
                .unwrap_or_else(|error| panic!("failed to call {attribute}: {error}")),
        )
    }
}

struct NoopCallContext;

impl CallContext for NoopCallContext {
    fn call_builtin(
        &mut self,
        _builtin: BuiltinFunction,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        panic!("builtin call requested without runtime call context")
    }

    fn call_function_named(
        &mut self,
        _name: &str,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        panic!("function call requested without runtime call context")
    }
}

fn map_unknown_attribute_to_unsupported(error: RuntimeError, operation: &str) -> RuntimeError {
    match error {
        RuntimeError::UnknownAttribute {
            attribute: _,
            type_name,
        } => RuntimeError::UnsupportedOperation {
            operation: operation.to_string(),
            type_name,
        },
        other => other,
    }
}
