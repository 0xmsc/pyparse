use crate::builtins::BuiltinFunction;
use crate::runtime::bool as runtime_bool;
use crate::runtime::callable::{BuiltinFunctionObject, CallableObject, FunctionObject};
use crate::runtime::class::{ClassObject, InstanceObject};
use crate::runtime::dict::DictObject;
use crate::runtime::error::RuntimeError;
use crate::runtime::int as runtime_int;
use crate::runtime::list::ListObject;
use crate::runtime::object::{
    BoundMethodCallable, CallContext, CallableId, ObjectRef, new_list_object,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

mod r#bool;
mod int;
mod none;
mod r#str;

/// Boxed runtime value shared across interpreter, VM, and JIT paths.
#[derive(Clone)]
pub(crate) enum Value {
    Int(i64),
    Bool(bool),
    Str(String),
    None,
    Object(ObjectRef),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Value({})", self.type_name())
    }
}

impl Value {
    pub(crate) fn from_object_ref(object: ObjectRef) -> Self {
        Self::Object(object)
    }

    pub(crate) fn type_name(&self) -> &'static str {
        match self {
            Value::Int(_) => "int",
            Value::Bool(_) => "bool",
            Value::Str(_) => "str",
            Value::None => "NoneType",
            Value::Object(object) => object.borrow().type_name(),
        }
    }

    pub(crate) fn as_int(&self) -> Option<i64> {
        if let Value::Int(value) = self {
            return Some(*value);
        }
        None
    }

    pub(crate) fn as_bool(&self) -> Option<bool> {
        if let Value::Bool(value) = self {
            return Some(*value);
        }
        None
    }

    pub(crate) fn as_str(&self) -> Option<&str> {
        if let Value::Str(value) = self {
            return Some(value);
        }
        None
    }

    pub(crate) fn is_none(&self) -> bool {
        matches!(self, Value::None)
    }

    pub(crate) fn to_output(&self) -> String {
        match self {
            Value::Int(value) => value.to_string(),
            Value::Bool(value) => {
                if *value {
                    "True".to_string()
                } else {
                    "False".to_string()
                }
            }
            Value::Str(value) => value.clone(),
            Value::None => "None".to_string(),
            Value::Object(_) => {
                if let Some(str_value) = self.try_call_magic_method("__str__", "__str__") {
                    return str_value
                        .as_str()
                        .expect("__str__ must return str")
                        .to_string();
                }

                if let Some(repr_value) = self.try_call_magic_method("__repr__", "__repr__") {
                    return repr_value
                        .as_str()
                        .expect("__repr__ must return str")
                        .to_string();
                }

                panic!("missing __str__ and __repr__ for {}", self.type_name());
            }
        }
    }

    pub(crate) fn is_truthy(&self) -> bool {
        match self {
            Value::Int(value) => return *value != 0,
            Value::Bool(value) => return *value,
            Value::Str(value) => return !value.is_empty(),
            Value::None => return false,
            Value::Object(_) => {}
        }

        if let Some(is_truthy) = self.try_builtin_truthiness() {
            return is_truthy;
        }

        if let Some(bool_value) = self.try_call_magic_method("__bool__", "__bool__") {
            return runtime_bool::downcast_bool(&bool_value).expect("__bool__ must return bool");
        }

        if let Some(length_value) = self.try_call_magic_method("__len__", "__len__") {
            return runtime_int::downcast_i64(&length_value).expect("__len__ must return int") != 0;
        }

        true
    }

    fn try_builtin_truthiness(&self) -> Option<bool> {
        let Value::Object(object_ref) = self else {
            return None;
        };
        let object = object_ref.borrow();
        if let Some(list) = object.as_any().downcast_ref::<ListObject>() {
            return Some(list.__len__() != 0);
        }
        None
    }

    pub(crate) fn get_attribute(&self, attribute: &str) -> Result<Value, RuntimeError> {
        match self {
            Value::Int(value) => int::attribute(*value, attribute),
            Value::Bool(value) => r#bool::attribute(*value, attribute),
            Value::Str(value) => r#str::attribute(value, attribute),
            Value::None => none::attribute(attribute),
            Value::Object(receiver) => {
                let object = receiver.borrow();
                object.get_attribute(receiver.clone(), attribute)
            }
        }
    }

    pub(crate) fn set_attribute(&self, attribute: &str, value: Value) -> Result<(), RuntimeError> {
        match self {
            Value::Object(object) => {
                let mut object = object.borrow_mut();
                object.set_attribute(attribute, value)
            }
            _ => Err(RuntimeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: self.type_name().to_string(),
            }),
        }
    }

    pub(crate) fn call(
        &self,
        context: &mut dyn CallContext,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match self {
            Value::Object(receiver) => {
                let object = receiver.borrow();
                object.call(receiver.clone(), context, args)
            }
            _ => Err(RuntimeError::ObjectNotCallable {
                type_name: self.type_name().to_string(),
            }),
        }
    }

    pub(crate) fn add(
        &self,
        context: &mut dyn CallContext,
        rhs: Value,
    ) -> Result<Value, RuntimeError> {
        if let (Some(left_int), Some(right_int)) = (
            runtime_int::downcast_i64(self),
            runtime_int::downcast_i64(&rhs),
        ) {
            return Ok(Value::int_object(left_int + right_int));
        }
        self.call_binary_operator(context, "__add__", rhs)
    }

    pub(crate) fn sub(
        &self,
        context: &mut dyn CallContext,
        rhs: Value,
    ) -> Result<Value, RuntimeError> {
        if let (Some(left_int), Some(right_int)) = (
            runtime_int::downcast_i64(self),
            runtime_int::downcast_i64(&rhs),
        ) {
            return Ok(Value::int_object(left_int - right_int));
        }
        self.call_binary_operator(context, "__sub__", rhs)
    }

    pub(crate) fn less_than(
        &self,
        context: &mut dyn CallContext,
        rhs: Value,
    ) -> Result<Value, RuntimeError> {
        if let (Some(left_int), Some(right_int)) = (
            runtime_int::downcast_i64(self),
            runtime_int::downcast_i64(&rhs),
        ) {
            return Ok(Value::bool_object(left_int < right_int));
        }
        self.call_binary_operator(context, "__lt__", rhs)
    }

    pub(crate) fn hash_key(&self, context: &mut dyn CallContext) -> Result<i64, RuntimeError> {
        if let Some(number) = numeric_key_value(self) {
            return Ok(number);
        }
        if let Some(hash) = r#str::try_hash_key(self) {
            return Ok(hash);
        }
        if self.is_none() {
            return Ok(0x9e37_79b9_i64);
        }
        if is_unhashable_container(self) {
            return Err(RuntimeError::UnhashableType {
                type_name: self.type_name().to_string(),
            });
        }

        if let Some((has_eq, has_hash)) = instance_hash_policy(self) {
            if has_hash {
                return call_hash_method(self, context);
            }
            if has_eq {
                return Err(RuntimeError::UnhashableType {
                    type_name: self.type_name().to_string(),
                });
            }
            return Ok(object_identity_hash(self));
        }

        match try_call_hash_method(self, context)? {
            Some(hash) => Ok(hash),
            None => Ok(object_identity_hash(self)),
        }
    }

    pub(crate) fn key_equals(
        &self,
        context: &mut dyn CallContext,
        rhs: &Value,
    ) -> Result<bool, RuntimeError> {
        if let (Value::Object(left), Value::Object(right)) = (self, rhs)
            && Rc::ptr_eq(left, right)
        {
            return Ok(true);
        }
        if let (Some(left), Some(right)) = (numeric_key_value(self), numeric_key_value(rhs)) {
            return Ok(left == right);
        }
        if let Some(equal) = r#str::try_key_equals(self, rhs) {
            return Ok(equal);
        }
        if self.is_none() || rhs.is_none() {
            return Ok(self.is_none() && rhs.is_none());
        }

        if let Some(equal) = try_call_eq_method(self, context, rhs)? {
            return Ok(equal);
        }
        Ok(false)
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
        Self::Object(new_list_object(values))
    }

    pub(crate) fn int_object(value: i64) -> Self {
        Self::Int(value)
    }

    pub(crate) fn bool_object(value: bool) -> Self {
        Self::Bool(value)
    }

    pub(crate) fn string_object(value: String) -> Self {
        Self::Str(value)
    }

    pub(crate) fn none_object() -> Self {
        Self::None
    }

    pub(crate) fn builtin_function_object(builtin: BuiltinFunction) -> Self {
        let builtin_object = BuiltinFunctionObject::new(builtin);
        Self::Object(Rc::new(RefCell::new(Box::new(builtin_object))))
    }

    pub(crate) fn function_object(name: String, callable_id: CallableId) -> Self {
        let function_object = FunctionObject::new(name, callable_id);
        Self::Object(Rc::new(RefCell::new(Box::new(function_object))))
    }

    pub(crate) fn class_object(name: String, methods: HashMap<String, Value>) -> Self {
        let class_object = ClassObject::new(name, methods);
        Self::Object(Rc::new(RefCell::new(Box::new(class_object))))
    }

    pub(crate) fn dict_object_with_context(
        entries: Vec<(Value, Value)>,
        context: &mut dyn CallContext,
    ) -> Result<Self, RuntimeError> {
        let dict_object = DictObject::new(entries, context)?;
        Ok(Self::Object(Rc::new(RefCell::new(Box::new(dict_object)))))
    }

    pub(crate) fn instance_object(class: ObjectRef) -> Self {
        let instance_object = InstanceObject::new(class);
        Self::Object(Rc::new(RefCell::new(Box::new(instance_object))))
    }

    pub(crate) fn bound_method_object(callable: BoundMethodCallable) -> Self {
        let bound_method_object = CallableObject::bound_method(callable);
        Self::Object(Rc::new(RefCell::new(Box::new(bound_method_object))))
    }

    pub(crate) fn method_wrapper_object(callable: BoundMethodCallable) -> Self {
        let method_wrapper_object = CallableObject::method_wrapper(callable);
        Self::Object(Rc::new(RefCell::new(Box::new(method_wrapper_object))))
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

    pub(super) fn call_bound_method(
        &self,
        callee: Value,
        context: &mut dyn CallContext,
        args: Vec<Value>,
        operation: &str,
    ) -> Result<Value, RuntimeError> {
        let callee_type_name = callee.type_name().to_string();
        callee.call(context, args).map_err(|error| match error {
            RuntimeError::ObjectNotCallable { .. } if operation != "__call__" => {
                RuntimeError::UnsupportedOperation {
                    operation: operation.to_string(),
                    type_name: callee_type_name,
                }
            }
            other => other,
        })
    }

    pub(super) fn try_call_magic_method(&self, attribute: &str, operation: &str) -> Option<Value> {
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

/// Minimal call context used when invoking magic methods that must not recurse
/// into builtin/function dispatch.
struct NoopCallContext;

impl CallContext for NoopCallContext {
    fn call_callable(
        &mut self,
        _callable_id: &CallableId,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        panic!("call requested without runtime call context")
    }
}

pub(super) fn expect_unary_method_args<'a>(
    method: &str,
    args: &'a [Value],
) -> Result<&'a Value, RuntimeError> {
    RuntimeError::expect_method_arity(method, 1, args.len())?;
    Ok(args.first().expect("len checked above"))
}

pub(super) fn invalid_argument_type(
    operation: &str,
    argument: &str,
    expected: &str,
    got: &Value,
) -> RuntimeError {
    RuntimeError::InvalidArgumentType {
        operation: operation.to_string(),
        argument: argument.to_string(),
        expected: expected.to_string(),
        got: got.type_name().to_string(),
    }
}

fn expect_int_return(operation: &str, value: &Value) -> Result<i64, RuntimeError> {
    let Some(integer) = value.as_int() else {
        return Err(invalid_argument_type(operation, "return", "int", value));
    };
    Ok(integer)
}

fn expect_bool_return(operation: &str, value: &Value) -> Result<bool, RuntimeError> {
    let Some(boolean) = value.as_bool() else {
        return Err(invalid_argument_type(operation, "return", "bool", value));
    };
    Ok(boolean)
}

fn numeric_key_value(value: &Value) -> Option<i64> {
    if let Some(boolean) = value.as_bool() {
        return Some(if boolean { 1 } else { 0 });
    }
    value.as_int()
}

fn object_identity_hash(value: &Value) -> i64 {
    let Value::Object(object) = value else {
        panic!("object_identity_hash only valid for object values");
    };
    Rc::as_ptr(object) as usize as i64
}

fn is_unhashable_container(value: &Value) -> bool {
    let Value::Object(object_ref) = value else {
        return false;
    };
    let object = object_ref.borrow();
    object.as_any().downcast_ref::<ListObject>().is_some()
        || object.as_any().downcast_ref::<DictObject>().is_some()
}

fn instance_hash_policy(value: &Value) -> Option<(bool, bool)> {
    let Value::Object(object_ref) = value else {
        return None;
    };
    let object = object_ref.borrow();
    let instance = object.as_any().downcast_ref::<InstanceObject>()?;
    let class_ref = instance.class_ref();
    drop(object);

    let class_borrow = class_ref.borrow();
    let class = class_borrow
        .as_any()
        .downcast_ref::<ClassObject>()
        .expect("instance class must be ClassObject");
    Some((class.has_method("__eq__"), class.has_method("__hash__")))
}

fn call_hash_method(value: &Value, context: &mut dyn CallContext) -> Result<i64, RuntimeError> {
    let callee = value.get_attribute("__hash__")?;
    call_hash_callee(value, context, callee)
}

fn try_call_hash_method(
    value: &Value,
    context: &mut dyn CallContext,
) -> Result<Option<i64>, RuntimeError> {
    let callee = match value.get_attribute("__hash__") {
        Ok(callee) => callee,
        Err(RuntimeError::UnknownAttribute { .. }) => return Ok(None),
        Err(error) => return Err(error),
    };
    Ok(Some(call_hash_callee(value, context, callee)?))
}

fn try_call_eq_method(
    lhs: &Value,
    context: &mut dyn CallContext,
    rhs: &Value,
) -> Result<Option<bool>, RuntimeError> {
    let callee = match lhs.get_attribute("__eq__") {
        Ok(callee) => callee,
        Err(RuntimeError::UnknownAttribute { .. }) => return Ok(None),
        Err(error) => return Err(error),
    };
    let result = lhs.call_bound_method(callee, context, vec![rhs.clone()], "__eq__")?;
    Ok(Some(expect_bool_return("__eq__", &result)?))
}

fn call_hash_callee(
    value: &Value,
    context: &mut dyn CallContext,
    callee: Value,
) -> Result<i64, RuntimeError> {
    let result = value.call_bound_method(callee, context, Vec::new(), "__hash__")?;
    expect_int_return("__hash__", &result)
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
