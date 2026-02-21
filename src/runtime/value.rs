use crate::builtins::BuiltinFunction;
use crate::runtime::bool::{self, BoolObject};
use crate::runtime::callable::{BuiltinFunctionObject, CallableObject, FunctionObject};
use crate::runtime::class::{ClassObject, InstanceObject};
use crate::runtime::dict::DictObject;
use crate::runtime::error::RuntimeError;
use crate::runtime::int::{self, IntObject};
use crate::runtime::list::ListObject;
use crate::runtime::none::NoneObject;
use crate::runtime::object::{
    BoundMethodCallable, CallContext, CallableId, ObjectRef, new_list_object,
};
use crate::runtime::string::{self, StringObject};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

/// Boxed runtime value shared across interpreter, VM, and JIT paths.
///
/// `Value` stores an object handle plus a small cached kind for common scalar
/// types (`int`, `bool`, `None`) so hot-path checks avoid repeated downcasts.
#[derive(Clone)]
pub(crate) struct Value {
    object: ObjectRef,
    kind: ValueKind,
}

/// Fast-path tag used by `Value` for frequently queried primitive kinds.
#[derive(Clone, Copy)]
enum ValueKind {
    Int(i64),
    Bool(bool),
    None,
    Other,
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Value({})", self.type_name())
    }
}

impl Value {
    fn new(object: ObjectRef) -> Self {
        Self::new_with_kind(object, ValueKind::Other)
    }

    fn new_with_kind(object: ObjectRef, kind: ValueKind) -> Self {
        Self { object, kind }
    }

    pub(crate) fn from_object_ref(object: ObjectRef) -> Self {
        Self::new(object)
    }

    pub(crate) fn object_ref(&self) -> ObjectRef {
        self.object.clone()
    }

    pub(crate) fn type_name(&self) -> &'static str {
        match self.kind {
            ValueKind::Int(_) => "int",
            ValueKind::Bool(_) => "bool",
            ValueKind::None => "NoneType",
            ValueKind::Other => self.object.borrow().type_name(),
        }
    }

    pub(crate) fn as_int(&self) -> Option<i64> {
        if let ValueKind::Int(value) = self.kind {
            return Some(value);
        }
        let object = self.object.borrow();
        object
            .as_any()
            .downcast_ref::<IntObject>()
            .map(IntObject::value)
    }

    pub(crate) fn as_bool(&self) -> Option<bool> {
        if let ValueKind::Bool(value) = self.kind {
            return Some(value);
        }
        let object = self.object.borrow();
        object
            .as_any()
            .downcast_ref::<BoolObject>()
            .map(BoolObject::value)
    }

    pub(crate) fn is_none(&self) -> bool {
        if matches!(self.kind, ValueKind::None) {
            return true;
        }
        let object = self.object.borrow();
        object.as_any().downcast_ref::<NoneObject>().is_some()
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
        if let Some(is_truthy) = self.try_builtin_truthiness() {
            return is_truthy;
        }

        if let Some(bool_value) = self.try_call_magic_method("__bool__", "__bool__") {
            return bool::downcast_bool(&bool_value).expect("__bool__ must return bool");
        }

        if let Some(length_value) = self.try_call_magic_method("__len__", "__len__") {
            return int::downcast_i64(&length_value).expect("__len__ must return int") != 0;
        }

        true
    }

    fn try_builtin_truthiness(&self) -> Option<bool> {
        match self.kind {
            ValueKind::Bool(value) => return Some(value),
            ValueKind::Int(value) => return Some(value != 0),
            ValueKind::None => return Some(false),
            ValueKind::Other => {}
        }
        let object = self.object.borrow();
        if let Some(string) = object.as_any().downcast_ref::<StringObject>() {
            return Some(!string.value().is_empty());
        }
        if let Some(list) = object.as_any().downcast_ref::<ListObject>() {
            return Some(list.__len__() != 0);
        }
        None
    }

    pub(crate) fn get_attribute(&self, attribute: &str) -> Result<Value, RuntimeError> {
        let receiver = self.object_ref();
        let object = receiver.borrow();
        object.get_attribute(receiver.clone(), attribute)
    }

    pub(crate) fn set_attribute(&self, attribute: &str, value: Value) -> Result<(), RuntimeError> {
        let mut object = self.object.borrow_mut();
        object.set_attribute(attribute, value)
    }

    pub(crate) fn call(
        &self,
        context: &mut dyn CallContext,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let receiver = self.object_ref();
        let object = receiver.borrow();
        object.call(receiver.clone(), context, args)
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

    pub(crate) fn hash_key(&self, context: &mut dyn CallContext) -> Result<i64, RuntimeError> {
        if let Some(value) = self.numeric_key_value() {
            return Ok(value);
        }
        if let Some(value) = string::downcast_string(self) {
            return Ok(hash_string(&value));
        }
        if self.is_none() {
            return Ok(0x9e37_79b9_i64);
        }
        if self.is_unhashable_container() {
            return Err(RuntimeError::UnhashableType {
                type_name: self.type_name().to_string(),
            });
        }

        if let Some((has_eq, has_hash)) = self.instance_hash_policy() {
            if has_hash {
                return self.call_hash_method(context);
            }
            if has_eq {
                return Err(RuntimeError::UnhashableType {
                    type_name: self.type_name().to_string(),
                });
            }
            return Ok(self.object_identity_hash());
        }

        match self.try_call_hash_method(context)? {
            Some(hash) => Ok(hash),
            None => Ok(self.object_identity_hash()),
        }
    }

    pub(crate) fn key_equals(
        &self,
        context: &mut dyn CallContext,
        rhs: &Value,
    ) -> Result<bool, RuntimeError> {
        if Rc::ptr_eq(&self.object, &rhs.object) {
            return Ok(true);
        }
        if let (Some(left), Some(right)) = (self.numeric_key_value(), rhs.numeric_key_value()) {
            return Ok(left == right);
        }
        if let (Some(left), Some(right)) =
            (string::downcast_string(self), string::downcast_string(rhs))
        {
            return Ok(left == right);
        }
        if self.is_none() || rhs.is_none() {
            return Ok(self.is_none() && rhs.is_none());
        }

        if let Some(equal) = self.try_call_eq_method(context, rhs)? {
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
        Self::new(new_list_object(values))
    }

    pub(crate) fn int_object(value: i64) -> Self {
        Self::new_with_kind(
            Rc::new(RefCell::new(Box::new(IntObject::new(value)))),
            ValueKind::Int(value),
        )
    }

    pub(crate) fn bool_object(value: bool) -> Self {
        Self::new_with_kind(
            Rc::new(RefCell::new(Box::new(BoolObject::new(value)))),
            ValueKind::Bool(value),
        )
    }

    pub(crate) fn string_object(value: String) -> Self {
        Self::new(Rc::new(RefCell::new(Box::new(StringObject::new(value)))))
    }

    pub(crate) fn none_object() -> Self {
        Self::new_with_kind(
            Rc::new(RefCell::new(Box::new(NoneObject::new()))),
            ValueKind::None,
        )
    }

    pub(crate) fn builtin_function_object(builtin: BuiltinFunction) -> Self {
        let builtin_object = BuiltinFunctionObject::new(builtin);
        Self::new(Rc::new(RefCell::new(Box::new(builtin_object))))
    }

    pub(crate) fn function_object(name: String, callable_id: CallableId) -> Self {
        let function_object = FunctionObject::new(name, callable_id);
        Self::new(Rc::new(RefCell::new(Box::new(function_object))))
    }

    pub(crate) fn class_object(name: String, methods: HashMap<String, Value>) -> Self {
        let class_object = ClassObject::new(name, methods);
        Self::new(Rc::new(RefCell::new(Box::new(class_object))))
    }

    pub(crate) fn dict_object_with_context(
        entries: Vec<(Value, Value)>,
        context: &mut dyn CallContext,
    ) -> Result<Self, RuntimeError> {
        let dict_object = DictObject::new(entries, context)?;
        Ok(Self::new(Rc::new(RefCell::new(Box::new(dict_object)))))
    }

    pub(crate) fn instance_object(class: ObjectRef) -> Self {
        let instance_object = InstanceObject::new(class);
        Self::new(Rc::new(RefCell::new(Box::new(instance_object))))
    }

    pub(crate) fn bound_method_object(callable: BoundMethodCallable) -> Self {
        let bound_method_object = CallableObject::bound_method(callable);
        Self::new(Rc::new(RefCell::new(Box::new(bound_method_object))))
    }

    pub(crate) fn method_wrapper_object(callable: BoundMethodCallable) -> Self {
        let method_wrapper_object = CallableObject::method_wrapper(callable);
        Self::new(Rc::new(RefCell::new(Box::new(method_wrapper_object))))
    }

    /// Calls a zero-argument magic method in a context that forbids nested dispatch.
    fn call_magic_method(
        &self,
        callee: Value,
        args: Vec<Value>,
        operation: &str,
    ) -> Result<Value, RuntimeError> {
        let mut context = NoopCallContext;
        self.call_bound_method(callee, &mut context, args, operation)
    }

    /// Resolves an operator magic method and executes it with one rhs argument.
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

    /// Invokes a previously-resolved callable and maps non-callable errors to operation errors.
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
            .call(object_ref.clone(), context, args)
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

    /// Best-effort helper for optional magic methods used by truthiness/string rendering.
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

    fn numeric_key_value(&self) -> Option<i64> {
        if let Some(value) = self.as_bool() {
            return Some(if value { 1 } else { 0 });
        }
        self.as_int()
    }

    fn object_identity_hash(&self) -> i64 {
        Rc::as_ptr(&self.object) as usize as i64
    }

    fn is_unhashable_container(&self) -> bool {
        let object = self.object.borrow();
        object.as_any().downcast_ref::<ListObject>().is_some()
            || object.as_any().downcast_ref::<DictObject>().is_some()
    }

    fn instance_hash_policy(&self) -> Option<(bool, bool)> {
        let object = self.object.borrow();
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

    fn call_hash_method(&self, context: &mut dyn CallContext) -> Result<i64, RuntimeError> {
        let callee = self.get_attribute("__hash__")?;
        let result = self.call_bound_method(callee, context, Vec::new(), "__hash__")?;
        let Some(hash) = result.as_int() else {
            return Err(RuntimeError::InvalidArgumentType {
                operation: "__hash__".to_string(),
                argument: "return".to_string(),
                expected: "int".to_string(),
                got: result.type_name().to_string(),
            });
        };
        Ok(hash)
    }

    fn try_call_hash_method(
        &self,
        context: &mut dyn CallContext,
    ) -> Result<Option<i64>, RuntimeError> {
        let callee = match self.get_attribute("__hash__") {
            Ok(callee) => callee,
            Err(RuntimeError::UnknownAttribute { .. }) => return Ok(None),
            Err(error) => return Err(error),
        };
        let result = self.call_bound_method(callee, context, Vec::new(), "__hash__")?;
        let Some(hash) = result.as_int() else {
            return Err(RuntimeError::InvalidArgumentType {
                operation: "__hash__".to_string(),
                argument: "return".to_string(),
                expected: "int".to_string(),
                got: result.type_name().to_string(),
            });
        };
        Ok(Some(hash))
    }

    fn try_call_eq_method(
        &self,
        context: &mut dyn CallContext,
        rhs: &Value,
    ) -> Result<Option<bool>, RuntimeError> {
        let callee = match self.get_attribute("__eq__") {
            Ok(callee) => callee,
            Err(RuntimeError::UnknownAttribute { .. }) => return Ok(None),
            Err(error) => return Err(error),
        };
        let result = self.call_bound_method(callee, context, vec![rhs.clone()], "__eq__")?;
        let Some(equal) = result.as_bool() else {
            return Err(RuntimeError::InvalidArgumentType {
                operation: "__eq__".to_string(),
                argument: "return".to_string(),
                expected: "bool".to_string(),
                got: result.type_name().to_string(),
            });
        };
        Ok(Some(equal))
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

fn hash_string(value: &str) -> i64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish() as i64
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
