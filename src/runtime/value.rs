use crate::builtins::BuiltinFunction;
use crate::runtime::bool::{self, BoolObject};
use crate::runtime::callable::{self, BoundMethodObject, BuiltinFunctionObject, FunctionObject};
use crate::runtime::error::RuntimeError;
use crate::runtime::int::{self, IntObject};
use crate::runtime::list::{self};
use crate::runtime::none::NoneObject;
use crate::runtime::object::{BoundMethodCallable, CallTarget, ObjectRef, new_list_object};
use crate::runtime::string::{self, StringObject};
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Copy)]
struct ValueBehavior {
    type_name: &'static str,
    to_output: fn(&Value) -> String,
    is_truthy: fn(&Value) -> bool,
    get_item: fn(&Value, Value) -> Result<Value, RuntimeError>,
    set_item: fn(&Value, Value, Value) -> Result<(), RuntimeError>,
}

#[derive(Clone)]
pub(crate) struct Value {
    object: ObjectRef,
    behavior: &'static ValueBehavior,
    call_target: Option<CallTarget>,
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Value({})", self.type_name())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CallTargetError {
    pub(crate) type_name: String,
}

fn unsupported_operation(operation: &str, type_name: &str) -> RuntimeError {
    RuntimeError::UnsupportedOperation {
        operation: operation.to_string(),
        type_name: type_name.to_string(),
    }
}

fn always_truthy(_value: &Value) -> bool {
    true
}

fn unsupported_get_item(value: &Value, _index: Value) -> Result<Value, RuntimeError> {
    Err(unsupported_operation("__getitem__", value.type_name()))
}

fn unsupported_set_item(
    value: &Value,
    _index: Value,
    _assigned_value: Value,
) -> Result<(), RuntimeError> {
    Err(unsupported_operation("__setitem__", value.type_name()))
}

fn int_to_output(value: &Value) -> String {
    int::try_to_output(value).expect("int behavior must wrap IntObject")
}

fn int_is_truthy(value: &Value) -> bool {
    int::try_is_truthy(value).expect("int behavior must wrap IntObject")
}

fn bool_to_output(value: &Value) -> String {
    bool::try_to_output(value).expect("bool behavior must wrap BoolObject")
}

fn bool_is_truthy(value: &Value) -> bool {
    bool::try_is_truthy(value).expect("bool behavior must wrap BoolObject")
}

fn string_to_output(value: &Value) -> String {
    string::try_to_output(value).expect("string behavior must wrap StringObject")
}

fn string_is_truthy(value: &Value) -> bool {
    string::try_is_truthy(value).expect("string behavior must wrap StringObject")
}

fn list_to_output(value: &Value) -> String {
    list::try_to_output(value).expect("list behavior must wrap ListObject")
}

fn list_is_truthy(value: &Value) -> bool {
    list::try_is_truthy(value).expect("list behavior must wrap ListObject")
}

fn list_get_item(value: &Value, index: Value) -> Result<Value, RuntimeError> {
    list::try_get_item(value, index).expect("list behavior must wrap ListObject")
}

fn list_set_item(value: &Value, index: Value, assigned_value: Value) -> Result<(), RuntimeError> {
    list::try_set_item(value, index, assigned_value).expect("list behavior must wrap ListObject")
}

fn none_to_output(_value: &Value) -> String {
    "None".to_string()
}

fn none_is_truthy(_value: &Value) -> bool {
    false
}

fn builtin_function_to_output(_value: &Value) -> String {
    "<built-in function>".to_string()
}

fn function_to_output(value: &Value) -> String {
    callable::function_to_output(value)
}

fn bound_method_to_output(_value: &Value) -> String {
    "<bound method>".to_string()
}

const INT_BEHAVIOR: ValueBehavior = ValueBehavior {
    type_name: "int",
    to_output: int_to_output,
    is_truthy: int_is_truthy,
    get_item: unsupported_get_item,
    set_item: unsupported_set_item,
};

const BOOL_BEHAVIOR: ValueBehavior = ValueBehavior {
    type_name: "bool",
    to_output: bool_to_output,
    is_truthy: bool_is_truthy,
    get_item: unsupported_get_item,
    set_item: unsupported_set_item,
};

const STRING_BEHAVIOR: ValueBehavior = ValueBehavior {
    type_name: "str",
    to_output: string_to_output,
    is_truthy: string_is_truthy,
    get_item: unsupported_get_item,
    set_item: unsupported_set_item,
};

const NONE_BEHAVIOR: ValueBehavior = ValueBehavior {
    type_name: "NoneType",
    to_output: none_to_output,
    is_truthy: none_is_truthy,
    get_item: unsupported_get_item,
    set_item: unsupported_set_item,
};

const LIST_BEHAVIOR: ValueBehavior = ValueBehavior {
    type_name: "list",
    to_output: list_to_output,
    is_truthy: list_is_truthy,
    get_item: list_get_item,
    set_item: list_set_item,
};

const BUILTIN_FUNCTION_BEHAVIOR: ValueBehavior = ValueBehavior {
    type_name: "builtin_function_or_method",
    to_output: builtin_function_to_output,
    is_truthy: always_truthy,
    get_item: unsupported_get_item,
    set_item: unsupported_set_item,
};

const FUNCTION_BEHAVIOR: ValueBehavior = ValueBehavior {
    type_name: "function",
    to_output: function_to_output,
    is_truthy: always_truthy,
    get_item: unsupported_get_item,
    set_item: unsupported_set_item,
};

const BOUND_METHOD_BEHAVIOR: ValueBehavior = ValueBehavior {
    type_name: "method",
    to_output: bound_method_to_output,
    is_truthy: always_truthy,
    get_item: unsupported_get_item,
    set_item: unsupported_set_item,
};

impl Value {
    fn new(
        object: ObjectRef,
        behavior: &'static ValueBehavior,
        call_target: Option<CallTarget>,
    ) -> Self {
        Self {
            object,
            behavior,
            call_target,
        }
    }

    pub(crate) fn object_ref(&self) -> ObjectRef {
        self.object.clone()
    }

    pub(crate) fn type_name(&self) -> &'static str {
        self.behavior.type_name
    }

    pub(crate) fn to_output(&self) -> String {
        (self.behavior.to_output)(self)
    }

    pub(crate) fn is_truthy(&self) -> bool {
        (self.behavior.is_truthy)(self)
    }

    pub(crate) fn call_target(&self) -> Result<CallTarget, CallTargetError> {
        self.call_target.clone().ok_or_else(|| CallTargetError {
            type_name: self.type_name().to_string(),
        })
    }

    pub(crate) fn get_attribute(&self, attribute: &str) -> Result<Value, RuntimeError> {
        self.object
            .borrow()
            .get_attribute(self.object_ref(), attribute)
    }

    pub(crate) fn get_item(&self, index: Value) -> Result<Value, RuntimeError> {
        (self.behavior.get_item)(self, index)
    }

    pub(crate) fn set_item(&self, index: Value, value: Value) -> Result<(), RuntimeError> {
        (self.behavior.set_item)(self, index, value)
    }

    pub(crate) fn list_object(values: Vec<Value>) -> Self {
        Self::new(new_list_object(values), &LIST_BEHAVIOR, None)
    }

    pub(crate) fn int_object(value: i64) -> Self {
        Self::new(
            Rc::new(RefCell::new(Box::new(IntObject::new(value)))),
            &INT_BEHAVIOR,
            None,
        )
    }

    pub(crate) fn bool_object(value: bool) -> Self {
        Self::new(
            Rc::new(RefCell::new(Box::new(BoolObject::new(value)))),
            &BOOL_BEHAVIOR,
            None,
        )
    }

    pub(crate) fn string_object(value: String) -> Self {
        Self::new(
            Rc::new(RefCell::new(Box::new(StringObject::new(value)))),
            &STRING_BEHAVIOR,
            None,
        )
    }

    pub(crate) fn none_object() -> Self {
        Self::new(
            Rc::new(RefCell::new(Box::new(NoneObject::new()))),
            &NONE_BEHAVIOR,
            None,
        )
    }

    pub(crate) fn builtin_function_object(builtin: BuiltinFunction) -> Self {
        let builtin_object = BuiltinFunctionObject::new(builtin);
        let call_target = builtin_object.call_target();
        Self::new(
            Rc::new(RefCell::new(Box::new(builtin_object))),
            &BUILTIN_FUNCTION_BEHAVIOR,
            Some(call_target),
        )
    }

    pub(crate) fn function_object(name: String) -> Self {
        let function_object = FunctionObject::new(name);
        let call_target = function_object.call_target();
        Self::new(
            Rc::new(RefCell::new(Box::new(function_object))),
            &FUNCTION_BEHAVIOR,
            Some(call_target),
        )
    }

    pub(crate) fn bound_method_object(callable: BoundMethodCallable) -> Self {
        let bound_method_object = BoundMethodObject::new(callable);
        let call_target = bound_method_object.call_target();
        Self::new(
            Rc::new(RefCell::new(Box::new(bound_method_object))),
            &BOUND_METHOD_BEHAVIOR,
            Some(call_target),
        )
    }
}
