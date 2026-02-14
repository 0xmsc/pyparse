use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

use crate::builtins::BuiltinFunction;
use crate::runtime::list::{ListError, ListObject};
use crate::runtime::value::Value;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum AttributeError {
    UnknownAttribute {
        attribute: String,
        type_name: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum MethodError {
    ArityMismatch {
        method: String,
        expected: usize,
        found: usize,
    },
    UnknownMethod {
        method: String,
        type_name: String,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum BinaryOpError {
    ExpectedIntegerType { got: String },
}

#[derive(Debug, Clone)]
pub(crate) enum CallTarget {
    Builtin(BuiltinFunction),
    Function(String),
    BoundMethod { receiver: ObjectRef, method: String },
}

pub(crate) trait RuntimeObject: std::fmt::Debug {
    fn as_any(&self) -> &dyn Any;
    fn type_name(&self) -> &'static str;
    fn is_truthy(&self) -> bool;
    fn to_output(&self, render_value: &dyn Fn(&Value) -> String) -> String;
    fn get_attribute_method_name(&self, attribute: &str) -> Result<String, AttributeError>;
    fn len(&self) -> Result<usize, ListError>;
    fn get_item(&self, index: Value) -> Result<Value, ListError>;
    fn set_item(&mut self, index: Value, value: Value) -> Result<(), ListError>;
    fn call_method(&mut self, method: &str, args: Vec<Value>) -> Result<(), MethodError>;
    fn add(&self, rhs: &Value) -> Result<Value, BinaryOpError>;
    fn sub(&self, rhs: &Value) -> Result<Value, BinaryOpError>;
    fn lt(&self, rhs: &Value) -> Result<Value, BinaryOpError>;
    fn call_target(&self) -> Option<CallTarget> {
        None
    }
}

pub(crate) type ObjectRef = Rc<RefCell<Box<dyn RuntimeObject>>>;

pub(crate) fn new_list_object(values: Vec<Value>) -> ObjectRef {
    Rc::new(RefCell::new(Box::new(ListObject::new(values))))
}
