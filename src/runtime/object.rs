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
    ListOperation(ListError),
    UnknownMethod {
        method: String,
        type_name: String,
    },
}

#[derive(Debug, Clone)]
pub(crate) enum CallTarget {
    Builtin(BuiltinFunction),
    Function(String),
    BoundMethod { receiver: ObjectRef, method: String },
}

pub(crate) type ObjectRef = Rc<RefCell<Box<dyn RuntimeObject>>>;

pub(crate) trait RuntimeObject: std::fmt::Debug + Any {
    fn get_attribute(&self, receiver: ObjectRef, attribute: &str) -> Result<Value, AttributeError>;
}

pub(crate) fn new_list_object(values: Vec<Value>) -> ObjectRef {
    Rc::new(RefCell::new(Box::new(ListObject::new(values))))
}
