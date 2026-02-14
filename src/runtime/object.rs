use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

use crate::builtins::BuiltinFunction;
use crate::runtime::error::RuntimeError;
use crate::runtime::list::ListObject;
use crate::runtime::value::Value;

#[derive(Debug, Clone)]
pub(crate) enum CallTarget {
    Builtin(BuiltinFunction),
    Function(String),
    BoundMethod { receiver: ObjectRef, method: String },
}

pub(crate) type ObjectRef = Rc<RefCell<Box<dyn RuntimeObject>>>;

pub(crate) trait RuntimeObject: std::fmt::Debug + Any {
    fn get_attribute(&self, receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError>;
}

pub(crate) fn new_list_object(values: Vec<Value>) -> ObjectRef {
    Rc::new(RefCell::new(Box::new(ListObject::new(values))))
}
