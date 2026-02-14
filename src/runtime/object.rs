use std::cell::RefCell;
use std::rc::Rc;

use crate::runtime::list::{ListError, ListObject};

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

pub(crate) trait RuntimeObject<Value>: std::fmt::Debug {
    fn type_name(&self) -> &'static str;
    fn is_truthy(&self) -> bool;
    fn to_output(&self, render_value: &dyn Fn(&Value) -> String) -> String;
    fn get_attribute_method_name(&self, attribute: &str) -> Result<String, AttributeError>;
    fn len(&self) -> usize;
    fn get_item(&self, index: i64) -> Result<Value, ListError>;
    fn set_item(&mut self, index: i64, value: Value) -> Result<(), ListError>;
    fn call_method(&mut self, method: &str, args: Vec<Value>) -> Result<(), MethodError>;
}

pub(crate) type ObjectRef<Value> = Rc<RefCell<Box<dyn RuntimeObject<Value>>>>;

#[derive(Debug, Clone)]
pub(crate) struct ObjectWrapper<Value> {
    object: ObjectRef<Value>,
}

impl<Value> ObjectWrapper<Value> {
    pub(crate) fn new(object: ObjectRef<Value>) -> Self {
        Self { object }
    }

    pub(crate) fn get_attribute_method_name(
        &self,
        attribute: &str,
    ) -> Result<String, AttributeError> {
        self.object.borrow().get_attribute_method_name(attribute)
    }

    pub(crate) fn len(&self) -> usize {
        self.object.borrow().len()
    }

    pub(crate) fn set_item(&self, index: i64, value: Value) -> Result<(), ListError> {
        self.object.borrow_mut().set_item(index, value)
    }

    pub(crate) fn call_method(&self, method: &str, args: Vec<Value>) -> Result<(), MethodError> {
        self.object.borrow_mut().call_method(method, args)
    }
}

impl<Value: Clone> ObjectWrapper<Value> {
    pub(crate) fn get_item(&self, index: i64) -> Result<Value, ListError> {
        self.object.borrow().get_item(index)
    }
}

pub(crate) fn new_list_object<Value: Clone + std::fmt::Debug + 'static>(
    values: Vec<Value>,
) -> ObjectRef<Value> {
    Rc::new(RefCell::new(Box::new(ListObject::new(values))))
}
