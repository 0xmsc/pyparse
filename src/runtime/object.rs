use std::cell::RefCell;
use std::rc::Rc;

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

pub(crate) trait RuntimeObject: std::fmt::Debug {
    fn type_name(&self) -> &'static str;
    fn is_truthy(&self) -> bool;
    fn to_output(&self, render_value: &dyn Fn(&Value) -> String) -> String;
    fn get_attribute_method_name(&self, attribute: &str) -> Result<String, AttributeError>;
    fn len(&self) -> usize;
    fn get_item(&self, index: i64) -> Result<Value, ListError>;
    fn set_item(&mut self, index: i64, value: Value) -> Result<(), ListError>;
    fn call_method(&mut self, method: &str, args: Vec<Value>) -> Result<(), MethodError>;
    fn add(&self, rhs: &Value) -> Result<Value, BinaryOpError>;
    fn sub(&self, rhs: &Value) -> Result<Value, BinaryOpError>;
    fn lt(&self, rhs: &Value) -> Result<Value, BinaryOpError>;
    fn as_i64(&self) -> Option<i64> {
        None
    }
}

pub(crate) type ObjectRef = Rc<RefCell<Box<dyn RuntimeObject>>>;

#[derive(Debug, Clone)]
pub(crate) struct ObjectWrapper {
    object: ObjectRef,
}

impl ObjectWrapper {
    pub(crate) fn new(object: ObjectRef) -> Self {
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

    pub(crate) fn add(&self, rhs: &Value) -> Result<Value, BinaryOpError> {
        self.object.borrow().add(rhs)
    }

    pub(crate) fn sub(&self, rhs: &Value) -> Result<Value, BinaryOpError> {
        self.object.borrow().sub(rhs)
    }

    pub(crate) fn lt(&self, rhs: &Value) -> Result<Value, BinaryOpError> {
        self.object.borrow().lt(rhs)
    }

    pub(crate) fn as_i64(&self) -> Option<i64> {
        self.object.borrow().as_i64()
    }
}

impl ObjectWrapper {
    pub(crate) fn get_item(&self, index: i64) -> Result<Value, ListError> {
        self.object.borrow().get_item(index)
    }
}

pub(crate) fn new_list_object(values: Vec<Value>) -> ObjectRef {
    Rc::new(RefCell::new(Box::new(ListObject::new(values))))
}
