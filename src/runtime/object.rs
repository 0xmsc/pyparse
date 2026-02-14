use std::cell::RefCell;
use std::rc::Rc;

use crate::runtime::list::{ListError, ListObject};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ObjectKind<Value> {
    List(ListObject<Value>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Object<Value> {
    pub(crate) kind: ObjectKind<Value>,
}

pub(crate) type ObjectRef<Value> = Rc<RefCell<Object<Value>>>;

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

#[derive(Debug, Clone)]
pub(crate) struct ObjectWrapper<Value> {
    object: ObjectRef<Value>,
}

impl<Value> Object<Value> {
    pub(crate) fn list(values: Vec<Value>) -> Self {
        Self {
            kind: ObjectKind::List(ListObject::new(values)),
        }
    }

    pub(crate) fn type_name(&self) -> &'static str {
        match &self.kind {
            ObjectKind::List(_) => "list",
        }
    }

    pub(crate) fn is_truthy(&self) -> bool {
        match &self.kind {
            ObjectKind::List(list) => !list.is_empty(),
        }
    }
}

impl<Value> ObjectWrapper<Value> {
    pub(crate) fn new(object: ObjectRef<Value>) -> Self {
        Self { object }
    }

    pub(crate) fn get_attribute_method_name(
        &self,
        attribute: &str,
    ) -> Result<String, AttributeError> {
        let borrowed = self.object.borrow();
        match &borrowed.kind {
            ObjectKind::List(_) if attribute == "append" => Ok(attribute.to_string()),
            _ => Err(AttributeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: borrowed.type_name().to_string(),
            }),
        }
    }

    pub(crate) fn len(&self) -> usize {
        let borrowed = self.object.borrow();
        match &borrowed.kind {
            ObjectKind::List(list) => list.__len__(),
        }
    }

    pub(crate) fn set_item(&self, index: i64, value: Value) -> Result<(), ListError> {
        let mut borrowed = self.object.borrow_mut();
        match &mut borrowed.kind {
            ObjectKind::List(list) => list.__setitem__(index, value),
        }
    }

    pub(crate) fn call_method(
        &self,
        method: &str,
        mut args: Vec<Value>,
    ) -> Result<(), MethodError> {
        let mut borrowed = self.object.borrow_mut();
        match &mut borrowed.kind {
            ObjectKind::List(list) => match method {
                "append" => {
                    if args.len() != 1 {
                        return Err(MethodError::ArityMismatch {
                            method: "append".to_string(),
                            expected: 1,
                            found: args.len(),
                        });
                    }
                    list.append(args.pop().expect("len checked above"));
                    Ok(())
                }
                _ => Err(MethodError::UnknownMethod {
                    method: method.to_string(),
                    type_name: "list".to_string(),
                }),
            },
        }
    }
}

impl<Value: Clone> ObjectWrapper<Value> {
    pub(crate) fn get_item(&self, index: i64) -> Result<Value, ListError> {
        let borrowed = self.object.borrow();
        match &borrowed.kind {
            ObjectKind::List(list) => list.__getitem__(index),
        }
    }
}

pub(crate) fn new_list_object<Value>(values: Vec<Value>) -> ObjectRef<Value> {
    Rc::new(RefCell::new(Object::list(values)))
}
