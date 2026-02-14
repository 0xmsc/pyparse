use std::cell::RefCell;
use std::rc::Rc;

use crate::runtime::list::ListObject;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum ObjectKind<Value> {
    List(ListObject<Value>),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Object<Value> {
    pub(crate) kind: ObjectKind<Value>,
}

pub(crate) type ObjectRef<Value> = Rc<RefCell<Object<Value>>>;

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

pub(crate) fn new_list_object<Value>(values: Vec<Value>) -> ObjectRef<Value> {
    Rc::new(RefCell::new(Object::list(values)))
}
