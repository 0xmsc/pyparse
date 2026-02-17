use crate::builtins::BuiltinFunction;
use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

use crate::runtime::error::RuntimeError;
use crate::runtime::list::ListObject;
use crate::runtime::value::Value;

pub(crate) trait CallContext {
    fn call_builtin(
        &mut self,
        builtin: BuiltinFunction,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError>;

    fn call_function_named(&mut self, name: &str, args: Vec<Value>) -> Result<Value, RuntimeError>;
}

pub(crate) type BoundMethodCallable =
    Rc<dyn Fn(&mut dyn CallContext, Vec<Value>) -> Result<Value, RuntimeError>>;

pub(crate) type ObjectRef = Rc<RefCell<Box<dyn RuntimeObject>>>;

pub(crate) type GetAttributeSlot = fn(ObjectRef, &str) -> Result<Value, RuntimeError>;
pub(crate) type SetAttributeSlot = fn(ObjectRef, &str, Value) -> Result<(), RuntimeError>;
pub(crate) type CallSlot =
    fn(ObjectRef, &mut dyn CallContext, Vec<Value>) -> Result<Value, RuntimeError>;

#[derive(Clone, Copy)]
pub(crate) struct TypeObject {
    pub(crate) name: &'static str,
    pub(crate) get_attribute: GetAttributeSlot,
    pub(crate) set_attribute: SetAttributeSlot,
    pub(crate) call: CallSlot,
}

impl TypeObject {
    pub(crate) fn name(&self) -> &'static str {
        self.name
    }

    pub(crate) fn get_attribute(
        &self,
        receiver: ObjectRef,
        attribute: &str,
    ) -> Result<Value, RuntimeError> {
        (self.get_attribute)(receiver, attribute)
    }

    pub(crate) fn set_attribute(
        &self,
        receiver: ObjectRef,
        attribute: &str,
        value: Value,
    ) -> Result<(), RuntimeError> {
        (self.set_attribute)(receiver, attribute, value)
    }

    pub(crate) fn call(
        &self,
        receiver: ObjectRef,
        context: &mut dyn CallContext,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        (self.call)(receiver, context, args)
    }
}

pub(crate) trait RuntimeObject: std::fmt::Debug + Any {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
    fn type_object(&self) -> &'static TypeObject;

    fn type_name(&self) -> &'static str {
        self.type_object().name()
    }
}

pub(crate) fn unknown_attribute(
    receiver: ObjectRef,
    attribute: &str,
) -> Result<Value, RuntimeError> {
    Err(RuntimeError::UnknownAttribute {
        attribute: attribute.to_string(),
        type_name: receiver_type_name(receiver),
    })
}

pub(crate) fn unsupported_attribute_assignment(
    receiver: ObjectRef,
    attribute: &str,
    _value: Value,
) -> Result<(), RuntimeError> {
    Err(RuntimeError::UnknownAttribute {
        attribute: attribute.to_string(),
        type_name: receiver_type_name(receiver),
    })
}

pub(crate) fn object_not_callable(
    receiver: ObjectRef,
    _context: &mut dyn CallContext,
    _args: Vec<Value>,
) -> Result<Value, RuntimeError> {
    Err(RuntimeError::ObjectNotCallable {
        type_name: receiver_type_name(receiver),
    })
}

fn receiver_type_name(receiver: ObjectRef) -> String {
    receiver.borrow().type_name().to_string()
}

pub(crate) fn new_list_object(values: Vec<Value>) -> ObjectRef {
    Rc::new(RefCell::new(Box::new(ListObject::new(values))))
}
