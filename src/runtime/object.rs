use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

use crate::runtime::error::RuntimeError;
use crate::runtime::list::ListObject;
use crate::runtime::value::Value;

/// Stable identifier for a callable target used by runtime dispatch.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct CallableId(pub(crate) u32);

/// Backend callback interface used by runtime values for indirect calls.
///
/// `Value`/`RuntimeObject` use this to invoke callable targets without
/// depending on a specific execution backend.
pub(crate) trait CallContext {
    /// Calls a resolved callable target with already-evaluated arguments.
    fn call_callable(
        &mut self,
        callable_id: &CallableId,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError>;
}

/// Shared closure type used for bound methods and method wrappers.
pub(crate) type BoundMethodCallable =
    Rc<dyn Fn(&mut dyn CallContext, Vec<Value>) -> Result<Value, RuntimeError>>;

/// Shared heap handle for all runtime objects wrapped by `Value`.
pub(crate) type ObjectRef = Rc<RefCell<Box<dyn RuntimeObject>>>;

/// Object protocol implemented by all runtime object types.
///
/// Default implementations report unsupported attribute access/calls.
pub(crate) trait RuntimeObject: std::fmt::Debug + Any {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
    fn type_name(&self) -> &'static str;

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: self.type_name().to_string(),
        })
    }

    fn set_attribute(&mut self, attribute: &str, _value: Value) -> Result<(), RuntimeError> {
        Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: self.type_name().to_string(),
        })
    }

    fn call(
        &self,
        _receiver: ObjectRef,
        _context: &mut dyn CallContext,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        Err(RuntimeError::ObjectNotCallable {
            type_name: self.type_name().to_string(),
        })
    }
}

pub(crate) fn new_list_object(values: Vec<Value>) -> ObjectRef {
    Rc::new(RefCell::new(Box::new(ListObject::new(values))))
}
