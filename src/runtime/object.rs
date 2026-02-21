use crate::builtins::BuiltinFunction;
use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

use crate::runtime::error::RuntimeError;
use crate::runtime::list::ListObject;
use crate::runtime::value::Value;

/// Stable identifier for a callable target used by runtime dispatch.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct CallableId(u32);

pub(crate) const FIRST_USER_CALLABLE_ID: u32 = 1024;

impl CallableId {
    pub(crate) fn builtin(builtin: BuiltinFunction) -> Self {
        Self(builtin.callable_id())
    }

    pub(crate) fn from_u32(callable_id: u32) -> Self {
        Self(callable_id)
    }

    pub(crate) fn as_u32(self) -> u32 {
        self.0
    }
}

pub(crate) fn assign_user_callable_ids<'a, I>(function_names: I) -> Vec<(String, CallableId)>
where
    I: IntoIterator<Item = &'a str>,
{
    let mut names = function_names
        .into_iter()
        .map(ToOwned::to_owned)
        .collect::<Vec<_>>();
    names.sort_unstable();
    names.dedup();
    names
        .into_iter()
        .enumerate()
        .map(|(offset, name)| {
            let offset = u32::try_from(offset).expect("callable id offset overflow");
            let callable_id = FIRST_USER_CALLABLE_ID
                .checked_add(offset)
                .expect("callable id overflow");
            (name, CallableId::from_u32(callable_id))
        })
        .collect()
}

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
