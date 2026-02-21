use crate::builtins::BuiltinFunction;
use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::{Mutex, OnceLock};

use crate::runtime::error::RuntimeError;
use crate::runtime::list::ListObject;
use crate::runtime::value::Value;

/// Stable identifier for a callable target used by runtime dispatch.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct CallableId(u32);

#[derive(Default)]
struct CallableNameInterner {
    next_id: u32,
    by_name: HashMap<String, u32>,
}

fn callable_name_interner() -> &'static Mutex<CallableNameInterner> {
    static INTER: OnceLock<Mutex<CallableNameInterner>> = OnceLock::new();
    INTER.get_or_init(|| {
        Mutex::new(CallableNameInterner {
            // Reserve low IDs for builtins.
            next_id: 1024,
            by_name: HashMap::new(),
        })
    })
}

impl CallableId {
    pub(crate) fn builtin(builtin: BuiltinFunction) -> Self {
        Self(builtin.callable_id())
    }

    pub(crate) fn function(name: &str) -> Self {
        let mut interner = callable_name_interner()
            .lock()
            .expect("callable interner mutex poisoned");
        if let Some(id) = interner.by_name.get(name) {
            return Self(*id);
        }
        let id = interner.next_id;
        interner.next_id = interner
            .next_id
            .checked_add(1)
            .expect("callable id overflow");
        interner.by_name.insert(name.to_string(), id);
        Self(id)
    }

    pub(crate) fn as_u32(self) -> u32 {
        self.0
    }
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
