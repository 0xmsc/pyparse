use std::collections::HashMap;

use crate::builtins::BuiltinFunction;
use crate::runtime::error::RuntimeError;
use crate::runtime::object::CallableId;

/// Backend-agnostic callable registry used for runtime `CallableId` dispatch.
#[derive(Clone)]
pub(crate) enum RegisteredCallable<F> {
    Builtin(BuiltinFunction),
    Function(F),
}

/// Stores runtime callables and allocates fresh `CallableId`s for user functions.
pub(crate) struct CallRegistry<F> {
    next_callable_id: u32,
    callables_by_id: HashMap<u32, RegisteredCallable<F>>,
}

impl<F> CallRegistry<F> {
    /// Creates a registry pre-populated with builtin callable IDs.
    pub(crate) fn new() -> Self {
        let mut callables_by_id = HashMap::new();
        for builtin in [BuiltinFunction::Print, BuiltinFunction::Len] {
            callables_by_id.insert(builtin.callable_id(), RegisteredCallable::Builtin(builtin));
        }
        let next_callable_id = callables_by_id
            .keys()
            .copied()
            .max()
            .unwrap_or(0)
            .checked_add(1)
            .expect("callable id overflow");
        Self {
            next_callable_id,
            callables_by_id,
        }
    }

    /// Registers a user function and returns the runtime `CallableId`.
    pub(crate) fn register_function(&mut self, function: F) -> CallableId {
        let callable_id = CallableId(self.next_callable_id);
        self.next_callable_id = self
            .next_callable_id
            .checked_add(1)
            .expect("callable id overflow");
        self.callables_by_id
            .insert(callable_id.0, RegisteredCallable::Function(function));
        callable_id
    }
}

impl<F: Clone> CallRegistry<F> {
    /// Resolves a runtime `CallableId` to either builtin or user callable metadata.
    pub(crate) fn resolve(
        &self,
        callable_id: &CallableId,
    ) -> Result<RegisteredCallable<F>, RuntimeError> {
        let callable_id = callable_id.0;
        self.callables_by_id
            .get(&callable_id)
            .cloned()
            .ok_or_else(|| RuntimeError::UndefinedFunction {
                name: format!("<callable:{callable_id}>"),
            })
    }
}
