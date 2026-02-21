use std::collections::HashMap;

use crate::runtime::error::RuntimeError;
use crate::runtime::object::CallableId;

/// Backend-agnostic callable registry used for runtime `CallableId` dispatch.
///
/// Builtins and user-defined functions are stored uniformly as backend-owned
/// callable payloads.
pub(crate) struct CallRegistry<F> {
    next_callable_id: u32,
    callables_by_id: HashMap<u32, F>,
}

impl<F> CallRegistry<F> {
    /// Creates an empty registry.
    pub(crate) fn new() -> Self {
        Self {
            next_callable_id: 1,
            callables_by_id: HashMap::new(),
        }
    }

    /// Registers a callable at a fixed `CallableId`.
    pub(crate) fn register_with_id(&mut self, callable_id: u32, function: F) {
        if self.callables_by_id.insert(callable_id, function).is_some() {
            panic!("duplicate callable id registration: {callable_id}");
        }
        self.next_callable_id = self
            .next_callable_id
            .max(callable_id.checked_add(1).expect("callable id overflow"));
    }

    /// Registers a callable and returns a freshly allocated runtime `CallableId`.
    pub(crate) fn register_function(&mut self, function: F) -> CallableId {
        let callable_id = CallableId(self.next_callable_id);
        self.next_callable_id = self
            .next_callable_id
            .checked_add(1)
            .expect("callable id overflow");
        if self
            .callables_by_id
            .insert(callable_id.0, function)
            .is_some()
        {
            panic!("duplicate callable id registration: {}", callable_id.0);
        }
        callable_id
    }
}

impl<F: Clone> CallRegistry<F> {
    /// Resolves a runtime `CallableId` to backend callable metadata.
    pub(crate) fn resolve(&self, callable_id: &CallableId) -> Result<F, RuntimeError> {
        let callable_id = callable_id.0;
        self.callables_by_id
            .get(&callable_id)
            .cloned()
            .ok_or_else(|| RuntimeError::UndefinedFunction {
                name: format!("<callable:{callable_id}>"),
            })
    }
}
