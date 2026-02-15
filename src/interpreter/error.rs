use crate::runtime::error::RuntimeError;
use thiserror::Error;

/// Typed errors produced by the tree-walking interpreter backend.
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub(crate) enum InterpreterError {
    #[error(transparent)]
    Runtime(#[from] RuntimeError),
}
