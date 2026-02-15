use thiserror::Error;

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub(crate) enum RuntimeError {
    #[error("Unknown attribute '{attribute}' for type {type_name}")]
    UnknownAttribute {
        attribute: String,
        type_name: String,
    },
    #[error("Method '{method}' expected {expected} arguments, got {found}")]
    ArityMismatch {
        method: String,
        expected: usize,
        found: usize,
    },
    #[error("Unknown method '{method}' for type {type_name}")]
    UnknownMethod { method: String, type_name: String },
    #[error("Operation '{operation}' is not supported for type {type_name}")]
    UnsupportedOperation {
        operation: String,
        type_name: String,
    },
    #[error(
        "Invalid argument type for operation '{operation}': '{argument}' expected {expected}, got {got}"
    )]
    InvalidArgumentType {
        operation: String,
        argument: String,
        expected: String,
        got: String,
    },
    #[error("List index must be non-negative, got {index}")]
    NegativeIndex { index: i64 },
    #[error("List index out of bounds: index {index}, len {len}")]
    IndexOutOfBounds { index: usize, len: usize },
    #[error("Undefined variable '{name}'")]
    UndefinedVariable { name: String },
    #[error("Undefined function '{name}'")]
    UndefinedFunction { name: String },
    #[error("Function '{name}' expected {expected} arguments, got {found}")]
    FunctionArityMismatch {
        name: String,
        expected: usize,
        found: usize,
    },
    #[error("Object of type {type_name} is not callable")]
    ObjectNotCallable { type_name: String },
    #[error("Nested function definitions are not supported")]
    NestedFunctionDefinitionsUnsupported,
    #[error("Return outside of function")]
    ReturnOutsideFunction,
}
