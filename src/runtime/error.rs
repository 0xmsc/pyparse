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
    #[error("Unhashable type: {type_name}")]
    UnhashableType { type_name: String },
    #[error("Dictionary key not found: {key}")]
    MissingKey { key: String },
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
    #[error("StopIteration")]
    StopIteration,
    #[error("Nested function definitions are not supported")]
    NestedFunctionDefinitionsUnsupported,
    #[error("Return outside of function")]
    ReturnOutsideFunction,
}

impl RuntimeError {
    pub(crate) fn expect_method_arity(
        method: &str,
        expected: usize,
        found: usize,
    ) -> Result<(), RuntimeError> {
        if found == expected {
            return Ok(());
        }
        Err(RuntimeError::ArityMismatch {
            method: method.to_string(),
            expected,
            found,
        })
    }

    pub(crate) fn expect_function_arity(
        name: &str,
        expected: usize,
        found: usize,
    ) -> Result<(), RuntimeError> {
        if found == expected {
            return Ok(());
        }
        Err(RuntimeError::FunctionArityMismatch {
            name: name.to_string(),
            expected,
            found,
        })
    }
}
