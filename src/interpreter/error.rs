use thiserror::Error;

/// Typed errors produced by the tree-walking interpreter backend.
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum InterpreterError {
    #[error("Expected integer, got {got}")]
    ExpectedIntegerType { got: String },
    #[error("Expected list, got {got}")]
    ExpectedListType { got: String },
    #[error("List index must be non-negative, got {index}")]
    NegativeListIndex { index: i64 },
    #[error("List index out of bounds: index {index}, len {len}")]
    ListIndexOutOfBounds { index: usize, len: usize },
    #[error("Nested function definitions are not supported")]
    NestedFunctionDefinitionsUnsupported,
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
    #[error("Can only call identifiers")]
    NonIdentifierCallTarget,
    #[error("Return outside of function")]
    ReturnOutsideFunction,
}
