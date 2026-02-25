//! `pyparse` library crate.
//!
//! High-level layout:
//! - frontend: `lexer` + `parser` produce the shared AST (`parser::ast`)
//! - shared runtime object/value model: `runtime`
//! - execution backends: `interpreter` (AST-walk) and `vm` (bytecode)
pub mod backend;
pub(crate) mod builtins;
pub(crate) mod bytecode;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub(crate) mod runtime;
pub mod vm;

pub use parser::ast;
