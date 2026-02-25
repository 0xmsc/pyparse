//! Shared runtime object model used by interpreter and VM backends.
//!
//! This module contains backend-agnostic pieces such as `Value`, object
//! protocols, builtins, exceptions, and environment helpers.
pub(crate) mod call_registry;
pub(crate) mod callable;
pub(crate) mod class;
pub(crate) mod dict;
pub(crate) mod error;
pub(crate) mod exception;
pub(crate) mod execution;
pub(crate) mod list;
pub(crate) mod method;
pub(crate) mod object;
pub(crate) mod value;
