//! Built-in exception type/instance runtime objects.
//!
//! The runtime models exception classes as callable objects that construct
//! `ExceptionInstanceObject` values. Raising stores a normalized
//! `RaisedException` for backend-agnostic unwinding.

use std::any::Any;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
use crate::runtime::object::{CallContext, ObjectRef, RuntimeObject};
use crate::runtime::value::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ExceptionTypeKind {
    BaseException,
    Exception,
    StopIteration,
}

impl ExceptionTypeKind {
    pub(crate) fn name(self) -> &'static str {
        match self {
            Self::BaseException => "BaseException",
            Self::Exception => "Exception",
            Self::StopIteration => "StopIteration",
        }
    }

    pub(crate) fn is_stop_iteration(self) -> bool {
        self == Self::StopIteration
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct RaisedException {
    pub(crate) kind: ExceptionTypeKind,
    pub(crate) message: String,
}

impl RaisedException {
    pub(crate) fn new(kind: ExceptionTypeKind, message: String) -> Self {
        Self { kind, message }
    }

    pub(crate) fn stop_iteration() -> Self {
        Self::new(ExceptionTypeKind::StopIteration, String::new())
    }

    pub(crate) fn is_stop_iteration(&self) -> bool {
        self.kind.is_stop_iteration()
    }
}

impl fmt::Display for RaisedException {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.message.is_empty() {
            write!(f, "{}", self.kind.name())
        } else {
            write!(f, "{}: {}", self.kind.name(), self.message)
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ExceptionTypeObject {
    kind: ExceptionTypeKind,
}

impl ExceptionTypeObject {
    pub(crate) fn new(kind: ExceptionTypeKind) -> Self {
        Self { kind }
    }

    pub(crate) fn kind(&self) -> ExceptionTypeKind {
        self.kind
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ExceptionInstanceObject {
    exception: RaisedException,
}

impl ExceptionInstanceObject {
    pub(crate) fn new(exception: RaisedException) -> Self {
        Self { exception }
    }

    pub(crate) fn exception(&self) -> RaisedException {
        self.exception.clone()
    }
}

fn parse_exception_message(args: &[Value]) -> String {
    match args.len() {
        0 => String::new(),
        1 => args[0].to_output(),
        _ => format!(
            "({})",
            args.iter()
                .map(Value::to_output)
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

impl RuntimeObject for ExceptionTypeObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "type"
    }

    fn get_attribute(&self, receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match attribute {
            "__call__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |context, args| {
                    let object = receiver.borrow();
                    object.call(receiver.clone(), context, args)
                }))
            }
            "__str__" | "__repr__" => {
                let rendered = format!("<class '{}'>", self.kind.name());
                let method = attribute.to_string();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity(&method, 0, args.len())?;
                    Ok(Value::string_object(rendered.clone()))
                }))
            }
            _ => Err(RuntimeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: self.type_name().to_string(),
            }),
        }
    }

    fn call(
        &self,
        _receiver: ObjectRef,
        _context: &mut dyn CallContext,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let raised = RaisedException::new(self.kind, parse_exception_message(&args));
        Ok(Value::from_object_ref(Rc::new(RefCell::new(Box::new(
            ExceptionInstanceObject::new(raised),
        )))))
    }
}

impl RuntimeObject for ExceptionInstanceObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "exception"
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match attribute {
            "__str__" => {
                let value = self.exception.message.clone();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity("__str__", 0, args.len())?;
                    Ok(Value::string_object(value.clone()))
                }))
            }
            "__repr__" => {
                let rendered = if self.exception.message.is_empty() {
                    format!("{}()", self.exception.kind.name())
                } else {
                    format!(
                        "{}({:?})",
                        self.exception.kind.name(),
                        self.exception.message
                    )
                };
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity("__repr__", 0, args.len())?;
                    Ok(Value::string_object(rendered.clone()))
                }))
            }
            _ => Err(RuntimeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: self.type_name().to_string(),
            }),
        }
    }
}

pub(crate) fn exception_type_value(kind: ExceptionTypeKind) -> Value {
    Value::from_object_ref(Rc::new(RefCell::new(Box::new(ExceptionTypeObject::new(
        kind,
    )))))
}

pub(crate) fn exception_type_kind(value: &Value) -> Option<ExceptionTypeKind> {
    let Value::Object(object_ref) = value else {
        return None;
    };
    let object = object_ref.borrow();
    let exception_type = object.as_any().downcast_ref::<ExceptionTypeObject>()?;
    Some(exception_type.kind())
}

pub(crate) fn raised_exception(value: &Value) -> Option<RaisedException> {
    let Value::Object(object_ref) = value else {
        return None;
    };
    let object = object_ref.borrow();
    let instance = object.as_any().downcast_ref::<ExceptionInstanceObject>()?;
    Some(instance.exception())
}
