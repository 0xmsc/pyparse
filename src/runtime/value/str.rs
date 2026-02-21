use super::Value;
use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
use std::hash::{Hash, Hasher};

pub(super) fn attribute(value: &str, attribute: &str) -> Result<Value, RuntimeError> {
    match attribute {
        "__bool__" => {
            let is_non_empty = !value.is_empty();
            Ok(bound_method(move |_context, args| {
                RuntimeError::expect_method_arity("__bool__", 0, args.len())?;
                Ok(Value::bool_object(is_non_empty))
            }))
        }
        "__str__" => {
            let value = value.to_string();
            Ok(bound_method(move |_context, args| {
                RuntimeError::expect_method_arity("__str__", 0, args.len())?;
                Ok(Value::string_object(value.clone()))
            }))
        }
        "__repr__" => {
            let rendered = format!("{value:?}");
            Ok(bound_method(move |_context, args| {
                RuntimeError::expect_method_arity("__repr__", 0, args.len())?;
                Ok(Value::string_object(rendered.clone()))
            }))
        }
        _ => Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "str".to_string(),
        }),
    }
}

pub(super) fn try_hash_key(value: &Value) -> Option<i64> {
    value.as_str().map(hash_string)
}

pub(super) fn try_key_equals(lhs: &Value, rhs: &Value) -> Option<bool> {
    match (lhs.as_str(), rhs.as_str()) {
        (Some(left), Some(right)) => Some(left == right),
        _ => None,
    }
}

fn hash_string(value: &str) -> i64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish() as i64
}
