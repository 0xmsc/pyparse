use super::Value;
use crate::runtime::string;
use std::hash::{Hash, Hasher};

pub(super) fn try_to_output(value: &Value) -> Option<String> {
    string::downcast_string(value)
}

pub(super) fn try_truthiness(value: &Value) -> Option<bool> {
    string::downcast_string(value).map(|text| !text.is_empty())
}

pub(super) fn try_hash_key(value: &Value) -> Option<i64> {
    string::downcast_string(value).map(|text| hash_string(&text))
}

pub(super) fn try_key_equals(lhs: &Value, rhs: &Value) -> Option<bool> {
    match (string::downcast_string(lhs), string::downcast_string(rhs)) {
        (Some(left), Some(right)) => Some(left == right),
        _ => None,
    }
}

fn hash_string(value: &str) -> i64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish() as i64
}
