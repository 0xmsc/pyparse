use crate::runtime::value::Value;

pub(crate) fn downcast_string(value: &Value) -> Option<String> {
    value.as_str().map(ToString::to_string)
}
