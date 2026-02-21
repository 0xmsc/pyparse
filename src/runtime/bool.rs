use crate::runtime::value::Value;

pub(crate) fn downcast_bool(value: &Value) -> Option<bool> {
    value.as_bool()
}
