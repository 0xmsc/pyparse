use crate::runtime::value::Value;

pub(crate) fn downcast_i64(value: &Value) -> Option<i64> {
    value.as_int()
}
