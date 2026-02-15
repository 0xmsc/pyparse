use crate::runtime::error::RuntimeError;
use crate::runtime::object::CallContext;
use crate::runtime::value::Value;
use std::rc::Rc;

pub(crate) fn bound_method<F>(callable: F) -> Value
where
    F: Fn(&mut dyn CallContext, Vec<Value>) -> Result<Value, RuntimeError> + 'static,
{
    Value::bound_method_object(Rc::new(callable))
}
