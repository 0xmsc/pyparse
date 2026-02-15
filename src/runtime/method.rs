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

pub(crate) fn zero_arg_value_method<F>(method: impl Into<String>, make_value: F) -> Value
where
    F: Fn() -> Value + 'static,
{
    let method = method.into();
    bound_method(move |_context, args| {
        RuntimeError::expect_method_arity(&method, 0, args.len())?;
        Ok(make_value())
    })
}

pub(crate) fn zero_arg_string_method(
    method: impl Into<String>,
    rendered: impl Into<String>,
) -> Value {
    let rendered = rendered.into();
    zero_arg_value_method(method, move || Value::string_object(rendered.clone()))
}
