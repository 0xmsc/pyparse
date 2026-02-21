use super::{Value, expect_unary_method_args, numeric_key_value};
use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;

pub(super) fn attribute(value: bool, attribute: &str) -> Result<Value, RuntimeError> {
    match attribute {
        "__eq__" => Ok(bound_method(move |_context, args| {
            let rhs = expect_unary_method_args("__eq__", &args)?;
            let lhs_value = if value { 1 } else { 0 };
            Ok(Value::bool_object(
                numeric_key_value(rhs).is_some_and(|rhs_value| lhs_value == rhs_value),
            ))
        })),
        "__hash__" => Ok(bound_method(move |_context, args| {
            RuntimeError::expect_method_arity("__hash__", 0, args.len())?;
            Ok(Value::int_object(if value { 1 } else { 0 }))
        })),
        "__bool__" => Ok(bound_method(move |_context, args| {
            RuntimeError::expect_method_arity("__bool__", 0, args.len())?;
            Ok(Value::bool_object(value))
        })),
        "__str__" | "__repr__" => {
            let method = attribute.to_string();
            let rendered = if value { "True" } else { "False" }.to_string();
            Ok(bound_method(move |_context, args| {
                RuntimeError::expect_method_arity(&method, 0, args.len())?;
                Ok(Value::string_object(rendered.clone()))
            }))
        }
        _ => Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "bool".to_string(),
        }),
    }
}
