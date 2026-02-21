use super::{Value, expect_unary_method_args, invalid_argument_type, numeric_key_value};
use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;

pub(super) fn attribute(value: i64, attribute: &str) -> Result<Value, RuntimeError> {
    match attribute {
        "__add__" => Ok(bound_method(move |_context, args| {
            let rhs_int = expect_unary_int_arg("__add__", &args)?;
            Ok(Value::int_object(value + rhs_int))
        })),
        "__sub__" => Ok(bound_method(move |_context, args| {
            let rhs_int = expect_unary_int_arg("__sub__", &args)?;
            Ok(Value::int_object(value - rhs_int))
        })),
        "__lt__" => Ok(bound_method(move |_context, args| {
            let rhs_int = expect_unary_int_arg("__lt__", &args)?;
            Ok(Value::bool_object(value < rhs_int))
        })),
        "__eq__" => Ok(bound_method(move |_context, args| {
            let rhs = expect_unary_method_args("__eq__", &args)?;
            Ok(Value::bool_object(
                numeric_key_value(rhs).is_some_and(|rhs_value| value == rhs_value),
            ))
        })),
        "__hash__" => Ok(bound_method(move |_context, args| {
            RuntimeError::expect_method_arity("__hash__", 0, args.len())?;
            Ok(Value::int_object(value))
        })),
        "__bool__" => Ok(bound_method(move |_context, args| {
            RuntimeError::expect_method_arity("__bool__", 0, args.len())?;
            Ok(Value::bool_object(value != 0))
        })),
        "__str__" | "__repr__" => {
            let method = attribute.to_string();
            let rendered = value.to_string();
            Ok(bound_method(move |_context, args| {
                RuntimeError::expect_method_arity(&method, 0, args.len())?;
                Ok(Value::string_object(rendered.clone()))
            }))
        }
        _ => Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "int".to_string(),
        }),
    }
}

fn expect_unary_int_arg(method: &str, args: &[Value]) -> Result<i64, RuntimeError> {
    let rhs = expect_unary_method_args(method, args)?;
    let Some(rhs_int) = rhs.as_int() else {
        return Err(invalid_argument_type(method, "rhs", "int", rhs));
    };
    Ok(rhs_int)
}
