use super::{Value, expect_unary_method_args};
use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;

pub(super) fn attribute(attribute: &str) -> Result<Value, RuntimeError> {
    match attribute {
        "__eq__" => Ok(bound_method(move |_context, args| {
            let rhs = expect_unary_method_args("__eq__", &args)?;
            Ok(Value::bool_object(rhs.is_none()))
        })),
        "__hash__" => Ok(bound_method(move |_context, args| {
            RuntimeError::expect_method_arity("__hash__", 0, args.len())?;
            Ok(Value::int_object(0x9e37_79b9_i64))
        })),
        "__bool__" => Ok(bound_method(move |_context, args| {
            RuntimeError::expect_method_arity("__bool__", 0, args.len())?;
            Ok(Value::bool_object(false))
        })),
        "__str__" | "__repr__" => {
            let method = attribute.to_string();
            Ok(bound_method(move |_context, args| {
                RuntimeError::expect_method_arity(&method, 0, args.len())?;
                Ok(Value::string_object("None".to_string()))
            }))
        }
        _ => Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "NoneType".to_string(),
        }),
    }
}
