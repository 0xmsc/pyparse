use anyhow::{Result, bail};

/// Runtime value model used by the tree-walking interpreter.
#[derive(Debug, Clone, PartialEq)]
pub(super) enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    None,
}

impl Value {
    pub(super) fn as_int(&self) -> Result<i64> {
        match self {
            Value::Integer(value) => Ok(*value),
            Value::Boolean(_) | Value::String(_) | Value::None => {
                bail!("Expected integer, got {self:?}")
            }
        }
    }

    pub(super) fn to_output(&self) -> String {
        match self {
            Value::Integer(value) => value.to_string(),
            Value::Boolean(value) => {
                if *value {
                    "True".to_string()
                } else {
                    "False".to_string()
                }
            }
            Value::String(value) => value.clone(),
            Value::None => "None".to_string(),
        }
    }

    pub(super) fn is_truthy(&self) -> bool {
        match self {
            Value::Integer(value) => *value != 0,
            Value::Boolean(value) => *value,
            Value::String(value) => !value.is_empty(),
            Value::None => false,
        }
    }
}
