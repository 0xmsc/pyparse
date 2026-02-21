use std::collections::HashMap;

use crate::builtins::BuiltinFunction;
use crate::runtime::error::RuntimeError;
use crate::runtime::value::Value;

/// Name-resolution scope used by interpreter and VM execution.
///
/// Locals shadow globals when present; top-level execution has no locals map.
pub(crate) struct Environment<'a> {
    globals: &'a mut HashMap<String, Value>,
    locals: Option<&'a mut HashMap<String, Value>>,
}

impl<'a> Environment<'a> {
    pub(crate) fn top_level(globals: &'a mut HashMap<String, Value>) -> Self {
        Self {
            globals,
            locals: None,
        }
    }

    fn with_locals(
        globals: &'a mut HashMap<String, Value>,
        locals: &'a mut HashMap<String, Value>,
    ) -> Self {
        Self {
            globals,
            locals: Some(locals),
        }
    }

    pub(crate) fn load(&self, name: &str) -> Option<&Value> {
        if let Some(locals) = self.locals.as_deref()
            && let Some(value) = locals.get(name)
        {
            return Some(value);
        }
        self.globals.get(name)
    }

    pub(crate) fn load_cloned(&self, name: &str) -> Option<Value> {
        self.load(name).cloned()
    }

    pub(crate) fn store(&mut self, name: String, value: Value) {
        if let Some(locals) = self.locals.as_deref_mut() {
            locals.insert(name, value);
        } else {
            self.globals.insert(name, value);
        }
    }

    pub(crate) fn child_with_locals<'b>(
        &'b mut self,
        locals: &'b mut HashMap<String, Value>,
    ) -> Environment<'b> {
        Environment::with_locals(self.globals, locals)
    }

    pub(crate) fn is_top_level(&self) -> bool {
        self.locals.is_none()
    }
}

/// Shared builtin-call implementation for backends that collect printed output.
pub(crate) fn call_builtin_with_output(
    builtin: BuiltinFunction,
    args: Vec<Value>,
    output: &mut Vec<String>,
) -> Result<Value, RuntimeError> {
    match builtin {
        BuiltinFunction::Print => {
            let rendered = args.iter().map(Value::to_output).collect::<Vec<_>>();
            output.push(rendered.join(" "));
            Ok(Value::none_object())
        }
        BuiltinFunction::Len => {
            RuntimeError::expect_function_arity("len", 1, args.len())?;
            args[0].len()
        }
        BuiltinFunction::Range => {
            RuntimeError::expect_function_arity("range", 1, args.len())?;
            let stop = args[0]
                .as_int()
                .ok_or_else(|| RuntimeError::InvalidArgumentType {
                    operation: "range".to_string(),
                    argument: "stop".to_string(),
                    expected: "int".to_string(),
                    got: args[0].type_name().to_string(),
                })?;
            if stop <= 0 {
                return Ok(Value::list_object(Vec::new()));
            }
            let mut values = Vec::with_capacity(stop as usize);
            for value in 0..stop {
                values.push(Value::int_object(value));
            }
            Ok(Value::list_object(values))
        }
        BuiltinFunction::Exception => {
            let message = match args.len() {
                0 => String::new(),
                1 => args[0].to_output(),
                _ => format!(
                    "({})",
                    args.iter()
                        .map(Value::to_output)
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            };
            if message.is_empty() {
                Ok(Value::string_object("Exception".to_string()))
            } else {
                Ok(Value::string_object(format!("Exception: {message}")))
            }
        }
        BuiltinFunction::StopIteration => {
            let message = match args.len() {
                0 => String::new(),
                1 => args[0].to_output(),
                _ => format!(
                    "({})",
                    args.iter()
                        .map(Value::to_output)
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            };
            if message.is_empty() {
                Ok(Value::string_object("StopIteration".to_string()))
            } else {
                Ok(Value::string_object(format!("StopIteration: {message}")))
            }
        }
    }
}

/// Ensures builtin callables are present in a globals table as ordinary values.
pub(crate) fn seed_builtin_globals(globals: &mut HashMap<String, Value>) {
    for builtin in [
        BuiltinFunction::Print,
        BuiltinFunction::Len,
        BuiltinFunction::Range,
        BuiltinFunction::Exception,
        BuiltinFunction::StopIteration,
    ] {
        globals
            .entry(builtin.name().to_string())
            .or_insert_with(|| Value::builtin_function_object(builtin));
    }
}
