use std::collections::HashMap;

use crate::builtins::BuiltinFunction;
use crate::runtime::error::RuntimeError;
use crate::runtime::value::Value;

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
    }
}
