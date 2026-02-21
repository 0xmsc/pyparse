use crate::runtime::error::RuntimeError;
use crate::runtime::int::downcast_i64;
use crate::runtime::method::bound_method;
use crate::runtime::object::{CallContext, ObjectRef, RuntimeObject};
use crate::runtime::string::downcast_string;
use crate::runtime::value::Value;
use std::any::Any;

#[derive(Debug, Clone, PartialEq, Eq)]
enum DictKey {
    Int(i64),
    Bool(bool),
    String(String),
    None,
}

impl DictKey {
    fn from_value(operation: &str, argument: &str, value: &Value) -> Result<Self, RuntimeError> {
        if let Some(integer) = downcast_i64(value) {
            return Ok(Self::Int(integer));
        }
        if let Some(boolean) = value.as_bool() {
            return Ok(Self::Bool(boolean));
        }
        if let Some(string) = downcast_string(value) {
            return Ok(Self::String(string));
        }
        if value.is_none() {
            return Ok(Self::None);
        }
        Err(RuntimeError::InvalidArgumentType {
            operation: operation.to_string(),
            argument: argument.to_string(),
            expected: "int|bool|str|None".to_string(),
            got: value.type_name().to_string(),
        })
    }

    fn render(&self) -> String {
        match self {
            Self::Int(value) => value.to_string(),
            Self::Bool(value) => {
                if *value {
                    "True".to_string()
                } else {
                    "False".to_string()
                }
            }
            Self::String(value) => format!("{value:?}"),
            Self::None => "None".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct DictObject {
    entries: Vec<(DictKey, Value)>,
}

impl DictObject {
    pub(crate) fn new(entries: Vec<(Value, Value)>) -> Result<Self, RuntimeError> {
        let mut object = Self {
            entries: Vec::with_capacity(entries.len()),
        };
        for (key, value) in entries {
            object.set_item_value(key, value)?;
        }
        Ok(object)
    }

    pub(crate) fn __len__(&self) -> usize {
        self.entries.len()
    }

    fn lookup_index(&self, key: &DictKey) -> Option<usize> {
        self.entries
            .iter()
            .position(|(candidate_key, _)| candidate_key == key)
    }

    fn __getitem__(&self, key: &DictKey) -> Result<Value, RuntimeError> {
        let Some(index) = self.lookup_index(key) else {
            return Err(RuntimeError::MissingKey { key: key.render() });
        };
        Ok(self.entries[index].1.clone())
    }

    fn __setitem__(&mut self, key: DictKey, value: Value) {
        if let Some(index) = self.lookup_index(&key) {
            self.entries[index].1 = value;
        } else {
            self.entries.push((key, value));
        }
    }

    fn render(&self) -> Result<String, RuntimeError> {
        let mut rendered = Vec::with_capacity(self.entries.len());
        for (key, value) in &self.entries {
            let value_repr = render_value_repr(value)?;
            rendered.push(format!("{}: {value_repr}", key.render()));
        }
        Ok(format!("{{{}}}", rendered.join(", ")))
    }

    pub(crate) fn get_item_value(&self, key: Value) -> Result<Value, RuntimeError> {
        let key = DictKey::from_value("__getitem__", "key", &key)?;
        self.__getitem__(&key)
    }

    pub(crate) fn set_item_value(&mut self, key: Value, value: Value) -> Result<(), RuntimeError> {
        let key = DictKey::from_value("__setitem__", "key", &key)?;
        self.__setitem__(key, value);
        Ok(())
    }
}

impl RuntimeObject for DictObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "dict"
    }

    fn get_attribute(&self, receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match attribute {
            "__len__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity("__len__", 0, args.len())?;
                    Ok(with_dict(&receiver, |dict| {
                        Value::int_object(dict.__len__() as i64)
                    }))
                }))
            }
            "__getitem__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |_context, mut args| {
                    RuntimeError::expect_method_arity("__getitem__", 1, args.len())?;
                    let key = args.pop().expect("len checked above");
                    with_dict(&receiver, |dict| dict.get_item_value(key))
                }))
            }
            "__setitem__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |_context, mut args| {
                    RuntimeError::expect_method_arity("__setitem__", 2, args.len())?;
                    let value = args.pop().expect("len checked above");
                    let key = args.pop().expect("len checked above");
                    with_dict_mut(&receiver, |dict| dict.set_item_value(key, value))?;
                    Ok(Value::none_object())
                }))
            }
            "__str__" | "__repr__" => {
                let receiver = receiver.clone();
                let method = attribute.to_string();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity(&method, 0, args.len())?;
                    let rendered = with_dict(&receiver, DictObject::render)?;
                    Ok(Value::string_object(rendered))
                }))
            }
            _ => Err(RuntimeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: self.type_name().to_string(),
            }),
        }
    }
}

fn with_dict<R>(receiver: &ObjectRef, f: impl FnOnce(&DictObject) -> R) -> R {
    let object = receiver.borrow();
    let dict = object
        .as_any()
        .downcast_ref::<DictObject>()
        .expect("dict get_attribute receiver must be DictObject");
    f(dict)
}

fn with_dict_mut<R>(receiver: &ObjectRef, f: impl FnOnce(&mut DictObject) -> R) -> R {
    let mut object = receiver.borrow_mut();
    let dict = object
        .as_any_mut()
        .downcast_mut::<DictObject>()
        .expect("dict get_attribute receiver must be DictObject");
    f(dict)
}

struct NoopCallContext;

impl CallContext for NoopCallContext {
    fn call_callable(
        &mut self,
        _callable_id: &crate::runtime::object::CallableId,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        panic!("NoopCallContext should not resolve callables");
    }
}

fn render_value_repr(value: &Value) -> Result<String, RuntimeError> {
    let repr_method = value.get_attribute("__repr__")?;
    let mut context = NoopCallContext;
    let repr_value = repr_method.call(&mut context, vec![])?;
    let Some(rendered) = downcast_string(&repr_value) else {
        return Err(RuntimeError::InvalidArgumentType {
            operation: "__repr__".to_string(),
            argument: "return".to_string(),
            expected: "str".to_string(),
            got: repr_value.type_name().to_string(),
        });
    };
    Ok(rendered)
}

#[cfg(test)]
mod tests {
    use super::DictObject;
    use crate::runtime::error::RuntimeError;
    use crate::runtime::value::Value;

    #[test]
    fn supports_len_get_set_and_render() {
        let mut dict = DictObject::new(vec![
            (Value::string_object("a".to_string()), Value::int_object(1)),
            (Value::string_object("b".to_string()), Value::int_object(2)),
        ])
        .expect("dict should build");

        assert_eq!(dict.__len__(), 2);
        assert_eq!(
            dict.get_item_value(Value::string_object("a".to_string()))
                .expect("key should exist")
                .to_output(),
            "1"
        );
        dict.set_item_value(Value::string_object("a".to_string()), Value::int_object(7))
            .expect("set should work");
        dict.set_item_value(Value::int_object(3), Value::int_object(9))
            .expect("set should work");
        assert_eq!(dict.__len__(), 3);
        assert_eq!(
            dict.get_item_value(Value::string_object("a".to_string()))
                .expect("updated key should exist")
                .to_output(),
            "7"
        );
        assert_eq!(
            dict.render().expect("render should succeed"),
            "{\"a\": 7, \"b\": 2, 3: 9}"
        );
    }

    #[test]
    fn reports_missing_key_and_invalid_key_type() {
        let dict = DictObject::new(vec![(Value::int_object(1), Value::int_object(2))])
            .expect("dict should build");
        assert_eq!(
            dict.get_item_value(Value::int_object(3))
                .expect_err("missing key should fail"),
            RuntimeError::MissingKey {
                key: "3".to_string()
            }
        );
        assert_eq!(
            dict.get_item_value(Value::list_object(vec![]))
                .expect_err("non-hashable key should fail"),
            RuntimeError::InvalidArgumentType {
                operation: "__getitem__".to_string(),
                argument: "key".to_string(),
                expected: "int|bool|str|None".to_string(),
                got: "list".to_string(),
            }
        );
    }
}
