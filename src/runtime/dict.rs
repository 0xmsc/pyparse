//! Runtime dictionary object with Python-like key semantics.
//!
//! Storage preserves insertion order (`entries`) while `buckets` accelerates
//! lookup by hash. Bucket collisions are resolved by checking key equality.

use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
use crate::runtime::object::{CallContext, ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct DictEntry {
    key: Value,
    value: Value,
}

#[derive(Debug, Clone)]
pub(crate) struct DictObject {
    entries: Vec<DictEntry>,
    buckets: HashMap<i64, Vec<usize>>,
}

impl DictObject {
    pub(crate) fn new(
        entries: Vec<(Value, Value)>,
        context: &mut dyn CallContext,
    ) -> Result<Self, RuntimeError> {
        let mut object = Self {
            entries: Vec::with_capacity(entries.len()),
            buckets: HashMap::new(),
        };
        for (key, value) in entries {
            object.set_item_value(context, key, value)?;
        }
        Ok(object)
    }

    pub(crate) fn __len__(&self) -> usize {
        self.entries.len()
    }

    fn find_index(
        &self,
        context: &mut dyn CallContext,
        hash: i64,
        key: &Value,
    ) -> Result<Option<usize>, RuntimeError> {
        let Some(indices) = self.buckets.get(&hash) else {
            return Ok(None);
        };
        for &index in indices {
            let entry = self.entries.get(index).expect("bucket index must be valid");
            if entry.key.key_equals(context, key)? {
                return Ok(Some(index));
            }
        }
        Ok(None)
    }

    pub(crate) fn get_item_value(
        &self,
        context: &mut dyn CallContext,
        key: Value,
    ) -> Result<Value, RuntimeError> {
        let hash = key.hash_key(context)?;
        let Some(index) = self.find_index(context, hash, &key)? else {
            return Err(RuntimeError::MissingKey {
                key: key.to_output(),
            });
        };
        Ok(self.entries[index].value.clone())
    }

    pub(crate) fn set_item_value(
        &mut self,
        context: &mut dyn CallContext,
        key: Value,
        value: Value,
    ) -> Result<(), RuntimeError> {
        // Hash first, then confirm equality inside the collision bucket to
        // preserve Python-like key aliasing (e.g. `True` and `1`).
        let hash = key.hash_key(context)?;
        if let Some(index) = self.find_index(context, hash, &key)? {
            self.entries[index].value = value;
            return Ok(());
        }

        let index = self.entries.len();
        self.entries.push(DictEntry { key, value });
        self.buckets.entry(hash).or_default().push(index);
        Ok(())
    }

    fn render(&self, context: &mut dyn CallContext) -> Result<String, RuntimeError> {
        let mut rendered = Vec::with_capacity(self.entries.len());
        for entry in &self.entries {
            let key_repr = render_value_repr(context, &entry.key)?;
            let value_repr = render_value_repr(context, &entry.value)?;
            rendered.push(format!("{key_repr}: {value_repr}"));
        }
        Ok(format!("{{{}}}", rendered.join(", ")))
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
                Ok(bound_method(move |context, mut args| {
                    RuntimeError::expect_method_arity("__getitem__", 1, args.len())?;
                    let key = args.pop().expect("len checked above");
                    with_dict(&receiver, |dict| dict.get_item_value(context, key))
                }))
            }
            "__setitem__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |context, mut args| {
                    RuntimeError::expect_method_arity("__setitem__", 2, args.len())?;
                    let value = args.pop().expect("len checked above");
                    let key = args.pop().expect("len checked above");
                    with_dict_mut(&receiver, |dict| dict.set_item_value(context, key, value))?;
                    Ok(Value::none_object())
                }))
            }
            "__str__" | "__repr__" => {
                let receiver = receiver.clone();
                let method = attribute.to_string();
                Ok(bound_method(move |context, args| {
                    RuntimeError::expect_method_arity(&method, 0, args.len())?;
                    let rendered = with_dict(&receiver, |dict| dict.render(context))?;
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
        .expect("dict helper receiver must be DictObject");
    f(dict)
}

fn with_dict_mut<R>(receiver: &ObjectRef, f: impl FnOnce(&mut DictObject) -> R) -> R {
    let mut object = receiver.borrow_mut();
    let dict = object
        .as_any_mut()
        .downcast_mut::<DictObject>()
        .expect("dict helper receiver must be DictObject");
    f(dict)
}

fn render_value_repr(context: &mut dyn CallContext, value: &Value) -> Result<String, RuntimeError> {
    let repr_method = value.get_attribute("__repr__")?;
    let repr_value = repr_method.call(context, vec![])?;
    let Some(rendered) = repr_value.as_str() else {
        return Err(RuntimeError::InvalidArgumentType {
            operation: "__repr__".to_string(),
            argument: "return".to_string(),
            expected: "str".to_string(),
            got: repr_value.type_name().to_string(),
        });
    };
    Ok(rendered.to_string())
}

#[cfg(test)]
mod tests {
    use super::DictObject;
    use crate::runtime::error::RuntimeError;
    use crate::runtime::object::{CallContext, CallableId};
    use crate::runtime::value::Value;

    struct TestCallContext;

    impl CallContext for TestCallContext {
        fn call_callable(
            &mut self,
            _callable_id: &CallableId,
            _args: Vec<Value>,
        ) -> Result<Value, RuntimeError> {
            panic!("unexpected call in dict tests")
        }
    }

    #[test]
    fn supports_len_get_set_and_render() {
        let mut context = TestCallContext;
        let mut dict = DictObject::new(
            vec![
                (Value::string_object("a".to_string()), Value::int_object(1)),
                (Value::string_object("b".to_string()), Value::int_object(2)),
            ],
            &mut context,
        )
        .expect("dict should build");

        assert_eq!(dict.__len__(), 2);
        assert_eq!(
            dict.get_item_value(&mut context, Value::string_object("a".to_string()))
                .expect("key should exist")
                .to_output(),
            "1"
        );
        dict.set_item_value(
            &mut context,
            Value::string_object("a".to_string()),
            Value::int_object(7),
        )
        .expect("set should work");
        dict.set_item_value(&mut context, Value::int_object(3), Value::int_object(9))
            .expect("set should work");
        assert_eq!(dict.__len__(), 3);
        assert_eq!(
            dict.get_item_value(&mut context, Value::string_object("a".to_string()))
                .expect("updated key should exist")
                .to_output(),
            "7"
        );
        assert_eq!(
            dict.render(&mut context).expect("render should succeed"),
            "{\"a\": 7, \"b\": 2, 3: 9}"
        );
    }

    #[test]
    fn bool_and_int_keys_alias() {
        let mut context = TestCallContext;
        let mut dict = DictObject::new(
            vec![(Value::bool_object(true), Value::int_object(1))],
            &mut context,
        )
        .expect("dict should build");
        dict.set_item_value(&mut context, Value::int_object(1), Value::int_object(9))
            .expect("set should work");
        assert_eq!(dict.__len__(), 1);
        assert_eq!(
            dict.get_item_value(&mut context, Value::bool_object(true))
                .expect("key should exist")
                .to_output(),
            "9"
        );
    }

    #[test]
    fn reports_missing_key_and_unhashable_type() {
        let mut context = TestCallContext;
        let dict = DictObject::new(
            vec![(Value::int_object(1), Value::int_object(2))],
            &mut context,
        )
        .expect("dict should build");
        assert_eq!(
            dict.get_item_value(&mut context, Value::int_object(3))
                .expect_err("missing key should fail"),
            RuntimeError::MissingKey {
                key: "3".to_string()
            }
        );
        assert_eq!(
            dict.get_item_value(&mut context, Value::list_object(vec![]))
                .expect_err("unhashable key should fail"),
            RuntimeError::UnhashableType {
                type_name: "list".to_string()
            }
        );
    }
}
