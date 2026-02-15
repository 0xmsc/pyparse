use crate::runtime::error::RuntimeError;
use crate::runtime::int::downcast_i64;
use crate::runtime::object::{ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub(crate) struct ListObject {
    values: Vec<Value>,
}

impl ListObject {
    pub(crate) fn new(values: Vec<Value>) -> Self {
        Self { values }
    }

    pub(crate) fn __len__(&self) -> usize {
        self.values.len()
    }

    pub(crate) fn __setitem__(&mut self, index: i64, value: Value) -> Result<(), RuntimeError> {
        if index < 0 {
            return Err(RuntimeError::NegativeIndex { index });
        }
        let index = index as usize;
        if index >= self.values.len() {
            return Err(RuntimeError::IndexOutOfBounds {
                index,
                len: self.values.len(),
            });
        }
        self.values[index] = value;
        Ok(())
    }

    pub(crate) fn append(&mut self, value: Value) {
        self.values.push(value);
    }

    pub(crate) fn iter(&self) -> std::slice::Iter<'_, Value> {
        self.values.iter()
    }

    pub(crate) fn __getitem__(&self, index: i64) -> Result<Value, RuntimeError> {
        if index < 0 {
            return Err(RuntimeError::NegativeIndex { index });
        }
        let index = index as usize;
        self.values
            .get(index)
            .cloned()
            .ok_or(RuntimeError::IndexOutOfBounds {
                index,
                len: self.values.len(),
            })
    }
}

impl RuntimeObject for ListObject {
    fn get_attribute(&self, receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        if matches!(
            attribute,
            "append" | "__len__" | "__getitem__" | "__setitem__" | "__str__" | "__repr__"
        ) {
            let receiver = receiver.clone();
            let method = attribute.to_string();
            return Ok(Value::bound_method_object(Rc::new(move |args| {
                call_method_on_receiver(&receiver, &method, args)
            })));
        }
        Err(RuntimeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "list".to_string(),
        })
    }
}

impl ListObject {
    fn render(&self) -> String {
        let rendered = self
            .iter()
            .map(Value::to_output)
            .collect::<Vec<_>>()
            .join(", ");
        format!("[{rendered}]")
    }

    pub(crate) fn get_item_value(&self, index: Value) -> Result<Value, RuntimeError> {
        let Some(index) = downcast_i64(&index) else {
            return Err(RuntimeError::InvalidArgumentType {
                operation: "__getitem__".to_string(),
                argument: "index".to_string(),
                expected: "int".to_string(),
                got: index.type_name().to_string(),
            });
        };
        self.__getitem__(index)
    }

    pub(crate) fn set_item_value(
        &mut self,
        index: Value,
        value: Value,
    ) -> Result<(), RuntimeError> {
        let Some(index) = downcast_i64(&index) else {
            return Err(RuntimeError::InvalidArgumentType {
                operation: "__setitem__".to_string(),
                argument: "index".to_string(),
                expected: "int".to_string(),
                got: index.type_name().to_string(),
            });
        };
        self.__setitem__(index, value)
    }

    pub(crate) fn call_method(
        &mut self,
        method: &str,
        mut args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "append" => {
                if args.len() != 1 {
                    return Err(RuntimeError::ArityMismatch {
                        method: "append".to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }
                self.append(args.pop().expect("len checked above"));
                Ok(Value::none_object())
            }
            "__len__" => {
                if !args.is_empty() {
                    return Err(RuntimeError::ArityMismatch {
                        method: "__len__".to_string(),
                        expected: 0,
                        found: args.len(),
                    });
                }
                Ok(Value::int_object(self.__len__() as i64))
            }
            "__getitem__" => {
                if args.len() != 1 {
                    return Err(RuntimeError::ArityMismatch {
                        method: "__getitem__".to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }
                let index = args.pop().expect("len checked above");
                self.get_item_value(index)
            }
            "__setitem__" => {
                if args.len() != 2 {
                    return Err(RuntimeError::ArityMismatch {
                        method: "__setitem__".to_string(),
                        expected: 2,
                        found: args.len(),
                    });
                }
                let value = args.pop().expect("len checked above");
                let index = args.pop().expect("len checked above");
                self.set_item_value(index, value)?;
                Ok(Value::none_object())
            }
            "__str__" | "__repr__" => {
                if !args.is_empty() {
                    return Err(RuntimeError::ArityMismatch {
                        method: method.to_string(),
                        expected: 0,
                        found: args.len(),
                    });
                }
                Ok(Value::string_object(self.render()))
            }
            _ => Err(RuntimeError::UnknownMethod {
                method: method.to_string(),
                type_name: "list".to_string(),
            }),
        }
    }
}

fn call_method_on_receiver(
    receiver: &ObjectRef,
    method: &str,
    args: Vec<Value>,
) -> Result<Value, RuntimeError> {
    let mut object = receiver.borrow_mut();
    let any = &mut **object as &mut dyn Any;
    let list = any
        .downcast_mut::<ListObject>()
        .expect("list get_attribute receiver must be ListObject");
    list.call_method(method, args)
}

#[cfg(test)]
mod tests {
    use super::ListObject;
    use crate::runtime::error::RuntimeError;
    use crate::runtime::value::Value;

    #[test]
    fn supports_len_get_set_and_append() {
        let mut list = ListObject::new(vec![Value::int_object(1), Value::int_object(2)]);
        assert_eq!(list.__len__(), 2);
        assert_eq!(list.__getitem__(0).expect("index 0").to_output(), "1");
        list.__setitem__(1, Value::int_object(7))
            .expect("set item at index 1");
        list.append(Value::int_object(9));
        assert_eq!(list.__len__(), 3);
        assert_eq!(list.__getitem__(1).expect("index 1").to_output(), "7");
        assert_eq!(list.__getitem__(2).expect("index 2").to_output(), "9");
    }

    #[test]
    fn reports_negative_and_out_of_bounds_indexes() {
        let mut list = ListObject::new(vec![Value::int_object(1)]);
        assert_eq!(
            list.__getitem__(-1)
                .expect_err("negative index should fail"),
            RuntimeError::NegativeIndex { index: -1 }
        );
        assert_eq!(
            list.__getitem__(2).expect_err("oob index should fail"),
            RuntimeError::IndexOutOfBounds { index: 2, len: 1 }
        );
        assert_eq!(
            list.__setitem__(-1, Value::int_object(5))
                .expect_err("negative index assignment should fail"),
            RuntimeError::NegativeIndex { index: -1 }
        );
        assert_eq!(
            list.__setitem__(2, Value::int_object(5))
                .expect_err("oob assignment should fail"),
            RuntimeError::IndexOutOfBounds { index: 2, len: 1 }
        );
    }
}
