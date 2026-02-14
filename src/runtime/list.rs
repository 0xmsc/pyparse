use crate::runtime::int::downcast_i64;
use crate::runtime::object::{AttributeError, MethodError, ObjectRef, RuntimeObject};
use crate::runtime::value::Value;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ListError {
    ExpectedListType { got: String },
    ExpectedIntegerType { got: String },
    NegativeIndex { index: i64 },
    OutOfBounds { index: usize, len: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ListObject<Element> {
    values: Vec<Element>,
}

impl<Element> ListObject<Element> {
    pub(crate) fn new(values: Vec<Element>) -> Self {
        Self { values }
    }

    pub(crate) fn __len__(&self) -> usize {
        self.values.len()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub(crate) fn __setitem__(&mut self, index: i64, value: Element) -> Result<(), ListError> {
        if index < 0 {
            return Err(ListError::NegativeIndex { index });
        }
        let index = index as usize;
        if index >= self.values.len() {
            return Err(ListError::OutOfBounds {
                index,
                len: self.values.len(),
            });
        }
        self.values[index] = value;
        Ok(())
    }

    pub(crate) fn append(&mut self, value: Element) {
        self.values.push(value);
    }

    pub(crate) fn iter(&self) -> std::slice::Iter<'_, Element> {
        self.values.iter()
    }
}

impl<Element: Clone> ListObject<Element> {
    pub(crate) fn __getitem__(&self, index: i64) -> Result<Element, ListError> {
        if index < 0 {
            return Err(ListError::NegativeIndex { index });
        }
        let index = index as usize;
        self.values
            .get(index)
            .cloned()
            .ok_or(ListError::OutOfBounds {
                index,
                len: self.values.len(),
            })
    }
}

impl RuntimeObject for ListObject<Value> {
    fn get_attribute(&self, receiver: ObjectRef, attribute: &str) -> Result<Value, AttributeError> {
        if matches!(attribute, "append" | "__getitem__" | "__setitem__") {
            return Ok(Value::bound_method_object(receiver, attribute.to_string()));
        }
        Err(AttributeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "list".to_string(),
        })
    }
}

impl ListObject<Value> {
    pub(crate) fn get_item_value(&self, index: Value) -> Result<Value, ListError> {
        let Some(index) = downcast_i64(&index) else {
            return Err(ListError::ExpectedIntegerType {
                got: format!("{index:?}"),
            });
        };
        self.__getitem__(index)
    }

    pub(crate) fn set_item_value(&mut self, index: Value, value: Value) -> Result<(), ListError> {
        let Some(index) = downcast_i64(&index) else {
            return Err(ListError::ExpectedIntegerType {
                got: format!("{index:?}"),
            });
        };
        self.__setitem__(index, value)
    }

    pub(crate) fn call_method(
        &mut self,
        method: &str,
        mut args: Vec<Value>,
    ) -> Result<Value, MethodError> {
        match method {
            "append" => {
                if args.len() != 1 {
                    return Err(MethodError::ArityMismatch {
                        method: "append".to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }
                self.append(args.pop().expect("len checked above"));
                Ok(Value::none_object())
            }
            "__getitem__" => {
                if args.len() != 1 {
                    return Err(MethodError::ArityMismatch {
                        method: "__getitem__".to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }
                let index = args.pop().expect("len checked above");
                self.get_item_value(index)
                    .map_err(MethodError::ListOperation)
            }
            "__setitem__" => {
                if args.len() != 2 {
                    return Err(MethodError::ArityMismatch {
                        method: "__setitem__".to_string(),
                        expected: 2,
                        found: args.len(),
                    });
                }
                let value = args.pop().expect("len checked above");
                let index = args.pop().expect("len checked above");
                self.set_item_value(index, value)
                    .map_err(MethodError::ListOperation)?;
                Ok(Value::none_object())
            }
            _ => Err(MethodError::UnknownMethod {
                method: method.to_string(),
                type_name: "list".to_string(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{ListError, ListObject};

    #[test]
    fn supports_len_get_set_and_append() {
        let mut list = ListObject::new(vec![1, 2]);
        assert_eq!(list.__len__(), 2);
        assert_eq!(list.__getitem__(0).expect("index 0"), 1);
        list.__setitem__(1, 7).expect("set item at index 1");
        list.append(9);
        assert_eq!(list.__len__(), 3);
        assert_eq!(list.__getitem__(1).expect("index 1"), 7);
        assert_eq!(list.__getitem__(2).expect("index 2"), 9);
    }

    #[test]
    fn reports_negative_and_out_of_bounds_indexes() {
        let mut list = ListObject::new(vec![1]);
        assert_eq!(
            list.__getitem__(-1)
                .expect_err("negative index should fail"),
            ListError::NegativeIndex { index: -1 }
        );
        assert_eq!(
            list.__getitem__(2).expect_err("oob index should fail"),
            ListError::OutOfBounds { index: 2, len: 1 }
        );
        assert_eq!(
            list.__setitem__(-1, 5)
                .expect_err("negative index assignment should fail"),
            ListError::NegativeIndex { index: -1 }
        );
        assert_eq!(
            list.__setitem__(2, 5)
                .expect_err("oob assignment should fail"),
            ListError::OutOfBounds { index: 2, len: 1 }
        );
    }
}
