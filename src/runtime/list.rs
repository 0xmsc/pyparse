use crate::runtime::int::downcast_i64;
use crate::runtime::object::{AttributeError, BinaryOpError, MethodError, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;

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
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "list"
    }

    fn is_truthy(&self) -> bool {
        !self.is_empty()
    }

    fn to_output(&self, render_value: &dyn Fn(&Value) -> String) -> String {
        let rendered = self.iter().map(render_value).collect::<Vec<_>>().join(", ");
        format!("[{rendered}]")
    }

    fn get_attribute_method_name(&self, attribute: &str) -> Result<String, AttributeError> {
        if attribute == "append" {
            return Ok(attribute.to_string());
        }
        Err(AttributeError::UnknownAttribute {
            attribute: attribute.to_string(),
            type_name: "list".to_string(),
        })
    }

    fn len(&self) -> Result<usize, ListError> {
        Ok(self.__len__())
    }

    fn get_item(&self, index: Value) -> Result<Value, ListError> {
        let Some(index) = downcast_i64(&index) else {
            return Err(ListError::ExpectedIntegerType {
                got: format!("{index:?}"),
            });
        };
        self.__getitem__(index)
    }

    fn set_item(&mut self, index: Value, value: Value) -> Result<(), ListError> {
        let Some(index) = downcast_i64(&index) else {
            return Err(ListError::ExpectedIntegerType {
                got: format!("{index:?}"),
            });
        };
        self.__setitem__(index, value)
    }

    fn call_method(&mut self, method: &str, mut args: Vec<Value>) -> Result<(), MethodError> {
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
                Ok(())
            }
            _ => Err(MethodError::UnknownMethod {
                method: method.to_string(),
                type_name: "list".to_string(),
            }),
        }
    }

    fn add(&self, _rhs: &Value) -> Result<Value, BinaryOpError> {
        Err(BinaryOpError::ExpectedIntegerType {
            got: "list".to_string(),
        })
    }

    fn sub(&self, _rhs: &Value) -> Result<Value, BinaryOpError> {
        Err(BinaryOpError::ExpectedIntegerType {
            got: "list".to_string(),
        })
    }

    fn lt(&self, _rhs: &Value) -> Result<Value, BinaryOpError> {
        Err(BinaryOpError::ExpectedIntegerType {
            got: "list".to_string(),
        })
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
