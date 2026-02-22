use crate::runtime::error::RuntimeError;
use crate::runtime::exception::RaisedException;
use crate::runtime::method::bound_method;
use crate::runtime::object::{ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub(crate) struct ListObject {
    values: Vec<Value>,
}

#[derive(Debug, Clone)]
struct ListIteratorObject {
    list: ObjectRef,
    index: usize,
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
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "list"
    }

    fn get_attribute(&self, receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match attribute {
            "__iter__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity("__iter__", 0, args.len())?;
                    let iterator = ListIteratorObject::new(receiver.clone());
                    Ok(Value::from_object_ref(Rc::new(RefCell::new(Box::new(
                        iterator,
                    )))))
                }))
            }
            "append" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |_context, mut args| {
                    RuntimeError::expect_method_arity("append", 1, args.len())?;
                    with_list_mut(&receiver, |list| {
                        list.append(args.pop().expect("len checked above"));
                    });
                    Ok(Value::none_object())
                }))
            }
            "__len__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity("__len__", 0, args.len())?;
                    Ok(with_list(&receiver, |list| {
                        Value::int_object(list.__len__() as i64)
                    }))
                }))
            }
            "__getitem__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |_context, mut args| {
                    RuntimeError::expect_method_arity("__getitem__", 1, args.len())?;
                    let index = args.pop().expect("len checked above");
                    with_list(&receiver, |list| list.get_item_value(index))
                }))
            }
            "__setitem__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |_context, mut args| {
                    RuntimeError::expect_method_arity("__setitem__", 2, args.len())?;
                    let value = args.pop().expect("len checked above");
                    let index = args.pop().expect("len checked above");
                    with_list_mut(&receiver, |list| list.set_item_value(index, value))?;
                    Ok(Value::none_object())
                }))
            }
            "__str__" | "__repr__" => {
                let receiver = receiver.clone();
                let method = attribute.to_string();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity(&method, 0, args.len())?;
                    Ok(with_list(&receiver, |list| {
                        Value::string_object(list.render())
                    }))
                }))
            }
            _ => Err(RuntimeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: self.type_name().to_string(),
            }),
        }
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
        let Some(index) = index.as_int() else {
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
        let Some(index) = index.as_int() else {
            return Err(RuntimeError::InvalidArgumentType {
                operation: "__setitem__".to_string(),
                argument: "index".to_string(),
                expected: "int".to_string(),
                got: index.type_name().to_string(),
            });
        };
        self.__setitem__(index, value)
    }
}

impl ListIteratorObject {
    fn new(list: ObjectRef) -> Self {
        Self { list, index: 0 }
    }

    fn next_value(&mut self) -> Result<Value, RuntimeError> {
        let result = with_list(&self.list, |list| list.__getitem__(self.index as i64));
        match result {
            Ok(value) => {
                self.index += 1;
                Ok(value)
            }
            Err(RuntimeError::IndexOutOfBounds { .. }) => Err(RuntimeError::Raised {
                exception: RaisedException::stop_iteration(),
            }),
            Err(error) => Err(error),
        }
    }
}

impl RuntimeObject for ListIteratorObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "list_iterator"
    }

    fn get_attribute(&self, receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match attribute {
            "__iter__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity("__iter__", 0, args.len())?;
                    Ok(Value::from_object_ref(receiver.clone()))
                }))
            }
            "__next__" => {
                let receiver = receiver.clone();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity("__next__", 0, args.len())?;
                    with_list_iterator_mut(&receiver, |iterator| iterator.next_value())
                }))
            }
            _ => Err(RuntimeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: self.type_name().to_string(),
            }),
        }
    }
}

fn with_list<R>(receiver: &ObjectRef, f: impl FnOnce(&ListObject) -> R) -> R {
    let object = receiver.borrow();
    let list = object
        .as_any()
        .downcast_ref::<ListObject>()
        .expect("list get_attribute receiver must be ListObject");
    f(list)
}

fn with_list_mut<R>(receiver: &ObjectRef, f: impl FnOnce(&mut ListObject) -> R) -> R {
    let mut object = receiver.borrow_mut();
    let list = object
        .as_any_mut()
        .downcast_mut::<ListObject>()
        .expect("list get_attribute receiver must be ListObject");
    f(list)
}

fn with_list_iterator_mut<R>(
    receiver: &ObjectRef,
    f: impl FnOnce(&mut ListIteratorObject) -> R,
) -> R {
    let mut object = receiver.borrow_mut();
    let iterator = object
        .as_any_mut()
        .downcast_mut::<ListIteratorObject>()
        .expect("list iterator receiver must be ListIteratorObject");
    f(iterator)
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
