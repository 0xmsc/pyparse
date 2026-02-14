#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ListError {
    NegativeIndex { index: i64 },
    OutOfBounds { index: usize, len: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ListObject<Value> {
    values: Vec<Value>,
}

impl<Value> ListObject<Value> {
    pub(crate) fn new(values: Vec<Value>) -> Self {
        Self { values }
    }

    pub(crate) fn __len__(&self) -> usize {
        self.values.len()
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub(crate) fn __setitem__(&mut self, index: i64, value: Value) -> Result<(), ListError> {
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

    pub(crate) fn append(&mut self, value: Value) {
        self.values.push(value);
    }

    pub(crate) fn iter(&self) -> std::slice::Iter<'_, Value> {
        self.values.iter()
    }
}

impl<Value: Clone> ListObject<Value> {
    pub(crate) fn __getitem__(&self, index: i64) -> Result<Value, ListError> {
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
