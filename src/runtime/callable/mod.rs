mod builtin_function_object;
mod callable_object;
mod function_object;

pub(crate) use builtin_function_object::BuiltinFunctionObject;
pub(crate) use callable_object::CallableObject;
pub(crate) use function_object::FunctionObject;

#[cfg(test)]
mod tests {
    use crate::runtime::error::RuntimeError;
    use crate::runtime::int::downcast_i64;
    use crate::runtime::object::CallContext;
    use crate::runtime::value::Value;
    use std::rc::Rc;

    struct TestCallContext;

    impl CallContext for TestCallContext {
        fn call_callable(
            &mut self,
            _callable_id: &crate::runtime::object::CallableId,
            _args: Vec<Value>,
        ) -> Result<Value, RuntimeError> {
            panic!("unexpected call in callable tests")
        }
    }

    #[test]
    fn bound_method_call_attribute_is_method_wrapper() {
        let bound_method =
            Value::bound_method_object(Rc::new(|_context, _args| Ok(Value::int_object(7))));
        let method_call = bound_method
            .get_attribute("__call__")
            .expect("__call__ should exist");
        assert_eq!(bound_method.type_name(), "method");
        assert_eq!(method_call.type_name(), "method-wrapper");

        let mut context = TestCallContext;
        let result = method_call
            .call(&mut context, vec![])
            .expect("call should work");
        assert_eq!(downcast_i64(&result), Some(7));
    }
}
