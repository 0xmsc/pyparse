use crate::builtins::BuiltinFunction;
use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
use crate::runtime::object::{BoundMethodCallable, CallContext, ObjectRef, RuntimeObject};
use crate::runtime::value::Value;
use std::any::Any;
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BuiltinFunctionObject {
    builtin: BuiltinFunction,
}

impl BuiltinFunctionObject {
    pub(crate) fn new(builtin: BuiltinFunction) -> Self {
        Self { builtin }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunctionObject {
    name: String,
}

impl FunctionObject {
    pub(crate) fn new(name: String) -> Self {
        Self { name }
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CallableObjectKind {
    BoundMethod,
    MethodWrapper,
}

#[derive(Clone)]
pub(crate) struct CallableObject {
    kind: CallableObjectKind,
    callable: BoundMethodCallable,
}

impl CallableObject {
    pub(crate) fn bound_method(callable: BoundMethodCallable) -> Self {
        Self {
            kind: CallableObjectKind::BoundMethod,
            callable,
        }
    }

    pub(crate) fn method_wrapper(callable: BoundMethodCallable) -> Self {
        Self {
            kind: CallableObjectKind::MethodWrapper,
            callable,
        }
    }
}

impl fmt::Debug for CallableObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            CallableObjectKind::BoundMethod => f.write_str("CallableObject(method, <callable>)"),
            CallableObjectKind::MethodWrapper => {
                f.write_str("CallableObject(method-wrapper, <callable>)")
            }
        }
    }
}

impl RuntimeObject for BuiltinFunctionObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "builtin_function_or_method"
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match attribute {
            "__call__" => {
                let builtin = self.builtin;
                Ok(bound_method(move |context, args| {
                    context.call_builtin(builtin, args)
                }))
            }
            "__str__" | "__repr__" => {
                let method = attribute.to_string();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity(&method, 0, args.len())?;
                    Ok(Value::string_object("<built-in function>".to_string()))
                }))
            }
            _ => Err(RuntimeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: self.type_name().to_string(),
            }),
        }
    }

    fn call(
        &self,
        _receiver: ObjectRef,
        context: &mut dyn CallContext,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        context.call_builtin(self.builtin, args)
    }
}

impl RuntimeObject for FunctionObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        "function"
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match attribute {
            "__call__" => {
                let function_name = self.name.clone();
                Ok(bound_method(move |context, args| {
                    context.call_function_named(&function_name, args)
                }))
            }
            "__str__" | "__repr__" => {
                let method = attribute.to_string();
                let rendered = format!("<function {}>", self.name());
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity(&method, 0, args.len())?;
                    Ok(Value::string_object(rendered.clone()))
                }))
            }
            _ => Err(RuntimeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: self.type_name().to_string(),
            }),
        }
    }

    fn call(
        &self,
        _receiver: ObjectRef,
        context: &mut dyn CallContext,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        context.call_function_named(&self.name, args)
    }
}

impl RuntimeObject for CallableObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_name(&self) -> &'static str {
        match self.kind {
            CallableObjectKind::BoundMethod => "method",
            CallableObjectKind::MethodWrapper => "method-wrapper",
        }
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match self.kind {
            CallableObjectKind::BoundMethod => match attribute {
                "__call__" => Ok(Value::method_wrapper_object(self.callable.clone())),
                "__str__" | "__repr__" => {
                    let method = attribute.to_string();
                    Ok(bound_method(move |_context, args| {
                        RuntimeError::expect_method_arity(&method, 0, args.len())?;
                        Ok(Value::string_object("<bound method>".to_string()))
                    }))
                }
                _ => Err(RuntimeError::UnknownAttribute {
                    attribute: attribute.to_string(),
                    type_name: self.type_name().to_string(),
                }),
            },
            CallableObjectKind::MethodWrapper => match attribute {
                "__call__" => Ok(Value::method_wrapper_object(self.callable.clone())),
                "__str__" | "__repr__" => {
                    let method = attribute.to_string();
                    Ok(bound_method(move |_context, args| {
                        RuntimeError::expect_method_arity(&method, 0, args.len())?;
                        Ok(Value::string_object(
                            "<method-wrapper '__call__'>".to_string(),
                        ))
                    }))
                }
                _ => Err(RuntimeError::UnknownAttribute {
                    attribute: attribute.to_string(),
                    type_name: self.type_name().to_string(),
                }),
            },
        }
    }

    fn call(
        &self,
        _receiver: ObjectRef,
        context: &mut dyn CallContext,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        (self.callable)(context, args)
    }
}

#[cfg(test)]
mod tests {
    use crate::runtime::error::RuntimeError;
    use crate::runtime::int::downcast_i64;
    use crate::runtime::object::CallContext;
    use crate::runtime::value::Value;
    use std::rc::Rc;

    struct TestCallContext;

    impl CallContext for TestCallContext {
        fn call_builtin(
            &mut self,
            _builtin: crate::builtins::BuiltinFunction,
            _args: Vec<Value>,
        ) -> Result<Value, RuntimeError> {
            panic!("unexpected builtin call in callable tests")
        }

        fn call_function_named(
            &mut self,
            _name: &str,
            _args: Vec<Value>,
        ) -> Result<Value, RuntimeError> {
            panic!("unexpected function call in callable tests")
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
