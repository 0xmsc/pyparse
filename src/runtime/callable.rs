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

#[derive(Clone)]
pub(crate) struct BoundMethodObject {
    callable: BoundMethodCallable,
}

impl BoundMethodObject {
    pub(crate) fn new(callable: BoundMethodCallable) -> Self {
        Self { callable }
    }

    pub(crate) fn callable(&self) -> BoundMethodCallable {
        self.callable.clone()
    }
}

#[derive(Clone)]
pub(crate) struct MethodWrapperObject {
    callable: BoundMethodCallable,
}

impl MethodWrapperObject {
    pub(crate) fn new(callable: BoundMethodCallable) -> Self {
        Self { callable }
    }

    pub(crate) fn callable(&self) -> BoundMethodCallable {
        self.callable.clone()
    }
}

impl fmt::Debug for BoundMethodObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("BoundMethodObject(<callable>)")
    }
}

impl fmt::Debug for MethodWrapperObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("MethodWrapperObject(<callable>)")
    }
}

impl RuntimeObject for BuiltinFunctionObject {
    fn type_name(&self) -> &'static str {
        "builtin_function_or_method"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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
                type_name: "builtin_function_or_method".to_string(),
            }),
        }
    }

    fn invoke(
        &self,
        _receiver: ObjectRef,
        _context: &mut dyn CallContext,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        Err(RuntimeError::ObjectNotCallable {
            type_name: self.type_name().to_string(),
        })
    }
}

impl RuntimeObject for FunctionObject {
    fn type_name(&self) -> &'static str {
        "function"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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
                type_name: "function".to_string(),
            }),
        }
    }

    fn invoke(
        &self,
        _receiver: ObjectRef,
        _context: &mut dyn CallContext,
        _args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        Err(RuntimeError::ObjectNotCallable {
            type_name: self.type_name().to_string(),
        })
    }
}

impl RuntimeObject for BoundMethodObject {
    fn type_name(&self) -> &'static str {
        "method"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match attribute {
            "__call__" => {
                let callable = self.callable();
                Ok(Value::method_wrapper_object(callable))
            }
            "__str__" | "__repr__" => {
                let method = attribute.to_string();
                Ok(bound_method(move |_context, args| {
                    RuntimeError::expect_method_arity(&method, 0, args.len())?;
                    Ok(Value::string_object("<bound method>".to_string()))
                }))
            }
            _ => Err(RuntimeError::UnknownAttribute {
                attribute: attribute.to_string(),
                type_name: "method".to_string(),
            }),
        }
    }

    fn invoke(
        &self,
        _receiver: ObjectRef,
        context: &mut dyn CallContext,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let callable = self.callable();
        callable(context, args)
    }
}

impl RuntimeObject for MethodWrapperObject {
    fn type_name(&self) -> &'static str {
        "method-wrapper"
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn get_attribute(&self, _receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
        match attribute {
            "__call__" => {
                let callable = self.callable();
                Ok(Value::method_wrapper_object(callable))
            }
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
                type_name: "method-wrapper".to_string(),
            }),
        }
    }

    fn invoke(
        &self,
        _receiver: ObjectRef,
        context: &mut dyn CallContext,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let callable = self.callable();
        callable(context, args)
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
        let bound_method = Value::bound_method_object(Rc::new(|_context, _args| {
            Ok(Value::int_object(7))
        }));
        let method_call = bound_method
            .get_attribute("__call__")
            .expect("__call__ should exist");
        assert_eq!(bound_method.type_name(), "method");
        assert_eq!(method_call.type_name(), "method-wrapper");

        let mut context = TestCallContext;
        let result = method_call.call(&mut context, vec![]).expect("call should work");
        assert_eq!(downcast_i64(&result), Some(7));
    }
}
