use crate::builtins::BuiltinFunction;
use crate::runtime::error::RuntimeError;
use crate::runtime::method::bound_method;
use crate::runtime::object::{
    BoundMethodCallable, CallContext, ObjectRef, RuntimeObject, TypeObject, unknown_attribute,
    unsupported_attribute_assignment,
};
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

fn with_builtin_function<R>(
    receiver: &ObjectRef,
    f: impl FnOnce(&BuiltinFunctionObject) -> R,
) -> R {
    let object = receiver.borrow();
    let builtin = object
        .as_any()
        .downcast_ref::<BuiltinFunctionObject>()
        .expect("builtin function receiver must be BuiltinFunctionObject");
    f(builtin)
}

fn with_function<R>(receiver: &ObjectRef, f: impl FnOnce(&FunctionObject) -> R) -> R {
    let object = receiver.borrow();
    let function = object
        .as_any()
        .downcast_ref::<FunctionObject>()
        .expect("function receiver must be FunctionObject");
    f(function)
}

fn with_bound_method<R>(receiver: &ObjectRef, f: impl FnOnce(&BoundMethodObject) -> R) -> R {
    let object = receiver.borrow();
    let method = object
        .as_any()
        .downcast_ref::<BoundMethodObject>()
        .expect("bound method receiver must be BoundMethodObject");
    f(method)
}

fn with_method_wrapper<R>(receiver: &ObjectRef, f: impl FnOnce(&MethodWrapperObject) -> R) -> R {
    let object = receiver.borrow();
    let method_wrapper = object
        .as_any()
        .downcast_ref::<MethodWrapperObject>()
        .expect("method wrapper receiver must be MethodWrapperObject");
    f(method_wrapper)
}

impl RuntimeObject for BuiltinFunctionObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_object(&self) -> &'static TypeObject {
        &BUILTIN_FUNCTION_TYPE
    }
}

impl RuntimeObject for FunctionObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_object(&self) -> &'static TypeObject {
        &FUNCTION_TYPE
    }
}

impl RuntimeObject for BoundMethodObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_object(&self) -> &'static TypeObject {
        &BOUND_METHOD_TYPE
    }
}

impl RuntimeObject for MethodWrapperObject {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }

    fn type_object(&self) -> &'static TypeObject {
        &METHOD_WRAPPER_TYPE
    }
}

static BUILTIN_FUNCTION_TYPE: TypeObject = TypeObject::new(
    "builtin_function_or_method",
    builtin_function_get_attribute,
    unsupported_attribute_assignment,
    builtin_function_call,
);

static FUNCTION_TYPE: TypeObject = TypeObject::new(
    "function",
    function_get_attribute,
    unsupported_attribute_assignment,
    function_call,
);

static BOUND_METHOD_TYPE: TypeObject = TypeObject::new(
    "method",
    bound_method_get_attribute,
    unsupported_attribute_assignment,
    bound_method_call,
);

static METHOD_WRAPPER_TYPE: TypeObject = TypeObject::new(
    "method-wrapper",
    method_wrapper_get_attribute,
    unsupported_attribute_assignment,
    method_wrapper_call,
);

fn builtin_function_get_attribute(
    receiver: ObjectRef,
    attribute: &str,
) -> Result<Value, RuntimeError> {
    match attribute {
        "__call__" => {
            let builtin = with_builtin_function(&receiver, |builtin| builtin.builtin);
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
        _ => unknown_attribute(receiver, attribute),
    }
}

fn builtin_function_call(
    receiver: ObjectRef,
    context: &mut dyn CallContext,
    args: Vec<Value>,
) -> Result<Value, RuntimeError> {
    let builtin = with_builtin_function(&receiver, |builtin| builtin.builtin);
    context.call_builtin(builtin, args)
}

fn function_get_attribute(receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
    match attribute {
        "__call__" => {
            let function_name = with_function(&receiver, |function| function.name.clone());
            Ok(bound_method(move |context, args| {
                context.call_function_named(&function_name, args)
            }))
        }
        "__str__" | "__repr__" => {
            let method = attribute.to_string();
            let rendered = with_function(&receiver, |function| {
                format!("<function {}>", function.name())
            });
            Ok(bound_method(move |_context, args| {
                RuntimeError::expect_method_arity(&method, 0, args.len())?;
                Ok(Value::string_object(rendered.clone()))
            }))
        }
        _ => unknown_attribute(receiver, attribute),
    }
}

fn function_call(
    receiver: ObjectRef,
    context: &mut dyn CallContext,
    args: Vec<Value>,
) -> Result<Value, RuntimeError> {
    let function_name = with_function(&receiver, |function| function.name.clone());
    context.call_function_named(&function_name, args)
}

fn bound_method_get_attribute(receiver: ObjectRef, attribute: &str) -> Result<Value, RuntimeError> {
    match attribute {
        "__call__" => {
            let callable = with_bound_method(&receiver, BoundMethodObject::callable);
            Ok(Value::method_wrapper_object(callable))
        }
        "__str__" | "__repr__" => {
            let method = attribute.to_string();
            Ok(bound_method(move |_context, args| {
                RuntimeError::expect_method_arity(&method, 0, args.len())?;
                Ok(Value::string_object("<bound method>".to_string()))
            }))
        }
        _ => unknown_attribute(receiver, attribute),
    }
}

fn bound_method_call(
    receiver: ObjectRef,
    context: &mut dyn CallContext,
    args: Vec<Value>,
) -> Result<Value, RuntimeError> {
    let callable = with_bound_method(&receiver, BoundMethodObject::callable);
    callable(context, args)
}

fn method_wrapper_get_attribute(
    receiver: ObjectRef,
    attribute: &str,
) -> Result<Value, RuntimeError> {
    match attribute {
        "__call__" => {
            let callable = with_method_wrapper(&receiver, MethodWrapperObject::callable);
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
        _ => unknown_attribute(receiver, attribute),
    }
}

fn method_wrapper_call(
    receiver: ObjectRef,
    context: &mut dyn CallContext,
    args: Vec<Value>,
) -> Result<Value, RuntimeError> {
    let callable = with_method_wrapper(&receiver, MethodWrapperObject::callable);
    callable(context, args)
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
