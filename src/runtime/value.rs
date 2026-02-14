use crate::builtins::BuiltinFunction;
use crate::runtime::bool::BoolObject;
use crate::runtime::callable::{BoundMethodObject, BuiltinFunctionObject, FunctionObject};
use crate::runtime::int::IntObject;
use crate::runtime::list::{ListError, ListObject};
use crate::runtime::none::NoneObject;
use crate::runtime::object::{AttributeError, CallTarget, MethodError, ObjectRef, new_list_object};
use crate::runtime::string::StringObject;
use std::any::Any;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub(crate) struct Value(ObjectRef);

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CallTargetError {
    pub(crate) type_name: String,
}

impl Value {
    pub(crate) fn from_object(object: ObjectRef) -> Self {
        Self(object)
    }

    pub(crate) fn object_ref(&self) -> ObjectRef {
        self.0.clone()
    }

    pub(crate) fn type_name(&self) -> &'static str {
        let object = self.0.borrow();
        let any = &**object as &dyn Any;
        if any.is::<IntObject>() {
            "int"
        } else if any.is::<BoolObject>() {
            "bool"
        } else if any.is::<StringObject>() {
            "str"
        } else if any.is::<NoneObject>() {
            "NoneType"
        } else if any.is::<ListObject<Value>>() {
            "list"
        } else if any.is::<BuiltinFunctionObject>() {
            "builtin_function_or_method"
        } else if any.is::<FunctionObject>() {
            "function"
        } else if any.is::<BoundMethodObject>() {
            "method"
        } else {
            "object"
        }
    }

    pub(crate) fn to_output(&self) -> String {
        let object = self.0.borrow();
        let any = &**object as &dyn Any;
        if let Some(int) = any.downcast_ref::<IntObject>() {
            int.value().to_string()
        } else if let Some(boolean) = any.downcast_ref::<BoolObject>() {
            if boolean.value() {
                "True".to_string()
            } else {
                "False".to_string()
            }
        } else if let Some(string) = any.downcast_ref::<StringObject>() {
            string.value().to_string()
        } else if any.is::<NoneObject>() {
            "None".to_string()
        } else if let Some(list) = any.downcast_ref::<ListObject<Value>>() {
            let rendered = list
                .iter()
                .map(Value::to_output)
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{rendered}]")
        } else if any.is::<BuiltinFunctionObject>() {
            "<built-in function>".to_string()
        } else if let Some(function) = any.downcast_ref::<FunctionObject>() {
            format!("<function {}>", function.name())
        } else if any.is::<BoundMethodObject>() {
            "<bound method>".to_string()
        } else {
            format!("<{} object>", self.type_name())
        }
    }

    pub(crate) fn is_truthy(&self) -> bool {
        let object = self.0.borrow();
        let any = &**object as &dyn Any;
        if let Some(int) = any.downcast_ref::<IntObject>() {
            int.value() != 0
        } else if let Some(boolean) = any.downcast_ref::<BoolObject>() {
            boolean.value()
        } else if let Some(string) = any.downcast_ref::<StringObject>() {
            !string.value().is_empty()
        } else if any.is::<NoneObject>() {
            false
        } else if let Some(list) = any.downcast_ref::<ListObject<Value>>() {
            !list.is_empty()
        } else {
            true
        }
    }

    pub(crate) fn call_target(&self) -> Result<CallTarget, CallTargetError> {
        let object = self.0.borrow();
        let any = &**object as &dyn Any;
        if let Some(builtin) = any.downcast_ref::<BuiltinFunctionObject>() {
            Ok(builtin.call_target())
        } else if let Some(function) = any.downcast_ref::<FunctionObject>() {
            Ok(function.call_target())
        } else if let Some(bound_method) = any.downcast_ref::<BoundMethodObject>() {
            Ok(bound_method.call_target())
        } else {
            Err(CallTargetError {
                type_name: self.type_name().to_string(),
            })
        }
    }

    pub(crate) fn get_attribute(&self, attribute: &str) -> Result<Value, AttributeError> {
        self.0.borrow().get_attribute(self.object_ref(), attribute)
    }

    pub(crate) fn len(&self) -> Result<usize, ListError> {
        let object = self.0.borrow();
        let any = &**object as &dyn Any;
        if let Some(list) = any.downcast_ref::<ListObject<Value>>() {
            Ok(list.__len__())
        } else {
            Err(ListError::ExpectedListType {
                got: self.type_name().to_string(),
            })
        }
    }

    pub(crate) fn get_item(&self, index: Value) -> Result<Value, ListError> {
        let object = self.0.borrow();
        let any = &**object as &dyn Any;
        if let Some(list) = any.downcast_ref::<ListObject<Value>>() {
            list.get_item_value(index)
        } else {
            Err(ListError::ExpectedListType {
                got: self.type_name().to_string(),
            })
        }
    }

    pub(crate) fn set_item(&self, index: Value, value: Value) -> Result<(), ListError> {
        let type_name = self.type_name().to_string();
        let mut object = self.0.borrow_mut();
        let any = &mut **object as &mut dyn Any;
        if let Some(list) = any.downcast_mut::<ListObject<Value>>() {
            list.set_item_value(index, value)
        } else {
            Err(ListError::ExpectedListType { got: type_name })
        }
    }

    pub(crate) fn call_method(&self, method: &str, args: Vec<Value>) -> Result<Value, MethodError> {
        let type_name = self.type_name().to_string();
        let mut object = self.0.borrow_mut();
        let any = &mut **object as &mut dyn Any;
        if let Some(list) = any.downcast_mut::<ListObject<Value>>() {
            list.call_method(method, args)
        } else if let Some(int) = any.downcast_mut::<IntObject>() {
            int.call_method(method, args)
        } else {
            Err(MethodError::UnknownMethod {
                method: method.to_string(),
                type_name,
            })
        }
    }

    pub(crate) fn list_object(values: Vec<Value>) -> Self {
        Self(new_list_object(values))
    }

    pub(crate) fn int_object(value: i64) -> Self {
        Self(Rc::new(RefCell::new(Box::new(IntObject::new(value)))))
    }

    pub(crate) fn bool_object(value: bool) -> Self {
        Self(Rc::new(RefCell::new(Box::new(BoolObject::new(value)))))
    }

    pub(crate) fn string_object(value: String) -> Self {
        Self(Rc::new(RefCell::new(Box::new(StringObject::new(value)))))
    }

    pub(crate) fn none_object() -> Self {
        Self(Rc::new(RefCell::new(Box::new(NoneObject::new()))))
    }

    pub(crate) fn builtin_function_object(builtin: BuiltinFunction) -> Self {
        Self(Rc::new(RefCell::new(Box::new(BuiltinFunctionObject::new(
            builtin,
        )))))
    }

    pub(crate) fn function_object(name: String) -> Self {
        Self(Rc::new(RefCell::new(Box::new(FunctionObject::new(name)))))
    }

    pub(crate) fn bound_method_object(receiver: ObjectRef, method: String) -> Self {
        Self(Rc::new(RefCell::new(Box::new(BoundMethodObject::new(
            receiver, method,
        )))))
    }
}
