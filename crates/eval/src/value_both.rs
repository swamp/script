use crate::prelude::ValueReference;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_core::prelude::Value;
use swamp_script_core::value::RustType;

#[derive(Debug, Clone)]
pub enum VariableValue {
    Value(Value),
    Reference(ValueReference),
}

impl PartialEq for VariableValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Reference(r1), Self::Value(other)) => r1.0.borrow().eq(other),
            (Self::Value(other), Self::Reference(r2)) => other.eq(&*r2.0.borrow()),
            (Self::Value(v1), Self::Value(v2)) => v1 == v2,
            (Self::Reference(r1), Self::Reference(r2)) => r1.0.borrow().eq(&*r2.0.borrow()),
        }
    }
}

impl VariableValue {
    pub fn downcast_rust_mut_or_not<T: RustType + 'static>(&self) -> Option<Rc<RefCell<Box<T>>>> {
        match self {
            VariableValue::Value(v) => v.downcast_rust(),
            VariableValue::Reference(r) => r.downcast_rust_mut(),
        }
    }

    pub fn convert_to_string_if_needed(&self) -> String {
        match self {
            Self::Value(v) => v.convert_to_string_if_needed(),
            Self::Reference(r) => r.convert_to_string_if_needed(),
        }
    }
}

#[inline]
pub fn convert_to_values(mem_values: &[VariableValue]) -> Option<Vec<Value>> {
    mem_values
        .iter()
        .map(|e| match e {
            VariableValue::Value(v) => Some(v.clone()),
            _ => None,
        })
        .collect()
}
