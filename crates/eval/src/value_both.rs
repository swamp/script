/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::prelude::ValueReference;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_core::prelude::{Value, ValueError, ValueRef};
use swamp_script_core::value::RustType;

#[derive(Debug, Clone)]
pub enum VariableValue {
    Value(Value),
    Reference(ValueRef),
}

impl VariableValue {
    pub(crate) fn to_value(&self) -> Value {
        match self {
            Self::Value(v) => v.clone(),
            Self::Reference(value_ref) => value_ref.borrow().clone(),
        }
    }
}

impl PartialEq for VariableValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Reference(r1), Self::Value(other)) => r1.borrow().eq(other),
            (Self::Value(other), Self::Reference(r2)) => other.eq(&*r2.borrow()),
            (Self::Value(v1), Self::Value(v2)) => v1 == v2,
            (Self::Reference(r1), Self::Reference(r2)) => r1.borrow().eq(&*r2.borrow()),
        }
    }
}

impl VariableValue {
    #[must_use]
    pub fn downcast_rust_mut_or_not<T: RustType + 'static>(&self) -> Option<Rc<RefCell<Box<T>>>> {
        match self {
            VariableValue::Value(v) => v.downcast_rust(),
            VariableValue::Reference(r) => ValueReference(r.clone()).downcast_rust_mut(),
        }
    }

    #[must_use]
    pub fn convert_to_string_if_needed(&self) -> String {
        match self {
            Self::Value(v) => v.convert_to_string_if_needed(),
            Self::Reference(r) => ValueReference(r.clone()).convert_to_string_if_needed(),
        }
    }

    /// # Errors
    ///
    pub fn into_iter(self) -> Result<Box<dyn Iterator<Item = Value>>, ValueError> {
        match self {
            Self::Value(v) => v.into_iter(),
            Self::Reference(_r) => Err(ValueError::CanNotCoerceToIterator),
        }
    }

    /// # Errors
    ///
    pub fn into_iter_pairs(self) -> Result<Box<dyn Iterator<Item = (Value, Value)>>, ValueError> {
        match self {
            Self::Value(v) => v.into_iter_pairs(),
            Self::Reference(_r) => Err(ValueError::CanNotCoerceToIterator),
        }
    }

    /// # Errors
    ///
    pub fn into_iter_pairs_mut(
        self,
    ) -> Result<Box<dyn Iterator<Item = (Value, ValueReference)>>, ValueError> {
        match self {
            Self::Value(_v) => Err(ValueError::CanNotCoerceToIterator),
            Self::Reference(r) => ValueReference(r).into_iter_mut_pairs(),
        }
    }
}

#[inline]
#[must_use]
pub fn convert_to_values(mem_values: &[VariableValue]) -> Option<Vec<Value>> {
    mem_values
        .iter()
        .map(|e| match e {
            VariableValue::Value(v) => Some(v.clone()),
            VariableValue::Reference(v) => Some(v.borrow().clone()),
        })
        .collect()
}
