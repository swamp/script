/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
/*
//use crate::typereg::TypeRegistry;
use crate::value::Value;
use fixed32::Fp;
use swamp_script_types::prelude::*;
pub trait SwampExport: Sized {
    fn get_resolved_type(registry: &TypeRegistry) -> Type;
    fn to_swamp_value(&self, registry: &TypeRegistry) -> Value; // Added registry parameter
    /// # Errors
    ///
    fn from_swamp_value(value: &Value) -> Result<Self, String>;
}
impl SwampExport for Fp {
    fn get_resolved_type(registry: &TypeRegistry) -> Type {
        registry.get_float_type()
    }

    fn to_swamp_value(&self, _registry: &TypeRegistry) -> Value {
        Value::Float(*self)
    }
    fn from_swamp_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::Float(f) => Ok(*f),
            _ => Err("Expected Float value".to_string()),
        }
    }
}
// Primitive type implementations
impl SwampExport for i32 {
    fn get_resolved_type(registry: &TypeRegistry) -> Type {
        registry.get_int_type()
    }

    fn to_swamp_value(&self, _registry: &TypeRegistry) -> Value {
        Value::Int(*self)
    }

    fn from_swamp_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::Int(i) => Ok(*i),
            _ => Err("Expected Int value".to_string()),
        }
    }
}

impl SwampExport for String {
    fn get_resolved_type(registry: &TypeRegistry) -> Type {
        registry.get_string_type()
    }

    fn to_swamp_value(&self, _registry: &TypeRegistry) -> Value {
        Value::String(self.clone())
    }

    fn from_swamp_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::String(s) => Ok(s.clone()),
            _ => Err("Expected String value".to_string()),
        }
    }
}

impl SwampExport for bool {
    fn get_resolved_type(registry: &TypeRegistry) -> Type {
        registry.get_bool_type()
    }

    fn to_swamp_value(&self, _registry: &TypeRegistry) -> Value {
        Value::Bool(*self)
    }

    fn from_swamp_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::Bool(b) => Ok(*b),
            _ => Err(format!("Expected Bool value {value}")),
        }
    }
}


 */
