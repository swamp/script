use crate::typereg::TypeRegistry;
use crate::value::Value;
use fixed32::Fp;
use swamp_script_semantic::prelude::*;

pub trait SwampExport: Sized {
    fn get_resolved_type(registry: &TypeRegistry) -> ResolvedType;
    fn to_swamp_value(&self, registry: &TypeRegistry) -> Value; // Added registry parameter
    fn from_swamp_value(value: &Value) -> Result<Self, String>;
}
impl SwampExport for Fp {
    fn to_swamp_value(&self, _registry: &TypeRegistry) -> Value {
        Value::Float(*self)
    }

    fn from_swamp_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::Float(f) => Ok(*f),
            _ => Err("Expected Float value".to_string()),
        }
    }
    fn get_resolved_type(registry: &TypeRegistry) -> ResolvedType {
        registry.get_float_type()
    }
}
// Primitive type implementations
impl SwampExport for i32 {
    fn get_resolved_type(registry: &TypeRegistry) -> ResolvedType {
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
    fn get_resolved_type(registry: &TypeRegistry) -> ResolvedType {
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
    fn get_resolved_type(registry: &TypeRegistry) -> ResolvedType {
        registry.get_bool_type()
    }

    fn to_swamp_value(&self, _registry: &TypeRegistry) -> Value {
        Value::Bool(*self)
    }

    fn from_swamp_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::Bool(b) => Ok(*b),
            _ => Err("Expected Bool value".to_string()),
        }
    }
}
