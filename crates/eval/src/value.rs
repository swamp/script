/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::{EvalExternalFunctionRef, ValueWithSignal};
use fixed32::Fp;
use seq_fmt::{comma, comma_tuple};
use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;
use swamp_script_semantic::{
    FormatSpecifier, PrecisionType, ResolvedArrayTypeRef, ResolvedEnumVariantStructTypeRef,
    ResolvedEnumVariantTupleTypeRef, ResolvedEnumVariantTypeRef,
    ResolvedInternalFunctionDefinitionRef, ResolvedStructTypeRef, ResolvedTupleTypeRef,
};
use swamp_script_semantic::{IdentifierName, ResolvedType};

pub trait SwampExport {
    fn generate_swamp_definition() -> String;
    fn to_swamp_value(&self) -> Value;
    fn from_swamp_value(value: &Value) -> Result<Self, String>
    where
        Self: Sized;
}

impl SwampExport for Fp {
    // Todo: change to fixed point 32 bit
    fn to_swamp_value(&self) -> Value {
        Value::Float(*self)
    }

    fn from_swamp_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::Float(f) => Ok(*f),
            _ => Err("Expected Float value".to_string()),
        }
    }

    fn generate_swamp_definition() -> String {
        // Primitives don't need struct definitions
        String::new()
    }
}

impl SwampExport for i32 {
    fn to_swamp_value(&self) -> Value {
        Value::Int(*self)
    }

    fn from_swamp_value(value: &Value) -> Result<Self, String> {
        match value {
            Value::Int(i) => Ok(*i),
            _ => Err("Expected Int value".to_string()),
        }
    }

    fn generate_swamp_definition() -> String {
        String::new()
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Float(Fp),
    String(String),
    Bool(bool),
    Unit, // Means 'no value' ()
    Reference(Rc<RefCell<Value>>),

    // Containers
    Array(ResolvedArrayTypeRef, Vec<Value>),
    Tuple(ResolvedTupleTypeRef, Vec<Value>),
    Struct(ResolvedStructTypeRef, Vec<Value>, ResolvedType), // type of the struct, and the fields themselves in strict order

    EnumVariantSimple(ResolvedEnumVariantTypeRef),
    EnumVariantTuple(ResolvedEnumVariantTupleTypeRef, Vec<Value>),
    EnumVariantStruct(ResolvedEnumVariantStructTypeRef, Vec<Value>),

    // Number generators
    ExclusiveRange(Box<i32>, Box<i32>),

    // Higher order
    InternalFunction(ResolvedInternalFunctionDefinitionRef),
    ExternalFunction(EvalExternalFunctionRef),
}

impl TryFrom<ValueWithSignal> for Value {
    type Error = String;

    fn try_from(value: ValueWithSignal) -> Result<Self, Self::Error> {
        match value {
            ValueWithSignal::Value(v) => Ok(v),
            ValueWithSignal::Return(v) => Ok(v),
            ValueWithSignal::Break => Err("break can not be converted".to_string()),
            ValueWithSignal::Continue => Err("continue can not be converted".to_string()),
        }
    }
}

impl Value {
    pub fn as_bool(&self) -> Result<bool, String> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err("Expected boolean value".to_string()),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(n) => write!(f, "{}", n),
            Self::Float(n) => write!(f, "{}", n),
            Self::String(s) => write!(f, "{}", s),
            Self::Bool(b) => write!(f, "{}", b),
            Self::Array(_item_type, arr) => {
                write!(f, "[")?;
                for (i, val) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, "]")
            }
            Self::Tuple(_tuple_type, arr) => {
                write!(f, "(")?;
                for (i, val) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val)?;
                }
                write!(f, ")")
            }
            Self::Struct(struct_type_ref, fields_in_strict_order, display_type) => {
                let struct_name = display_type.display_name();
                write!(f, "{struct_name} {{ ")?;

                let fields = struct_type_ref
                    .borrow()
                    .fields
                    .keys()
                    .cloned()
                    .collect::<Vec<_>>();
                for (i, val) in fields_in_strict_order.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    let field_name = &fields[i];
                    write!(f, "{field_name}: {val}")?;
                }
                write!(f, " }}")
            }
            Self::InternalFunction(_reference) => write!(f, "<function>"), // TODO:
            Self::Unit => write!(f, "()"),
            Self::ExclusiveRange(start, end) => write!(f, "{start}..{end}"),

            Self::Reference(reference) => write!(f, "{}", reference.borrow()),
            Self::ExternalFunction(_) => write!(f, "<external>"), // TODO:

            // Enums ----
            Self::EnumVariantTuple(enum_name, fields_in_order) => {
                if enum_name.common.module_path.0.is_empty() {
                    write!(
                        f,
                        "{}::{}({})",
                        enum_name.common.enum_ref.name,
                        enum_name.common.variant_name.text,
                        comma(fields_in_order),
                    )
                } else {
                    write!(
                        f,
                        "{}::{}::{}{}",
                        enum_name.common.module_path,
                        enum_name.common.enum_ref.name,
                        enum_name.common.variant_name.text,
                        comma(fields_in_order),
                    )
                }
            }
            Self::EnumVariantStruct(struct_variant, values) => {
                let decorated_values: Vec<(IdentifierName, Value)> = struct_variant
                    .fields
                    .keys()
                    .cloned()
                    .zip(values.clone())
                    .collect();

                write!(
                    f,
                    "{}::{} {{ {} }}",
                    struct_variant.common.enum_ref.name,
                    struct_variant.common.variant_name,
                    comma_tuple(&decorated_values)
                )
            }
            Self::EnumVariantSimple(enum_type_ref) => write!(f, "{enum_type_ref}"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Reference(r1), Self::Reference(r2)) => {
                // Compare the actual values inside the references
                r1.borrow().eq(&*r2.borrow())
            }
            (Self::Reference(r1), other) => {
                // Compare reference value with direct value
                r1.borrow().eq(other)
            }
            (other, Self::Reference(r2)) => {
                // Compare direct value with reference value
                other.eq(&*r2.borrow())
            }
            // Regular value comparisons
            (Self::Int(a), Self::Int(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a == b,
            (Self::String(a), Self::String(b)) => a == b,
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::Unit, Self::Unit) => true,
            _ => false,
        }
    }
}

pub fn format_value(value: &Value, spec: &FormatSpecifier) -> Result<String, String> {
    match (value, spec) {
        (Value::Int(n), FormatSpecifier::Debug) => Ok(format!("{n:?}")),
        (Value::Int(n), FormatSpecifier::LowerHex) => Ok(format!("{n:x}")),
        (Value::Int(n), FormatSpecifier::UpperHex) => Ok(format!("{n:X}")),
        (Value::Int(n), FormatSpecifier::Binary) => Ok(format!("{n:b}")),

        (Value::Float(f), FormatSpecifier::Float) => Ok(format!("{f}")),
        (Value::Float(f), FormatSpecifier::Precision(prec, PrecisionType::Float)) => {
            Ok(format!("{:.*}", *prec as usize, f))
        }

        (Value::String(s), FormatSpecifier::Precision(prec, PrecisionType::String)) => {
            Ok(format!("{:.*}", *prec as usize, s))
        }

        // Debug format for complex types
        (Value::Struct(_type_id, fields, _), FormatSpecifier::Debug) => Ok(format!("{fields:?}")),
        (Value::Array(_type_id, elements), FormatSpecifier::Debug) => Ok(format!("{elements:?}")),
        (Value::EnumVariantTuple(_type_id, variant), FormatSpecifier::Debug) => {
            Ok(format!("{variant:?}"))
        }

        // Default string conversion for other cases
        (value, FormatSpecifier::Debug) => Ok(format!("{value}")),

        _ => Err(format!(
            "Unsupported format specifier {spec:?} for value type {value:?}"
        )),
    }
}
