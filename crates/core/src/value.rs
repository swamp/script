/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::extra::{SparseValueId, SparseValueMap};
use core::any::Any;
use fixed32::Fp;
use seq_fmt::{comma, comma_tuple};
use seq_map::SeqMap;
use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::rc::Rc;
use swamp_script_semantic::ns::{ResolvedModuleNamespace, SemanticError};
use swamp_script_semantic::{
    ExternalFunctionId, FormatSpecifier, PrecisionType, ResolvedArrayTypeRef,
    ResolvedEnumVariantStructTypeRef, ResolvedEnumVariantTupleTypeRef, ResolvedEnumVariantTypeRef,
    ResolvedInternalFunctionDefinitionRef, ResolvedMapTypeRef, ResolvedRustTypeRef,
    ResolvedStructTypeRef, ResolvedTupleTypeRef, TypeNumber,
};
use swamp_script_semantic::{IdentifierName, ResolvedType};

pub trait RustType: Any + Debug + Display {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

// Blanket implementation
impl<T: Any + Debug + Display> RustType for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
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
    Option(Option<Box<Value>>),

    // Containers
    Array(ResolvedArrayTypeRef, Vec<Value>),
    Map(ResolvedMapTypeRef, SeqMap<Value, Value>), // Do not change to HashMap, the order is important for it to be deterministic
    Tuple(ResolvedTupleTypeRef, Vec<Value>),
    Struct(ResolvedStructTypeRef, Vec<Value>, ResolvedType), // type of the struct, and the fields themselves in strict order

    EnumVariantSimple(ResolvedEnumVariantTypeRef),
    EnumVariantTuple(ResolvedEnumVariantTupleTypeRef, Vec<Value>),
    EnumVariantStruct(ResolvedEnumVariantStructTypeRef, Vec<Value>),

    // Number generators
    ExclusiveRange(Box<i32>, Box<i32>),

    // Higher order
    InternalFunction(ResolvedInternalFunctionDefinitionRef),
    ExternalFunction(ExternalFunctionId),

    // Other
    RustValue(ResolvedRustTypeRef, Rc<RefCell<Box<dyn RustType>>>),
}

pub fn to_rust_value<T: RustType + 'static>(type_ref: ResolvedRustTypeRef, value: T) -> Value {
    Value::RustValue(
        type_ref,
        Rc::new(RefCell::new(Box::new(value) as Box<dyn RustType>)),
    )
}

#[derive(Debug, PartialEq, Eq)]
pub enum ValueError {
    NotAnIterator,
    NotSparseMap,
    CanNotCoerceToIterator,
    ConversionError(String),
    WrongNumberOfArguments { expected: usize, got: usize },
    TypeError(String),
}

pub const SPARSE_TYPE_ID: TypeNumber = 999;

// Iterators

impl Value {
    pub fn into_iter(
        self,
        is_mutable: bool,
    ) -> Result<Box<dyn Iterator<Item = Value>>, ValueError> {
        match self {
            Self::Reference(value_ref) => value_ref.borrow().clone().into_iter(is_mutable),
            Self::Array(_, values) => Ok(Box::new(values.into_iter())),
            Self::Map(_, seq_map) => Ok(Box::new(seq_map.into_values())),
            Self::RustValue(ref rust_type_ref, _) => match rust_type_ref.number {
                SPARSE_TYPE_ID => {
                    let sparse_map = self
                        .downcast_rust::<SparseValueMap>()
                        .expect("must be sparsemap");
                    let values: Vec<_> = sparse_map.borrow().values().into_iter().collect();
                    Ok(Box::new(values.into_iter()))
                }
                _ => Err(ValueError::NotSparseMap),
            },
            Self::ExclusiveRange(start_val, max_val) => {
                let start = *start_val;
                let end = *max_val;
                Ok(Box::new((start..end).map(Value::Int)))
            }
            _ => Err(ValueError::CanNotCoerceToIterator),
        }
    }

    pub fn into_iter_pairs(
        self,
        is_mutable: bool,
    ) -> Result<Box<dyn Iterator<Item = (Value, Value)>>, ValueError> {
        let values = match self {
            Self::Reference(value_ref) => value_ref
                .borrow_mut()
                .to_owned()
                .into_iter_pairs(is_mutable)?,
            Self::Map(_, seq_map) => Box::new(seq_map.into_iter()),
            Self::Tuple(_type_ref, elements) => {
                let iter = elements
                    .into_iter()
                    .enumerate()
                    .map(move |(i, v)| (Value::Int(i as i32), v));
                Box::new(iter)
            }
            Self::Array(_type_ref, array) => {
                let iter = array
                    .into_iter()
                    .enumerate()
                    .map(move |(i, v)| (Value::Int(i as i32), v));
                Box::new(iter)
            }
            Self::RustValue(ref rust_type_ref, ref _rust_value) => {
                Box::new(match rust_type_ref.number {
                    SPARSE_TYPE_ID => {
                        let sparse_map = self
                            .downcast_rust::<SparseValueMap>()
                            .expect("must be sparsemap");

                        let id_type_ref = sparse_map.borrow().rust_type_ref_for_id.clone();

                        if is_mutable {
                            let pairs: Vec<_> = sparse_map
                                .borrow_mut()
                                .iter_mut()
                                .map(|(k, v)| {
                                    (
                                        Value::RustValue(
                                            id_type_ref.clone(),
                                            Rc::new(RefCell::new(Box::new(SparseValueId(k)))),
                                        ),
                                        v.clone(),
                                    )
                                })
                                .collect();

                            Box::new(pairs.into_iter())
                        } else {
                            let pairs: Vec<_> = sparse_map
                                .borrow()
                                .iter()
                                .map(|(k, v)| {
                                    (
                                        Value::RustValue(
                                            id_type_ref.clone(),
                                            Rc::new(RefCell::new(Box::new(SparseValueId(k)))),
                                        ),
                                        v.clone(),
                                    )
                                })
                                .collect();

                            Box::new(pairs.into_iter())
                        }
                    }
                    _ => return Err(ValueError::NotSparseMap),
                })
            }
            _ => return Err(ValueError::NotAnIterator),
        };

        Ok(values)
    }

    pub fn convert_to_string_if_needed(&self) -> String {
        match self {
            Self::String(string) => string.clone(),
            Self::Reference(value) => value.borrow().convert_to_string_if_needed(),
            _ => self.to_string(),
        }
    }
    pub fn expect_string(&self) -> Result<String, ValueError> {
        match self.unref() {
            Value::String(s) => Ok(s),
            _ => Err(ValueError::ConversionError("Expected string value".into())),
        }
    }

    pub fn expect_int(&self) -> Result<i32, ValueError> {
        match self.unref() {
            Value::Int(v) => Ok(v),
            _ => Err(ValueError::ConversionError("Expected int value".into())),
        }
    }

    pub fn as_bool(&self) -> Result<bool, String> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err("Expected boolean value".to_string()),
        }
    }

    pub fn is_truthy(&self) -> Result<bool, String> {
        let v = match self {
            Value::Bool(b) => *b,
            _ => return Err("Expected boolean value".to_string()),
        };

        Ok(v)
    }

    pub fn downcast_rust<T: RustType + 'static>(&self) -> Option<Rc<RefCell<Box<T>>>> {
        match self {
            Value::RustValue(_rust_type_ref, rc) => {
                let type_matches = {
                    let guard = rc.borrow();
                    (**guard).as_any().is::<T>()
                };

                if type_matches {
                    Some(unsafe { std::mem::transmute(rc.clone()) })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn downcast_rust_mut_or_not<T: RustType + 'static>(&self) -> Option<Rc<RefCell<Box<T>>>> {
        if let Some(found) = self.downcast_rust_mut() {
            Some(found)
        } else {
            self.downcast_rust()
        }
    }

    pub fn downcast_rust_mut<T: RustType + 'static>(&self) -> Option<Rc<RefCell<Box<T>>>> {
        match self {
            Value::Reference(r) => match &*r.borrow() {
                Value::RustValue(_rust_type_ref, rc) => {
                    let type_matches = {
                        let guard = rc.borrow();
                        (**guard).as_any().is::<T>()
                    };

                    if type_matches {
                        Some(unsafe { std::mem::transmute(rc.clone()) })
                    } else {
                        None
                    }
                }
                _ => None,
            },

            _ => None,
        }
    }

    pub fn downcast_hidden_rust<T: RustType + 'static>(&self) -> Option<Rc<RefCell<Box<T>>>> {
        match self {
            Value::Struct(_struct_ref, fields, _) => fields[0].downcast_rust(),
            _ => None,
        }
    }

    pub fn new_rust_value<T: RustType + 'static>(
        rust_type_ref: ResolvedRustTypeRef,
        value: T,
    ) -> Self {
        let boxed = Box::new(Box::new(value)) as Box<dyn RustType>;
        Value::RustValue(rust_type_ref, Rc::new(RefCell::new(boxed)))
    }

    pub fn new_hidden_rust_struct<T: RustType + 'static>(
        struct_type: ResolvedStructTypeRef,
        rust_description: ResolvedRustTypeRef,
        value: T,
        resolved_type: ResolvedType,
    ) -> Self {
        let rust_value = Self::new_rust_value(rust_description, value);
        Value::Struct(struct_type, vec![rust_value], resolved_type)
    }

    pub fn new_hidden_rust_type<T: RustType + 'static>(
        name: &str,
        rust_description: ResolvedRustTypeRef,
        value: T,
        namespace: &mut ResolvedModuleNamespace,
    ) -> Result<(Self, ResolvedStructTypeRef), SemanticError> {
        let struct_type =
            namespace.util_insert_struct_type(name, &[("hidden", ResolvedType::Any)])?;
        let struct_value = Self::new_hidden_rust_struct(
            struct_type.clone(),
            rust_description,
            value,
            ResolvedType::Struct(struct_type.clone()),
        );
        Ok((struct_value, struct_type))
    }

    #[must_use]
    pub fn unref(&self) -> Self {
        match self {
            Value::Reference(ref inner) => inner.borrow().clone(),
            _ => self.clone(),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(n) => write!(f, "{}", n),
            Self::Float(n) => write!(f, "{}", n),
            Self::String(s) => write!(f, "\"{}\"", s),
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
            Value::Map(_map_type_ref, items) => {
                write!(f, "[")?;
                for (i, (key, val)) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key}: {val}")?;
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
            Self::RustValue(_rust_type, rust_type_pointer) => {
                write!(f, "{}", rust_type_pointer.borrow())
            }
            Self::Option(maybe_val) => write!(f, "{maybe_val:?}"),
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
            (Self::Option(r1), Self::Option(r2)) => r1 == r2,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Self::Int(n) => n.hash(state),
            Self::Float(f) => f.hash(state),
            Self::String(s) => s.hash(state),
            Self::Bool(b) => b.hash(state),
            Self::Unit => (),
            Self::Reference(r) => r.borrow().hash(state),
            Self::Option(o) => o.hash(state),
            Self::Array(_, arr) => arr.hash(state),
            Self::Struct(type_ref, values, _resolved_type) => {
                type_ref.borrow().name.hash(state);
                for v in values {
                    v.hash(state);
                }
            }
            Self::Map(_, items) => items.hash(state),
            Self::Tuple(_, arr) => arr.hash(state),
            Self::EnumVariantSimple(_) => (),
            Self::EnumVariantTuple(_, fields) => fields.hash(state),
            Self::EnumVariantStruct(_, fields) => fields.hash(state),
            Self::ExclusiveRange(start, end) => {
                start.hash(state);
                end.hash(state);
            }
            Self::RustValue(_rust_type, _rust_val) => (),
            Self::InternalFunction(_) => (),
            Self::ExternalFunction(_) => (),
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
