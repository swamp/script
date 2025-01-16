/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::extra::{SparseValueId, SparseValueMap};
use core::any::Any;
use fixed32::Fp;
use seq_map::SeqMap;
use std::cell::Ref;
use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::rc::Rc;
use swamp_script_semantic::{
    ExternalFunctionId, ResolvedArrayTypeRef, ResolvedEnumVariantStructTypeRef,
    ResolvedEnumVariantTupleTypeRef, ResolvedEnumVariantTypeRef, ResolvedFormatSpecifierKind,
    ResolvedInternalFunctionDefinitionRef, ResolvedMapTypeRef, ResolvedPrecisionType,
    ResolvedRustTypeRef, ResolvedStructTypeRef, ResolvedTupleTypeRef, TypeNumber,
};
use swamp_script_semantic::{ResolvedNode, Span};

pub type ValueRef = Rc<RefCell<Value>>;

pub trait RustType: Any + Debug + Display + QuickSerialize {
    fn as_any(&self) -> &dyn Any;
    fn as_any_mut(&mut self) -> &mut dyn Any;
}

// Blanket implementation
impl<T: Any + Debug + Display + QuickSerialize> RustType for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self
    }
}

pub trait QuickSerialize {
    fn quick_serialize(&self, _octets: &mut [u8]) -> usize {
        0
    }
}

pub trait QuickDeserialize {
    fn quick_deserialize(octets: &[u8]) -> (Self, usize)
    where
        Self: Sized;
}

impl<'a, T: QuickSerialize + ?Sized> QuickSerialize for Ref<'a, T> {
    fn quick_serialize(&self, octets: &mut [u8]) -> usize {
        (**self).quick_serialize(octets)
    }
}

impl<T: QuickSerialize> QuickSerialize for Box<T> {
    fn quick_serialize(&self, octets: &mut [u8]) -> usize {
        // Delegate serialization to the inner T
        (**self).quick_serialize(octets)
    }
}

impl QuickSerialize for Rc<RefCell<dyn RustType>> {
    fn quick_serialize(&self, octets: &mut [u8]) -> usize {
        self.borrow().quick_serialize(octets)
    }
}

#[derive(Debug, Default)]
pub enum Value {
    Int(i32),
    Float(Fp),
    String(String),
    Bool(bool),
    #[default]
    Unit, // Means 'no value' ()

    Option(Option<ValueRef>),

    // Containers
    Array(ResolvedArrayTypeRef, Vec<ValueRef>),
    Map(ResolvedMapTypeRef, SeqMap<Value, ValueRef>), // Do not change to HashMap, the order is important for it to be deterministic
    Tuple(ResolvedTupleTypeRef, Vec<ValueRef>),
    Struct(ResolvedStructTypeRef, Vec<ValueRef>), // type of the struct, and the fields themselves in strict order

    EnumVariantSimple(ResolvedEnumVariantTypeRef),
    EnumVariantTuple(ResolvedEnumVariantTupleTypeRef, Vec<Value>),
    EnumVariantStruct(ResolvedEnumVariantStructTypeRef, Vec<Value>),

    // Number generators
    ExclusiveRange(Box<i32>, Box<i32>),
    InclusiveRange(Box<i32>, Box<i32>),

    // Higher order
    InternalFunction(ResolvedInternalFunctionDefinitionRef),
    ExternalFunction(ExternalFunctionId),

    // Other
    RustValue(ResolvedRustTypeRef, Rc<RefCell<Box<dyn RustType>>>),
}

#[allow(unused)]
fn quick_serialize_values(values: &[Value], buffer: &mut [u8], depth: usize) -> usize {
    let mut offset = 0;

    for value in values {
        let bytes_written = value.quick_serialize(&mut buffer[offset..], depth + 1);
        offset += bytes_written;
    }

    offset
}

impl Value {
    /// Serialize as quickly as possible
    /// Endian format is undefined. It is only used for serializing during a running application
    ///
    #[allow(clippy::too_many_lines)]
    #[inline]
    pub fn quick_serialize(&self, octets: &mut [u8], depth: usize) -> usize {
        match self {
            Self::Int(x) => {
                let value_octets = x.to_le_bytes();
                octets[..value_octets.len()].copy_from_slice(&value_octets);
                value_octets.len()
            }
            Self::Float(fp) => {
                let value_octets = fp.inner().to_le_bytes();
                octets[..value_octets.len()].copy_from_slice(&value_octets);
                value_octets.len()
            }
            Self::String(s) => {
                let len = s.len() as u16;
                let len_bytes = len.to_le_bytes();
                octets[..len_bytes.len()].copy_from_slice(&len_bytes);
                let mut offset = len_bytes.len();

                // Serialize the string bytes
                octets[offset..offset + len as usize].copy_from_slice(s.as_bytes());
                offset += len as usize;
                offset
            }

            Self::Bool(b) => {
                octets[0] = u8::from(*b);
                1
            }

            Self::Unit => 0,
            Self::Option(maybe_value) => match maybe_value {
                None => {
                    octets[0] = 0;
                    1
                }
                Some(inner_value) => {
                    octets[0] = 1;
                    let inner_size = inner_value
                        .borrow()
                        .quick_serialize(&mut octets[1..], depth + 1);
                    1 + inner_size
                }
            },
            Self::Array(_array_ref, values) => {
                let mut offset = 0;

                let count: u16 = values.len() as u16;
                let count_octets = count.to_le_bytes();
                octets[offset..offset + 2].copy_from_slice(&count_octets);
                offset += count_octets.len();

                for value in values {
                    let size = value
                        .borrow()
                        .quick_serialize(&mut octets[offset..], depth + 1);

                    offset += size;
                }
                offset
            }
            Self::Map(_map_ref, values) => {
                let mut offset = 0;

                let count: u16 = values.len() as u16;
                let count_octets = count.to_le_bytes();
                octets[offset..offset + count_octets.len()].copy_from_slice(&count_octets);
                offset += count_octets.len();

                for (key, value_ref) in values {
                    offset += key.quick_serialize(&mut octets[offset..], depth + 1);

                    let value_val = value_ref.borrow();
                    offset += value_val.quick_serialize(&mut octets[offset..], depth + 1);
                }

                offset
            }

            Self::Tuple(_tuple_type_ref, values) => {
                let mut offset = 0;
                for value in values {
                    let size = value
                        .borrow()
                        .quick_serialize(&mut octets[offset..], depth + 1);
                    offset += size;
                }
                offset
            }

            Self::Struct(_struct_type, values) => {
                let mut offset = 0;
                for value in values {
                    let size = value
                        .borrow()
                        .quick_serialize(&mut octets[offset..], depth + 1);
                    offset += size;
                }
                offset
            }

            Self::EnumVariantSimple(enum_variant) => {
                octets[0] = enum_variant.container_index;
                1
            }
            Self::EnumVariantTuple(enum_tuple_ref, values) => {
                let mut offset = 0;
                octets[offset] = enum_tuple_ref.common.container_index;
                offset += 1;
                for value in values {
                    let size = value.quick_serialize(&mut octets[offset..], depth + 1);
                    offset += size;
                }
                offset
            }
            Self::EnumVariantStruct(enum_struct_ref, values) => {
                let mut offset = 0;
                octets[offset] = enum_struct_ref.common.container_index;
                offset += 1;
                for value in values {
                    let size = value.quick_serialize(&mut octets[offset..], depth + 1);
                    offset += size;
                }
                offset
            }

            Self::ExclusiveRange(_, _) => {
                todo!("range is not supported yet")
            }
            Self::InclusiveRange(_, _) => {
                todo!("range is not supported yet")
            }

            Self::InternalFunction(_) => {
                todo!("internal_functions are not supported yet")
            }
            Self::ExternalFunction(_) => {
                todo!("external_functions are not supported yet")
            }

            Self::RustValue(_rust_value, rust_value) => rust_value.borrow().quick_serialize(octets),
        }
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        match self {
            Self::Int(i) => Self::Int(*i),
            Self::Float(f) => Self::Float(*f),
            Self::String(s) => Self::String(s.clone()),
            Self::Bool(b) => Self::Bool(*b),
            Self::Unit => Self::Unit,

            Self::Option(opt) => {
                let cloned_opt = opt.as_ref().map(std::clone::Clone::clone);
                Self::Option(cloned_opt)
            }

            // Containers
            Self::Array(resolved_ref, vec_refs) => {
                Self::Array(resolved_ref.clone(), deep_clone_valrefs(vec_refs))
            }

            Self::Map(resolved_ref, seq_map) => {
                let cloned_seq_map = seq_map
                    .iter()
                    .map(|(key, val_ref)| (key.clone(), deep_clone_valref(val_ref)))
                    .collect();

                Self::Map(resolved_ref.clone(), cloned_seq_map)
            }

            Self::Tuple(resolved_ref, vec_refs) => {
                Self::Tuple(resolved_ref.clone(), deep_clone_valrefs(vec_refs))
            }

            Self::Struct(resolved_ref, vec_refs) => {
                Self::Struct(resolved_ref.clone(), deep_clone_valrefs(vec_refs))
            }

            Self::EnumVariantSimple(resolved_ref) => Self::EnumVariantSimple(resolved_ref.clone()),

            Self::EnumVariantTuple(resolved_ref, vec_values) => {
                Self::EnumVariantTuple(resolved_ref.clone(), vec_values.clone())
            }

            Self::EnumVariantStruct(resolved_ref, vec_values) => {
                Self::EnumVariantStruct(resolved_ref.clone(), vec_values.clone())
            }

            Self::ExclusiveRange(start, end) => {
                Self::ExclusiveRange(Box::new(**start), Box::new(**end))
            }

            Self::InclusiveRange(start, end) => {
                Self::InclusiveRange(Box::new(**start), Box::new(**end))
            }

            Self::InternalFunction(resolved_def_ref) => {
                Self::InternalFunction(resolved_def_ref.clone())
            }

            Self::ExternalFunction(external_id) => Self::ExternalFunction(*external_id),

            Self::RustValue(resolved_rust_ref, rust_type_rc) => {
                Self::RustValue(resolved_rust_ref.clone(), rust_type_rc.clone())
            }
        }
    }
}

#[inline]
fn deep_clone_valrefs(vec_values: &[ValueRef]) -> Vec<ValueRef> {
    vec_values.iter().map(deep_clone_valref).collect()
}

#[inline]
fn deep_clone_valref(val_ref: &ValueRef) -> ValueRef {
    let cloned_value = val_ref.borrow().clone();
    Rc::new(RefCell::new(cloned_value))
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
pub const SPARSE_ID_TYPE_ID: TypeNumber = 998;

// Iterators

impl Value {
    /// # Errors
    ///
    /// # Panics
    ///
    #[allow(clippy::should_implement_trait)] // TODO: Fix this
    pub fn into_iter(self) -> Result<Box<dyn Iterator<Item = Self>>, ValueError> {
        match self {
            // TODO: Self::Reference(value_ref) => value_ref.borrow().clone().into_iter(is_mutable),
            Self::Array(_, values) => Ok(Box::new(
                values.into_iter().map(|item| item.borrow().clone()),
            )),
            Self::String(values) => Ok(Box::new(
                values
                    .chars()
                    .map(|item| Self::String(item.to_string()))
                    .collect::<Vec<Self>>()
                    .into_iter(),
            )),
            Self::Map(_, seq_map) => Ok(Box::new(
                seq_map.into_values().map(|item| item.borrow().clone()),
            )),
            Self::RustValue(ref rust_type_ref, _) => match rust_type_ref.number {
                SPARSE_TYPE_ID => {
                    let sparse_map = self
                        .downcast_rust::<SparseValueMap>()
                        .expect("must be sparsemap");
                    let values: Vec<_> = sparse_map
                        .borrow()
                        .values()
                        .iter()
                        .map(|item| item.borrow().clone())
                        .collect();
                    Ok(Box::new(values.into_iter()))
                }
                _ => Err(ValueError::NotSparseMap),
            },
            Self::ExclusiveRange(start_val, max_val) => {
                let start = *start_val;
                let end = *max_val;
                Ok(Box::new((start..end).map(Value::Int)))
            }
            Self::InclusiveRange(start_val, max_val) => {
                let start = *start_val;
                let end = *max_val;
                Ok(Box::new((start..=end).map(Value::Int)))
            }
            _ => Err(ValueError::CanNotCoerceToIterator),
        }
    }

    /// # Errors
    ///
    /// # Panics
    ///
    pub fn into_iter_pairs(self) -> Result<Box<dyn Iterator<Item = (Self, Self)>>, ValueError> {
        let values = match self {
            Self::Map(_, seq_map) => {
                Box::new(seq_map.into_iter().map(|(k, v)| (k, v.borrow().clone())))
            }
            Self::Tuple(_type_ref, elements) => {
                let iter = elements
                    .into_iter()
                    .enumerate()
                    .map(move |(i, v)| (Self::Int(i as i32), v.borrow().clone()));
                Box::new(iter) as Box<dyn Iterator<Item = (Self, Self)>>
            }
            Self::Array(_type_ref, array) => {
                let iter = array
                    .into_iter()
                    .enumerate()
                    .map(move |(i, v)| (Self::Int(i as i32), v.borrow().clone()));
                Box::new(iter) as Box<dyn Iterator<Item = (Self, Self)>>
            }
            Self::String(string) => {
                let iter = string
                    .chars()
                    .enumerate()
                    .map(|(i, v)| (Self::Int(i as i32), Self::String(v.to_string())))
                    .collect::<Vec<(Self, Self)>>()
                    .into_iter();
                Box::new(iter) as Box<dyn Iterator<Item = (Self, Self)> + 'static>
            }
            Self::RustValue(ref rust_type_ref, ref _rust_value) => {
                Box::new(match rust_type_ref.number {
                    SPARSE_TYPE_ID => {
                        let sparse_map = self
                            .downcast_rust::<SparseValueMap>()
                            .expect("must be sparsemap");

                        let id_type_ref = sparse_map.borrow().rust_type_ref_for_id.clone();

                        let pairs: Vec<_> = sparse_map
                            .borrow()
                            .iter()
                            .map(|(k, v)| {
                                (
                                    Self::RustValue(
                                        id_type_ref.clone(),
                                        Rc::new(RefCell::new(Box::new(SparseValueId(k)))),
                                    ),
                                    v.borrow().clone(),
                                )
                            })
                            .collect();

                        Box::new(pairs.into_iter()) as Box<dyn Iterator<Item = (Self, Self)>>
                    }

                    _ => return Err(ValueError::NotSparseMap),
                })
            }
            _ => return Err(ValueError::NotAnIterator),
        };

        Ok(values)
    }

    #[must_use]
    pub fn convert_to_string_if_needed(&self) -> String {
        match self {
            Self::String(string) => string.clone(),
            _ => self.to_string(),
        }
    }

    /// # Errors
    ///
    pub fn expect_string(&self) -> Result<String, ValueError> {
        match self {
            Self::String(s) => Ok(s.clone()),
            _ => Err(ValueError::ConversionError("Expected string value".into())),
        }
    }

    /// # Errors
    ///
    pub fn expect_int(&self) -> Result<i32, ValueError> {
        match self {
            Self::Int(v) => Ok(*v),
            _ => Err(ValueError::ConversionError("Expected int value".into())),
        }
    }

    /// # Errors
    ///
    pub fn expect_float(&self) -> Result<Fp, ValueError> {
        match self {
            Self::Float(v) => Ok(*v),
            _ => Err(ValueError::ConversionError("Expected float value".into())),
        }
    }

    /// # Errors
    ///
    pub fn as_bool(&self) -> Result<bool, ValueError> {
        match self {
            Self::Bool(b) => Ok(*b),
            _ => Err(ValueError::ConversionError("Expected bool value".into())),
        }
    }

    /// # Errors
    ///
    pub fn is_truthy(&self) -> Result<bool, ValueError> {
        let v = match self {
            Self::Bool(b) => *b,
            _ => return Err(ValueError::ConversionError("Expected bool value".into())),
        };

        Ok(v)
    }

    /// # Errors
    ///
    #[must_use]
    pub fn downcast_rust<T: RustType + 'static>(&self) -> Option<Rc<RefCell<Box<T>>>> {
        match self {
            Self::RustValue(_rust_type_ref, rc) => {
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

    #[must_use]
    pub fn downcast_hidden_rust<T: RustType + 'static>(&self) -> Option<Rc<RefCell<Box<T>>>> {
        match self {
            Self::Struct(_struct_ref, fields) => fields[0].borrow().downcast_rust(),
            _ => None,
        }
    }

    pub fn new_rust_value<T: RustType + 'static>(
        rust_type_ref: ResolvedRustTypeRef,
        value: T,
    ) -> Self {
        let boxed = Box::new(Box::new(value)) as Box<dyn RustType>;
        Self::RustValue(rust_type_ref, Rc::new(RefCell::new(boxed)))
    }

    pub fn new_hidden_rust_struct<T: RustType + 'static>(
        struct_type: ResolvedStructTypeRef,
        rust_description: ResolvedRustTypeRef,
        value: T,
    ) -> Self {
        let rust_value = Rc::new(RefCell::new(Self::new_rust_value(rust_description, value)));
        Value::Struct(struct_type, vec![rust_value])
    }
}

pub trait SourceMapLookup: Debug {
    fn get_text(&self, resolved_node: &ResolvedNode) -> &str;
    fn get_text_span(&self, span: &Span) -> &str;
}

impl Display for Value {
    #[allow(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Self::Int(n) => write!(f, "{n}"),
            Self::Float(n) => write!(f, "{n}"),
            Self::String(s) => write!(f, "\"{s}\""),
            Self::Bool(b) => write!(f, "{b}"),
            Self::Array(_item_type, arr) => {
                write!(f, "[")?;
                for (i, val) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val.borrow())?;
                }
                write!(f, "]")
            }
            Self::Map(_map_type_ref, items) => {
                write!(f, "[")?;
                for (i, (key, val)) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{key}: {}", val.borrow())?;
                }
                write!(f, "]")
            }

            Self::Tuple(_tuple_type, arr) => {
                write!(f, "(")?;
                for (i, val) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", val.borrow())?;
                }
                write!(f, ")")
            }
            Self::Struct(struct_type_ref, fields_in_strict_order) => {
                write!(f, "{} {{ ", struct_type_ref.borrow().assigned_name)?;

                let fields = struct_type_ref
                    .borrow()
                    .anon_struct_type
                    .defined_fields
                    .keys()
                    .cloned()
                    .collect::<Vec<_>>();
                for (i, val) in fields_in_strict_order.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    let field_name = &fields[i];
                    write!(f, "{field_name}: {}", val.borrow())?;
                }
                write!(f, " }}")
            }
            Self::InternalFunction(_reference) => write!(f, "<function>"), // TODO:
            Self::Unit => write!(f, "()"),
            Self::ExclusiveRange(start, end) => write!(f, "{start}..{end}"),
            Self::InclusiveRange(start, end) => write!(f, "{start}..={end}"),

            Self::ExternalFunction(_) => write!(f, "<external>"), // TODO:

            // Enums ----
            Self::EnumVariantTuple(enum_name, fields_in_order) => {
                if enum_name.common.module_path.0.is_empty() {
                    write!(
                        f,
                        "{:?}::{:?}",
                        enum_name.common.enum_ref.borrow().name,
                        enum_name.common.variant_name,
                    )?;
                } else {
                    write!(
                        f,
                        "{:?}::{:?}::{:?}",
                        enum_name.common.module_path,
                        enum_name.common.enum_ref.borrow().name,
                        enum_name.common.variant_name,
                    )?;
                }

                for field in fields_in_order {
                    write!(f, "{field}")?;
                }

                Ok(())
            }
            Self::EnumVariantStruct(struct_variant, values) => {
                let decorated_values: Vec<(String, Value)> = struct_variant
                    .anon_struct
                    .defined_fields
                    .keys()
                    .cloned()
                    .zip(values.clone())
                    .collect();

                write!(
                    f,
                    "{}::{} {{ ",
                    struct_variant.common.enum_ref.borrow().assigned_name,
                    &struct_variant.common.assigned_name
                )?;

                for (field_name, value) in &decorated_values {
                    write!(f, "{field_name}: {value}")?;
                }

                write!(f, " }}")?;
                Ok(())
            }
            Self::EnumVariantSimple(enum_variant_type_ref) => write!(
                f,
                "{}::{}",
                &enum_variant_type_ref.owner.borrow().assigned_name,
                &enum_variant_type_ref.assigned_name,
            ),
            Self::RustValue(_rust_type, rust_type_pointer) => {
                write!(f, "{}", rust_type_pointer.borrow())
            }
            Self::Option(maybe_val) => {
                let inner_str = if maybe_val.is_none() {
                    "none"
                } else {
                    &*maybe_val.as_ref().unwrap().borrow().to_string()
                };
                write!(f, "Option({inner_str})")
            } // TODO: Fix this. It is recursing now
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
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
            Self::Option(_wrapped) => {}
            Self::Array(_, _arr) => {}
            Self::Struct(type_ref, values) => {
                type_ref.borrow().name().span.hash(state);
                for v in values {
                    v.borrow().hash(state);
                }
            }
            Self::Map(_, _items) => {}
            Self::Tuple(_, _arr) => {}
            Self::EnumVariantSimple(_) => (),
            Self::EnumVariantTuple(_, fields) => fields.hash(state),
            Self::EnumVariantStruct(_, fields) => fields.hash(state),
            Self::ExclusiveRange(start, end) => {
                start.hash(state);
                end.hash(state);
            }
            Self::InclusiveRange(start, end) => {
                start.hash(state);
                end.hash(state);
            }
            Self::RustValue(_rust_type, _rust_val) => (),
            Self::InternalFunction(_) => (),
            Self::ExternalFunction(_) => (),
        }
    }
}

/// # Errors
///
pub fn format_value(value: &Value, spec: &ResolvedFormatSpecifierKind) -> Result<String, String> {
    match (value, spec) {
        (Value::Int(n), ResolvedFormatSpecifierKind::LowerHex) => Ok(format!("{n:x}")),
        (Value::Int(n), ResolvedFormatSpecifierKind::UpperHex) => Ok(format!("{n:X}")),
        (Value::Int(n), ResolvedFormatSpecifierKind::Binary) => Ok(format!("{n:b}")),

        (Value::Float(f), ResolvedFormatSpecifierKind::Float) => Ok(format!("{f}")),
        (
            Value::Float(f),
            ResolvedFormatSpecifierKind::Precision(prec, _node, ResolvedPrecisionType::Float),
        ) => Ok(format!("{:.*}", *prec as usize, f)),

        (
            Value::String(s),
            ResolvedFormatSpecifierKind::Precision(prec, _node, ResolvedPrecisionType::String, ..),
        ) => Ok(format!("{:.*}", *prec as usize, s)),

        _ => Err(format!(
            "Unsupported format specifier {spec:?} for value type {value:?}"
        )),
    }
}

#[must_use]
pub fn convert_vec_to_rc_refcell(vec: Vec<Value>) -> Vec<Rc<RefCell<Value>>> {
    vec.into_iter().map(|v| Rc::new(RefCell::new(v))).collect()
}
