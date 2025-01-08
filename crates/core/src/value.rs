/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::extra::{SparseValueId, SparseValueMap};
use core::any::Any;
use fixed32::Fp;
use seq_map::SeqMap;
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

#[derive(Debug, Clone, Default)]
pub enum Value {
    Int(i32),
    Float(Fp),
    String(String),
    Bool(bool),
    #[default]
    Unit, // Means 'no value' ()

    Option(Option<Box<Value>>),

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
    pub fn into_iter(self) -> Result<Box<dyn Iterator<Item = Value>>, ValueError> {
        match self {
            // TODO: Self::Reference(value_ref) => value_ref.borrow().clone().into_iter(is_mutable),
            Self::Array(_, values) => Ok(Box::new(
                values.into_iter().map(|item| item.borrow().clone()),
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
                        .into_iter()
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
            _ => Err(ValueError::CanNotCoerceToIterator),
        }
    }

    pub fn into_iter_pairs(self) -> Result<Box<dyn Iterator<Item = (Value, Value)>>, ValueError> {
        let values = match self {
            Self::Map(_, seq_map) => {
                Box::new(seq_map.into_iter().map(|(k, v)| (k, v.borrow().clone())))
            }
            Self::Tuple(_type_ref, elements) => {
                let iter = elements
                    .into_iter()
                    .enumerate()
                    .map(move |(i, v)| (Value::Int(i as i32), v.borrow().clone()));
                Box::new(iter) as Box<dyn Iterator<Item = (Value, Value)>>
            }
            Self::Array(_type_ref, array) => {
                let iter = array
                    .into_iter()
                    .enumerate()
                    .map(move |(i, v)| (Value::Int(i as i32), v.borrow().clone()));
                Box::new(iter) as Box<dyn Iterator<Item = (Value, Value)>>
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
                                    Value::RustValue(
                                        id_type_ref.clone(),
                                        Rc::new(RefCell::new(Box::new(SparseValueId(k)))),
                                    ),
                                    v.borrow().clone(),
                                )
                            })
                            .collect();

                        Box::new(pairs.into_iter()) as Box<dyn Iterator<Item = (Value, Value)>>
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
            _ => self.to_string(),
        }
    }
    pub fn expect_string(&self) -> Result<String, ValueError> {
        match self {
            Self::String(s) => Ok(s.clone()),
            _ => Err(ValueError::ConversionError("Expected string value".into())),
        }
    }

    pub fn expect_int(&self) -> Result<i32, ValueError> {
        match self {
            Self::Int(v) => Ok(*v),
            _ => Err(ValueError::ConversionError("Expected int value".into())),
        }
    }

    pub fn expect_float(&self) -> Result<Fp, ValueError> {
        match self {
            Self::Float(v) => Ok(*v),
            _ => Err(ValueError::ConversionError("Expected float value".into())),
        }
    }

    pub fn as_bool(&self) -> Result<bool, ValueError> {
        match self {
            Self::Bool(b) => Ok(*b),
            _ => Err(ValueError::ConversionError("Expected bool value".into())),
        }
    }

    pub fn is_truthy(&self) -> Result<bool, ValueError> {
        let v = match self {
            Self::Bool(b) => *b,
            _ => return Err(ValueError::ConversionError("Expected bool value".into())),
        };

        Ok(v)
    }

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
            Value::Struct(_struct_ref, fields) => fields[0].borrow().downcast_rust(),
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
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
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
                    write!(f, "{}", val.borrow())?;
                }
                write!(f, "]")
            }
            Value::Map(_map_type_ref, items) => {
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

            Self::ExternalFunction(_) => write!(f, "<external>"), // TODO:

            // Enums ----
            Self::EnumVariantTuple(enum_name, fields_in_order) => {
                if enum_name.common.module_path.0.is_empty() {
                    write!(
                        f,
                        "{:?}::{:?}",
                        enum_name.common.enum_ref.name, enum_name.common.variant_name,
                    )?;
                } else {
                    write!(
                        f,
                        "{:?}::{:?}::{:?}",
                        enum_name.common.module_path,
                        enum_name.common.enum_ref.name,
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
                    struct_variant.common.enum_ref.assigned_name,
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
                &enum_variant_type_ref.owner.assigned_name, &enum_variant_type_ref.assigned_name,
            ),
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
            Self::Option(o) => o.hash(state),
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
            Self::RustValue(_rust_type, _rust_val) => (),
            Self::InternalFunction(_) => (),
            Self::ExternalFunction(_) => (),
        }
    }
}

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
