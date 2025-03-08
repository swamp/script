/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

pub mod prelude;

use fmt::{Debug, Display};
use seq_fmt::comma;
use seq_map::SeqMap;
use std::cell::RefCell;
use std::cmp::PartialEq;
use std::fmt;
use std::hash::Hash;
use std::rc::Rc;
use swamp_script_node::Node;

#[derive(Eq, Clone, PartialEq)]
pub enum Type {
    // Primitives
    Int,
    Float,
    String,
    Bool,

    Slice(Box<Type>),
    SlicePair(Box<Type>, Box<Type>),

    Unit,  // Empty or nothing
    Never, // Not even empty since control flow has escaped with break or return.

    // Containers
    Tuple(Vec<Type>),
    NamedStruct(NamedStructTypeRef),
    AnonymousStruct(AnonymousStructType),

    Enum(EnumTypeRef),

    Function(Signature),
    Iterable(Box<Type>),

    Optional(Box<Type>),

    Generic(ParameterizedTypeBlueprint, Vec<Type>),
    Blueprint(ParameterizedTypeBlueprint),
    Variable(String),

    External(ExternalTypeRef),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParameterizedTypeKind {
    Struct(NamedStructType),
    Enum(EnumTypeRef),
}

impl ParameterizedTypeKind {
    pub(crate) fn same_type(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Struct(a), Self::Struct(b)) => a.type_id == b.type_id,
            (Self::Enum(a), Self::Enum(b)) => a.borrow().type_id == b.borrow().type_id,
            _ => false,
        }
    }

    pub fn name(&self) -> String {
        match self {
            Self::Struct(struct_type_ref) => struct_type_ref.assigned_name.clone(),
            Self::Enum(enum_type_ref) => enum_type_ref.borrow().assigned_name.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ParameterizedTypeBlueprint {
    pub kind: ParameterizedTypeKind,
    pub type_variables: Vec<String>,
    pub type_id: TypeNumber,
    pub defined_in_module_path: Vec<String>,
}

impl ParameterizedTypeBlueprint {
    pub fn name(&self) -> String {
        self.kind.name()
    }
}

pub type NamedStructTypeRef = Rc<RefCell<NamedStructType>>;

#[derive(Clone, Eq, PartialEq)]
pub struct ParameterNode {
    pub name: Node,
    pub is_mutable: Option<Node>,
}

impl Debug for ParameterNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parameter")
    }
}

impl ParameterNode {
    #[inline]
    #[must_use]
    pub const fn is_mutable(&self) -> bool {
        self.is_mutable.is_some()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ExternalType {
    pub type_name: String, // To identify the specific Rust type
    pub number: u32,       // For type comparison
}

pub type ExternalTypeRef = Rc<ExternalType>;

#[derive(Debug, Clone)]
pub struct TypeForParameter {
    pub name: String,
    pub resolved_type: Type,
    pub is_mutable: bool,
    pub node: Option<ParameterNode>,
}

impl Display for TypeForParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}{}: {:?}",
            if self.is_mutable { "mut " } else { "" },
            self.name,
            self.resolved_type
        )
    }
}

impl Eq for TypeForParameter {}

impl PartialEq for TypeForParameter {
    fn eq(&self, other: &Self) -> bool {
        let types_equal = self.resolved_type.compatible_with(&other.resolved_type);

        types_equal && (self.is_mutable == other.is_mutable)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Signature {
    pub parameters: Vec<TypeForParameter>,
    pub return_type: Box<Type>,
}

impl Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "({})->{}", comma(&self.parameters), self.return_type)
    }
}

impl Signature {
    #[must_use]
    pub fn same_type(&self, other: &Self) -> bool {
        if self.parameters.len() != other.parameters.len()
            || !self.return_type.compatible_with(&other.return_type)
        {
            return false;
        }

        for (param, other_param) in self.parameters.iter().zip(other.parameters.clone()) {
            if !&param
                .resolved_type
                .compatible_with(&other_param.resolved_type)
            {
                return false;
            }

            if param.is_mutable != other_param.is_mutable {
                return false;
            }
        }

        true
    }
}

impl Type {
    #[must_use]
    pub const fn is_concrete(&self) -> bool {
        !matches!(self, Self::Unit | Self::Never | Self::Variable(_))
    }

    /// # Panics
    ///
    #[must_use]
    pub fn id(&self) -> Option<TypeNumber> {
        let found_id = match self {
            Self::Unit => 0,
            Self::Int => 1,
            Self::Bool => 2,
            Self::Float => 3,
            Self::String => 4,
            Self::External(external) => external.number,
            Self::NamedStruct(struct_ref) => struct_ref.borrow().type_id,
            Self::Enum(enum_type) => enum_type.borrow().type_id,
            Self::Blueprint(blueprint) => blueprint.type_id,
            _ => return None,
        };
        Some(found_id)
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::String => write!(f, "String"),
            Self::Bool => write!(f, "Bool"),
            Self::Unit => write!(f, "Unit"),
            Self::Never => write!(f, "!"),
            Self::Slice(ty) => write!(f, "Slice<{ty:?}>"),
            Self::SlicePair(key, value) => write!(f, "Slice<{key:?}, {value:?}>"),
            Self::Tuple(tuple_type_ref) => write!(f, "( {tuple_type_ref:?} )"),
            Self::NamedStruct(struct_type_ref) => {
                write!(f, "{}", struct_type_ref.borrow().assigned_name)
            }
            Self::AnonymousStruct(anonymous_struct_type) => {
                write!(f, "{anonymous_struct_type:?}")
            }
            Self::Enum(enum_type_ref) => write!(f, "{:?}", enum_type_ref.borrow().assigned_name),
            Self::Function(function_type_signature) => {
                write!(f, "{function_type_signature:?}")
            }
            Self::Iterable(type_generated) => write!(f, "Iterable<{type_generated:?}>"),
            Self::Optional(base_type) => write!(f, "{base_type:?}?"),
            Self::External(rust_type) => write!(f, "{:?}?", rust_type.type_name),
            Self::Variable(variable_name) => write!(f, "<|{variable_name}|>"),
            Self::Generic(blueprint, non_concrete_arguments) => {
                write!(f, "{blueprint:?}<{non_concrete_arguments:?}>")
            }
            Self::Blueprint(blueprint) => {
                write!(f, "{blueprint:?}")
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::String => write!(f, "String"),
            Self::Bool => write!(f, "Bool"),
            Self::Unit => write!(f, "Unit"),
            Self::Never => write!(f, "!"),
            Self::Slice(ty) => write!(f, "Slice<{ty}>"),
            Self::SlicePair(key, value) => write!(f, "Slice<{key}, {value}>"),
            Self::Tuple(tuple) => write!(f, "({})", comma(tuple)),
            Self::NamedStruct(struct_ref) => write!(f, "{}", struct_ref.borrow().assigned_name),
            Self::AnonymousStruct(struct_ref) => write!(f, "{struct_ref:?}"),
            Self::Enum(enum_type) => write!(f, "{}", enum_type.borrow().assigned_name),
            Self::Function(signature) => write!(f, "function {signature}"),
            Self::Iterable(generating_type) => write!(f, "Iterable<{generating_type}>"),
            Self::Optional(base_type) => write!(f, "{base_type}?"),
            Self::External(rust_type) => write!(f, "RustType {}", rust_type.type_name),
            Self::Variable(variable_name) => write!(f, "<|{variable_name}|>"),
            Self::Generic(blueprint, non_concrete_arguments) => {
                write!(f, "{blueprint:?}<{non_concrete_arguments:?}>")
            }
            Self::Blueprint(blueprint) => {
                write!(f, "{blueprint:?}")
            }
        }
    }
}

impl Type {
    #[must_use]
    pub fn assignable_type(&self, other: &Self) -> bool {
        if self.compatible_with(other) {
            true
        } else if let Self::Optional(inner_type) = self {
            inner_type.compatible_with(other)
        } else {
            false
        }
    }

    #[must_use]
    pub fn compatible_with(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Function(a), Self::Function(b)) => a.same_type(b),

            (_, Self::Never)
            | (Self::Never, _)
            | (Self::Int, Self::Int)
            | (Self::Float, Self::Float)
            | (Self::String, Self::String)
            | (Self::Bool, Self::Bool)
            | (Self::Unit, Self::Unit) => true,

            (Self::Enum(a), Self::Enum(b)) => a == b,

            (Self::Slice(a), Self::Slice(b)) | (Self::Iterable(a), Self::Iterable(b)) => {
                a.compatible_with(b)
            }

            (Self::SlicePair(a1, a2), Self::SlicePair(b1, b2)) => {
                a1.compatible_with(b1) && a2.compatible_with(b2)
            }

            (Self::NamedStruct(a), Self::NamedStruct(b)) => compare_struct_types(a, b),

            (Self::AnonymousStruct(a), Self::AnonymousStruct(b)) => {
                compare_anonymous_struct_types(a, b)
            }

            (Self::Tuple(a), Self::Tuple(b)) => {
                if a.len() != b.len() {
                    return false;
                }
                a.iter().zip(b.iter()).all(|(a, b)| a.compatible_with(b))
            }

            (Self::Optional(inner_type_a), Self::Optional(inner_type_b)) => {
                inner_type_a.compatible_with(inner_type_b)
            }

            (Self::Generic(blueprint_a, args_a), Self::Generic(blueprint_b, args_b)) => {
                blueprint_a == blueprint_b && (args_a == args_b)
            }

            (Self::Variable(a), Self::Variable(b)) => a == b,

            (Self::External(type_ref_a), Self::External(type_ref_b)) => {
                type_ref_a.number == type_ref_b.number
            }

            _ => false,
        }
    }
}

fn compare_struct_types(a: &NamedStructTypeRef, b: &NamedStructTypeRef) -> bool {
    let a_borrow = a.borrow();
    let b_borrow = b.borrow();

    if a_borrow.type_id != b.borrow().type_id {
        return false;
    }

    if a_borrow.assigned_name != b_borrow.assigned_name {
        return false;
    }

    compare_anonymous_struct_types(&a_borrow.anon_struct_type, &b_borrow.anon_struct_type)
}

#[must_use]
pub fn same_anon_struct_ref(a: &AnonymousStructType, b: &AnonymousStructType) -> bool {
    compare_anonymous_struct_types(a, b)
}

pub fn same_named_struct_ref(a: &NamedStructTypeRef, b: &NamedStructTypeRef) -> bool {
    if a.borrow().assigned_name != b.borrow().assigned_name {
        return false;
    }

    compare_anonymous_struct_types(&a.borrow().anon_struct_type, &b.borrow().anon_struct_type)
}

#[must_use]
pub fn compare_anonymous_struct_types(a: &AnonymousStructType, b: &AnonymousStructType) -> bool {
    if a.field_name_sorted_fields.len() != b.field_name_sorted_fields.len() {
        return false;
    }

    for ((a_name, a_type), (b_name, b_type)) in a
        .field_name_sorted_fields
        .iter()
        .zip(b.field_name_sorted_fields.clone())
    {
        if *a_name != b_name {
            return false;
        }

        if !a_type.field_type.compatible_with(&b_type.field_type) {
            return false;
        }
    }

    true
}

#[must_use]
pub fn check_assignable_anonymous_struct_types(
    a: &AnonymousStructType,
    b: &AnonymousStructType,
) -> bool {
    if a.field_name_sorted_fields.len() != b.field_name_sorted_fields.len() {
        return false;
    }

    for (name, field) in &a.field_name_sorted_fields {
        if let Some(found_field) = b.field_name_sorted_fields.get(name) {
            if !found_field.field_type.compatible_with(&field.field_type) {
                return false;
            }
        } else {
            return false;
        }
    }

    true
}

#[must_use]
pub fn comma_seq<K: Clone + Hash + Eq + Display, V: Display>(values: &SeqMap<K, V>) -> String {
    let mut result = String::new();
    for (i, (key, value)) in values.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        result.push_str(format!("{key}: {value}").as_str());
    }
    result
}

#[must_use]
pub fn comma_seq_nl<K: Clone + Hash + Eq + Display, V: Display>(
    values: &SeqMap<K, V>,
    prefix: &str,
) -> String {
    let mut result = String::new();
    for (key, value) in values.iter() {
        result.push_str(format!("{prefix}{key}: {value}\n").as_str());
    }
    result
}

#[derive(Clone, Eq, PartialEq)]
pub struct StructTypeField {
    pub identifier: Option<Node>,
    pub field_type: Type,
}

impl Display for StructTypeField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{:?}:{}", self.identifier, self.field_type)
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct AnonymousStructType {
    //pub source_ordered_fields: SeqMap<String, StructTypeField>,
    pub field_name_sorted_fields: SeqMap<String, StructTypeField>,
}

impl Debug for AnonymousStructType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", comma_seq(&self.field_name_sorted_fields))
    }
}

#[must_use]
pub fn sort_struct_fields2(
    unordered_seq_map: &SeqMap<String, StructTypeField>,
) -> SeqMap<String, StructTypeField> {
    let mut sorted_pairs: Vec<(&String, &StructTypeField)> = unordered_seq_map.iter().collect();
    sorted_pairs.sort_by(|a, b| a.0.cmp(b.0));

    sorted_pairs
        .into_iter()
        .map(|(name, field)| (name.clone(), field.clone()))
        .collect()
}

impl AnonymousStructType {
    #[must_use]
    pub fn new_and_sort_fields(source_ordered_fields: &SeqMap<String, StructTypeField>) -> Self {
        Self {
            field_name_sorted_fields: sort_struct_fields2(source_ordered_fields),
        }
    }

    #[must_use]
    pub const fn new(defined_order: SeqMap<String, StructTypeField>) -> Self {
        Self {
            field_name_sorted_fields: defined_order,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EnumVariantStructType {
    pub common: EnumVariantCommon,
    pub anon_struct: AnonymousStructType,
}

pub type EnumVariantTupleTypeRef = Rc<EnumVariantTupleType>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EnumVariantTupleType {
    pub common: EnumVariantCommon,
    pub fields_in_order: Vec<Type>,
}

pub type EnumTypeRef = Rc<RefCell<EnumType>>;

#[derive(Clone, Eq, PartialEq)]
pub struct EnumType {
    pub name: Node,
    pub assigned_name: String,
    pub module_path: Vec<String>,
    pub type_id: TypeNumber,

    pub variants: SeqMap<String, EnumVariantTypeRef>,
}

impl Debug for EnumType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}", self.assigned_name)?;
        let s = comma(
            &self
                .variants
                .iter()
                .map(|(name, _variant)| name)
                .collect::<Vec<&String>>(),
        );
        write!(f, "{{ {s} }}")
    }
}

impl EnumType {
    #[must_use]
    pub fn new(
        name: Node,
        assigned_name: &str,
        module_path: Vec<String>,
        number: TypeNumber,
    ) -> Self {
        Self {
            name,
            assigned_name: assigned_name.to_string(),
            module_path,
            type_id: number,
            variants: SeqMap::new(),
        }
    }

    #[must_use]
    pub const fn name(&self) -> &Node {
        &self.name
    }

    #[must_use]
    pub fn get_variant(&self, name: &str) -> Option<&EnumVariantTypeRef> {
        self.variants.get(&name.to_string())
    }

    #[must_use]
    pub fn get_variant_from_index(&self, index: usize) -> Option<&EnumVariantTypeRef> {
        Some(self.variants.values().collect::<Vec<_>>()[index])
    }
}

pub type EnumVariantTypeRef = Rc<EnumVariantType>;

#[derive(Clone, Eq, PartialEq)]
pub struct EnumVariantCommon {
    pub name: Node,
    pub assigned_name: String,
    pub number: TypeNumber,
    pub container_index: u8,
    pub owner: EnumTypeRef,
}

impl Debug for EnumVariantCommon {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "<{}>{}::{}",
            self.number,
            self.owner.borrow().assigned_name,
            self.assigned_name
        )
    }
}

pub type EnumVariantStructFieldTypeRef = Rc<EnumVariantStructFieldType>;

#[derive(Debug)]
pub struct EnumVariantStructFieldType {
    pub name: Node,
    pub enum_variant: EnumVariantTypeRef,
    pub resolved_type: Type,

    pub field_index: usize,
}

pub type EnumVariantTupleFieldTypeRef = Rc<EnumVariantTupleFieldType>;

#[derive(Debug, Eq, PartialEq)]
pub struct EnumVariantTupleFieldType {
    pub name: Node,
    pub enum_variant: EnumVariantTypeRef,
    pub resolved_type: Type,

    pub field_index: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EnumVariantSimpleType {
    pub common: EnumVariantCommon,
}

pub type EnumVariantStructTypeRef = Rc<EnumVariantStructType>;

pub type EnumVariantSimpleTypeRef = Rc<EnumVariantSimpleType>;

#[derive(Clone, Eq, PartialEq)]
pub enum EnumVariantType {
    Struct(EnumVariantStructTypeRef),
    Tuple(EnumVariantTupleTypeRef),
    Nothing(EnumVariantSimpleTypeRef),
}
impl EnumVariantType {
    #[must_use]
    pub fn common(&self) -> &EnumVariantCommon {
        match self {
            Self::Tuple(tuple) => &tuple.common,
            Self::Struct(c) => &c.common,
            Self::Nothing(c) => &c.common,
        }
    }
}

impl Debug for EnumVariantType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Struct(x) => write!(f, "{{ {x:?} }}"),
            Self::Tuple(x) => write!(f, "({x:?})"),
            Self::Nothing(_x) => Ok(()),
        }
    }
}

#[derive(Debug)]
pub struct AliasType {
    pub name: Node,
    pub assigned_name: String,
    pub referenced_type: Type,
}
pub type AliasTypeRef = Rc<AliasType>;

#[derive(Clone, PartialEq, Eq)]
pub struct NamedStructType {
    pub name: Node,
    pub assigned_name: String,
    pub anon_struct_type: AnonymousStructType,
    pub type_id: TypeNumber,
}

impl Debug for NamedStructType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "struct {} anon: {:?}",
            self.assigned_name, self.anon_struct_type
        )
    }
}

pub type TypeNumber = u32;

impl NamedStructType {
    #[must_use]
    pub fn new(
        name: Node,
        assigned_name: &str,
        anon_struct_type: AnonymousStructType,
        type_id: TypeNumber,
    ) -> Self {
        Self {
            //defined_in_module,
            anon_struct_type,
            name,
            assigned_name: assigned_name.to_string(),
            type_id,
        }
    }

    #[must_use]
    pub fn field_index(&self, field_name: &str) -> Option<usize> {
        self.anon_struct_type
            .field_name_sorted_fields
            .get_index(&field_name.to_string())
    }

    #[must_use]
    pub const fn name(&self) -> &Node {
        &self.name
    }
}
