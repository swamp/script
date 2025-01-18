/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod modules;
pub mod ns;
pub mod prelude;

pub use fixed32::Fp;
use seq_fmt::comma;
use seq_map::{SeqMap, SeqMapError};
use seq_set::SeqSet;
use std::cell::RefCell;
use std::cmp::PartialEq;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;
use tracing::{error, info};

#[derive(Clone, Eq, PartialEq, Default)]
pub struct ResolvedNode {
    pub span: Span,
}

impl Spanned for ResolvedNode {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl Debug for ResolvedNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.span.file_id == 0xffff {
            write!(f, "<{}:{}>", self.span.offset, self.span.length)
        } else {
            write!(
                f,
                "<{}:{} ({})>",
                self.span.offset, self.span.length, self.span.file_id
            )
        }
    }
}

pub type FileId = u16;

#[derive(PartialEq, Eq, Hash, Default, Clone)]
pub struct Span {
    pub file_id: FileId,
    pub offset: u32,
    pub length: u16,
}

impl Span {
    pub fn merge(&self, other: &Span) -> Span {
        if other.file_id == 0xffff {
            return self.clone();
        }
        if other.file_id == 0 {
            return self.clone();
        }
        if self.file_id == 0xffff {
            return self.clone();
        }
        if self.file_id == 0 {
            return self.clone();
        }

        assert_eq!(
            self.file_id, other.file_id,
            "file_id must be the same when merging"
        );
        if self.offset <= other.offset {
            // self starts first (or same position)
            let end = other.offset + other.length as u32;
            let my_end = self.offset + self.length as u32;
            let final_end = end.max(my_end);

            Span {
                offset: self.offset,
                length: (final_end - self.offset) as u16,
                file_id: self.file_id,
            }
        } else {
            // other starts first
            other.merge(self) // reuse the logic by swap the arguments around
        }
    }

    pub fn merge_opt<S: Spanned>(&self, other: Option<&S>) -> Span {
        match other {
            Some(spanned) => self.merge(&spanned.span()),
            None => self.clone(),
        }
    }

    pub fn merge_spanned<S: Spanned>(&self, other: &S) -> Span {
        self.merge(&other.span())
    }

    pub fn merge_opt_span(&self, other: Option<Span>) -> Span {
        match other {
            Some(span) => self.merge(&span),
            None => self.clone(),
        }
    }

    pub fn dummy() -> Self {
        Span {
            offset: 0,
            length: 0,
            file_id: 0xffff,
        }
    }

    // Helper method to get the end position
    pub fn end(&self) -> u32 {
        self.offset + self.length as u32
    }

    // Merge spans from an iterator of Spanned items
    pub fn merge_all<'a, I, S>(iter: I) -> Option<Span>
    where
        I: IntoIterator<Item = &'a S>,
        S: Spanned + 'a,
    {
        iter.into_iter()
            .map(|s| s.span())
            .reduce(|acc, span| acc.merge(&span))
    }

    // Merge spans from an iterator with a base span
    pub fn merge_iter<'a, I, S>(&self, iter: I) -> Span
    where
        I: IntoIterator<Item = &'a S>,
        S: Spanned + 'a,
    {
        iter.into_iter()
            .fold(self.clone(), |acc, item| acc.merge(&item.span()))
    }

    // Merge optional spans from an iterator
    pub fn merge_opt_iter<'a, I, S>(&self, iter: I) -> Span
    where
        I: IntoIterator<Item = &'a Option<S>>,
        S: Spanned + 'a,
    {
        iter.into_iter()
            .filter_map(|opt| opt.as_ref())
            .fold(self.clone(), |acc, item| acc.merge(&item.span()))
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}:{} ({})>", self.offset, self.length, self.file_id)
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct ResolvedParameterNode {
    pub name: ResolvedNode,
    pub is_mutable: Option<ResolvedNode>,
}

impl Debug for ResolvedParameterNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ResolvedParameter")
    }
}

impl ResolvedParameterNode {
    #[inline]
    #[must_use]
    pub const fn is_mutable(&self) -> bool {
        self.is_mutable.is_some()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionTypeSignature {
    pub first_parameter_is_self: bool,
    pub parameters: Vec<ResolvedTypeForParameter>,
    pub return_type: Box<ResolvedType>,
}

impl Display for FunctionTypeSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "({})->{}", comma(&self.parameters), self.return_type)
    }
}

impl FunctionTypeSignature {
    pub fn same_type(&self, other: &FunctionTypeSignature) -> bool {
        if self.first_parameter_is_self != other.first_parameter_is_self
            || self.parameters.len() != other.parameters.len()
            || !self.return_type.same_type(&other.return_type)
        {
            return false;
        }

        for (param, other_param) in self.parameters.iter().zip(other.parameters.clone()) {
            if !param.resolved_type.same_type(&other_param.resolved_type) {
                return false;
            }

            if param.is_mutable != other_param.is_mutable {
                return false;
            }
        }

        true
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ResolvedRustType {
    pub type_name: String, // To identify the specific Rust type
    pub number: u32,       // For type comparison
}

impl Spanned for ResolvedRustType {
    fn span(&self) -> Span {
        Span::dummy()
    }
}

pub type ResolvedRustTypeRef = Rc<ResolvedRustType>;

#[derive(Debug, Clone)]
pub struct LocalTypeName(pub ResolvedNode);

impl Spanned for LocalTypeName {
    fn span(&self) -> Span {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedTypeForParameter {
    pub name: String,
    pub resolved_type: ResolvedType,
    pub is_mutable: bool,
    pub node: Option<ResolvedParameterNode>,
}

impl Display for ResolvedTypeForParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(
            f,
            "{}{}: {}",
            if self.is_mutable { "mut " } else { "" },
            self.name,
            self.resolved_type
        )
    }
}

impl Eq for ResolvedTypeForParameter {}

impl PartialEq for ResolvedTypeForParameter {
    fn eq(&self, other: &Self) -> bool {
        // Name and Nodes are not interesting for a function signature
        self.resolved_type.same_type(&other.resolved_type) && self.is_mutable == other.is_mutable
    }
}

#[derive(Clone, Eq, PartialEq)]
pub enum ResolvedType {
    // Primitives
    Int,
    Float,
    String,
    Bool,
    Unit,

    // Containers
    Array(ResolvedArrayTypeRef),
    Tuple(ResolvedTupleTypeRef),
    Struct(ResolvedStructTypeRef),
    Map(ResolvedMapTypeRef),

    Enum(ResolvedEnumTypeRef),
    //EnumVariant(ResolvedEnumVariantTypeRef),
    Generic(Box<ResolvedType>, Vec<ResolvedType>),

    Function(FunctionTypeSignature),
    Iterable(Box<ResolvedType>),

    Optional(Box<ResolvedType>),
    RustType(ResolvedRustTypeRef),

    Any,
}

impl Debug for ResolvedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::String => write!(f, "String"),
            Self::Bool => write!(f, "Bool"),
            Self::Unit => write!(f, "()"),
            Self::Array(array_type_ref) => write!(f, "[{:?}]", array_type_ref.item_type),
            Self::Tuple(tuple_type_ref) => write!(f, "( {:?} )", tuple_type_ref.0),
            Self::Struct(struct_type_ref) => {
                write!(f, "{}", struct_type_ref.borrow().assigned_name)
            }
            Self::Map(map_type_ref) => write!(
                f,
                "[{:?}:{:?}]",
                map_type_ref.key_type, map_type_ref.value_type
            ),
            Self::Generic(base, parameters) => write!(f, "{:?}<{:?}>", base, parameters),
            Self::Enum(enum_type_ref) => write!(f, "{:?}", enum_type_ref.borrow().assigned_name),
            //            Self::EnumVariant(enum_type_variant) => {
            //              write!(f, "{:?}", enum_type_variant.assigned_name)
            //        }
            Self::Function(function_type_signature) => {
                write!(f, "{:?}", function_type_signature)
            }
            Self::Iterable(type_generated) => write!(f, "Iterable<{type_generated:?}>"),
            Self::Optional(base_type) => write!(f, "{:?}?", base_type),
            Self::RustType(rust_type) => write!(f, "{:?}?", rust_type.type_name),
            Self::Any => write!(f, "Any"),
        }
    }
}

impl Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::String => write!(f, "String"),
            Self::Bool => write!(f, "Bool"),
            Self::Unit => write!(f, "()"),
            Self::Array(array_ref) => write!(f, "[{}]", &array_ref.item_type.to_string()),
            Self::Tuple(tuple) => write!(f, "({})", comma(&tuple.0)),
            Self::Struct(struct_ref) => write!(f, "{}", struct_ref.borrow().assigned_name),
            Self::Map(map_ref) => write!(f, "[{}:{}]", map_ref.key_type, map_ref.value_type),
            Self::Generic(base_type, params) => write!(f, "{base_type}<{}>", comma(params)),
            Self::Enum(enum_type) => write!(f, "{}", enum_type.borrow().assigned_name),
            //Self::EnumVariant(variant) => write!(
            //  f,
            //"{}::{}",
            //variant.owner.assigned_name, variant.assigned_name
            //),
            Self::Function(signature) => write!(f, "function {signature}"),
            Self::Iterable(generating_type) => write!(f, "Iterable<{generating_type}>"),
            Self::Optional(base_type) => write!(f, "{base_type}?"),
            Self::RustType(rust_type) => write!(f, "RustType {}", rust_type.type_name),
            Self::Any => write!(f, "ANY"),
        }
    }
}

impl Spanned for ResolvedType {
    fn span(&self) -> Span {
        match self {
            // Primitives
            Self::Int => Span::dummy(),
            Self::Float => Span::dummy(),
            Self::String => Span::dummy(),
            Self::Bool => Span::dummy(),
            Self::Unit => todo!(),

            // Compound Types
            Self::Array(type_ref) => type_ref.item_type.span(),
            Self::Tuple(tuple_ref) => tuple_ref.0[0].span(),
            Self::Struct(type_ref) => type_ref.borrow().name.span.clone(),
            Self::Map(type_ref) => type_ref.key_type.span(),

            // Generic Types
            Self::Generic(base_type, type_params) => base_type.span().merge_iter(type_params),

            // Enum Types
            Self::Enum(_type_ref) => todo!(),
            //Self::EnumVariant(_type_ref) => todo!(),

            // Function Types
            Self::Function(_signature) => todo!(),

            // Range Type
            Self::Iterable(_type_ref) => todo!(),

            // Optional Type
            Self::Optional(inner_type) => inner_type.span(),

            // Rust Type
            Self::RustType(type_ref) => type_ref.span(),

            // Any Type (might want to use a dummy span or specific location)
            Self::Any => Span::dummy(),
        }
    }
}

#[derive(Debug)]
pub enum SemanticError {
    CouldNotInsertStruct,
    DuplicateTypeAlias(String),
    CanOnlyUseStructForMemberFunctions,
    ResolveNotStruct,
    DuplicateStructName(String),
    DuplicateEnumType(String),
    DuplicateEnumVariantType(String, String),
    DuplicateFieldName(String),
    DuplicateExternalFunction(String),
    DuplicateRustType(String),
    DuplicateConstName(String),
    CircularConstantDependency(Vec<ConstantId>),
    DuplicateConstantId(ConstantId),
    IncompatibleTypes,
}

impl ResolvedType {
    pub fn expect_struct_type(&self) -> Result<ResolvedStructTypeRef, SemanticError> {
        match self {
            ResolvedType::Struct(struct_type_ref) => Ok(struct_type_ref.clone()),
            _ => Err(SemanticError::ResolveNotStruct),
        }
    }

    pub fn assignable_type(&self, other: &ResolvedType) -> bool {
        if self.same_type(other) {
            true
        } else if let Self::Optional(inner_type) = self {
            inner_type.same_type(other)
        } else {
            false
        }
    }
    pub fn same_type(&self, other: &ResolvedType) -> bool {
        match (self, other) {
            (Self::Any, _) => true,
            (_, Self::Any) => true,
            (Self::Function(a), Self::Function(b)) => a.same_type(b),
            (Self::Int, Self::Int) => true,
            (Self::Float, Self::Float) => true,
            (Self::String, Self::String) => true,
            (Self::Bool, Self::Bool) => true,
            (Self::Unit, Self::Unit) => true,
            (Self::Array(_), Self::Array(_)) => true,
            (Self::Map(a), Self::Map(b)) => {
                a.key_type.same_type(&b.key_type) && a.value_type.same_type(&b.value_type)
            }
            (Self::Struct(a), Self::Struct(b)) => compare_struct_types(a, b),
            (Self::Tuple(a), Self::Tuple(b)) => {
                if a.0.len() != b.0.len() {
                    return false;
                }
                a.0.iter().zip(b.0.iter()).all(|(a, b)| a.same_type(b))
            }
            (Self::Enum(_), Self::Enum(_)) => true,
            (Self::Iterable(a), Self::Iterable(b)) => a.same_type(b),
            //(Self::EnumVariant(a), Self::EnumVariant(b)) => a.owner.number == b.owner.number,
            (Self::Optional(inner_type_a), Self::Optional(inner_type_b)) => {
                inner_type_a.same_type(inner_type_b)
            }
            (Self::RustType(type_ref_a), Self::RustType(type_ref_b)) => {
                type_ref_a.number == type_ref_b.number
            }

            (Self::Generic(base_a, params_a), Self::Generic(base_b, params_b)) => {
                if !base_a.same_type(base_b) {
                    return false;
                }

                if params_a.len() != params_b.len() {
                    return false;
                }

                for (param_a, param_b) in params_a.iter().zip(params_b) {
                    if !param_a.same_type(param_b) {
                        return false;
                    }
                }
                true
            }
            _ => false,
        }
    }
}

fn compare_struct_types(a: &ResolvedStructTypeRef, b: &ResolvedStructTypeRef) -> bool {
    let a_borrow = a.borrow();
    let b_borrow = b.borrow();
    if a_borrow.assigned_name != b_borrow.assigned_name {
        return false;
    }

    if a_borrow.anon_struct_type.defined_fields.len()
        != b_borrow.anon_struct_type.defined_fields.len()
    {
        return false;
    }

    for ((a_name, a_type), (b_name, b_type)) in a_borrow
        .anon_struct_type
        .defined_fields
        .iter()
        .zip(b_borrow.anon_struct_type.defined_fields.clone())
    {
        if *a_name != b_name {
            return false;
        }

        if !a_type.field_type.same_type(&b_type.field_type) {
            return false;
        }
    }

    true
}

impl ResolvedNode {
    pub fn new_unknown() -> Self {
        Self {
            span: Span {
                file_id: 0xffff,
                offset: 0,
                length: 0,
            },
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ResolvedLocalIdentifier(pub ResolvedNode);

//#[derive(Debug)]
pub struct ResolvedInternalFunctionDefinition {
    pub body: ResolvedExpression,
    pub name: ResolvedLocalIdentifier,
    pub signature: FunctionTypeSignature,
}

impl Debug for ResolvedInternalFunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}\n{:?}", self.signature, self.body)
    }
}

impl PartialEq<Self> for ResolvedInternalFunctionDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for ResolvedInternalFunctionDefinition {}

impl Spanned for ResolvedInternalFunctionDefinition {
    fn span(&self) -> Span {
        self.name.0.span.clone()
    }
}

pub type ResolvedInternalFunctionDefinitionRef = Rc<ResolvedInternalFunctionDefinition>;

pub type ExternalFunctionId = u32;

pub type ConstantId = u32;

pub struct ResolvedExternalFunctionDefinition {
    pub name: Option<ResolvedNode>,
    pub assigned_name: String,
    pub signature: FunctionTypeSignature,
    pub id: ExternalFunctionId,
}

impl PartialEq<Self> for ResolvedExternalFunctionDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for ResolvedExternalFunctionDefinition {}

impl Debug for ResolvedExternalFunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "external fn")
    }
}

impl Spanned for ResolvedExternalFunctionDefinition {
    fn span(&self) -> Span {
        Span::default()
    }
}

pub type ResolvedExternalFunctionDefinitionRef = Rc<crate::ResolvedExternalFunctionDefinition>;

#[derive(Debug)]
pub struct ResolvedVariable {
    pub name: ResolvedNode,
    pub resolved_type: ResolvedType,
    pub mutable_node: Option<ResolvedNode>,

    pub scope_index: usize,
    pub variable_index: usize,
}

impl Spanned for ResolvedVariable {
    fn span(&self) -> Span {
        self.name.span.merge_opt(self.mutable_node.as_ref())
    }
}

impl ResolvedVariable {
    #[must_use]
    pub const fn is_mutable(&self) -> bool {
        self.mutable_node.is_some()
    }
}

pub type ResolvedVariableRef = Rc<ResolvedVariable>;

#[derive(Debug)]
pub struct ResolvedMutVariable {
    pub variable_ref: ResolvedVariableRef,
}
impl Spanned for ResolvedMutVariable {
    fn span(&self) -> Span {
        self.variable_ref.span()
    }
}

type ResolvedMutVariableRef = Rc<ResolvedMutVariable>;

#[derive(Debug)]
pub enum ResolvedBinaryOperatorKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    LogicalOr,
    LogicalAnd,
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    RangeExclusive,
}

#[derive(Debug)]
pub struct ResolvedBinaryOperator {
    pub left: Box<ResolvedExpression>,
    pub right: Box<ResolvedExpression>,
    pub kind: ResolvedBinaryOperatorKind,
    pub node: ResolvedNode,
    pub resolved_type: ResolvedType,
}

impl Spanned for ResolvedBinaryOperator {
    fn span(&self) -> Span {
        self.node.span.clone()
    }
}

#[derive(Debug)]
pub enum ResolvedUnaryOperatorKind {
    Not,
    Negate,
}
#[derive(Debug)]
pub struct ResolvedUnaryOperator {
    pub left: Box<ResolvedExpression>,
    pub kind: ResolvedUnaryOperatorKind,
    pub resolved_type: ResolvedType,
    pub node: ResolvedNode,
}

impl Spanned for ResolvedUnaryOperator {
    fn span(&self) -> Span {
        self.node.span.clone()
    }
}

#[derive(Debug)]
pub enum ResolvedPostfixOperatorKind {
    Unwrap,
}
#[derive(Debug)]
pub struct ResolvedPostfixOperator {
    pub left: Box<ResolvedExpression>,
    pub kind: ResolvedPostfixOperatorKind,
    pub resolved_type: ResolvedType,
    pub node: ResolvedNode,
}

impl Spanned for ResolvedPostfixOperator {
    fn span(&self) -> Span {
        self.node.span.clone()
    }
}

#[derive()]
pub struct ResolvedInternalFunctionCall {
    pub arguments: Vec<ResolvedExpression>,

    pub function_definition: ResolvedInternalFunctionDefinitionRef,
    pub function_expression: Box<ResolvedExpression>,
}

impl Debug for ResolvedInternalFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "InFuncCall({:?} {:?})",
            self.function_expression, self.arguments
        )
    }
}

#[derive(Debug)]
pub struct ResolvedStaticCall {
    pub function: Rc<ResolvedFunction>,
    pub arguments: Vec<ResolvedExpression>,
}

impl Spanned for ResolvedStaticCall {
    fn span(&self) -> Span {
        self.function.span()
    }
}

#[derive(Debug)]
pub struct ResolvedExternalFunctionCall {
    pub arguments: Vec<ResolvedExpression>,
    pub function_definition: ResolvedExternalFunctionDefinitionRef,
    pub function_expression: Box<ResolvedExpression>,
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

pub fn comma_seq_nl<K: Clone + Hash + Eq + Display, V: Display>(
    values: &SeqMap<K, V>,
    prefix: &str,
) -> String {
    let mut result = String::new();
    for (key, value) in values.iter() {
        result.push_str(format!("{}{}: {}\n", prefix, key, value).as_str());
    }
    result
}

pub fn comma_tuple_ref<K: Display, V: Display>(values: &[(&K, &V)]) -> String {
    let mut result = String::new();
    for (i, (key, value)) in values.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        result.push_str(format!("{}: {}", key, value).as_str());
    }
    result
}

#[derive(Debug)]
pub struct ResolvedMemberCall {
    pub function: ResolvedFunctionRef,
    pub arguments: Vec<ResolvedExpression>,
    pub self_expression: Box<ResolvedExpression>,
    pub self_is_mutable: bool,
}

#[derive(Debug)]
pub enum ResolvedAccess {
    FieldIndex(ResolvedNode, usize),
    ArrayIndex(ResolvedExpression),
    MapIndex(ResolvedExpression),
}

impl Spanned for ResolvedAccess {
    fn span(&self) -> Span {
        match self {
            Self::FieldIndex(node, _) => node.span.clone(),
            Self::ArrayIndex(expr) => expr.span(),
            Self::MapIndex(expr) => expr.span(),
        }
    }
}

pub type ResolvedMutStructTypeFieldRef = Rc<ResolvedMutStructTypeField>;
#[derive(Debug)]
pub struct ResolvedMutStructTypeField {
    pub inner: ResolvedStructTypeFieldRef,
}

pub type ResolvedMutTupleFieldRef = Rc<ResolvedMutTupleField>;

#[derive(Debug)]
pub struct ResolvedStructTypeField {
    pub struct_type_ref: ResolvedStructTypeRef,
    pub field_name: ResolvedLocalIdentifier,
    pub resolved_type: ResolvedType,
    pub index: usize,
}

impl Spanned for ResolvedStructTypeField {
    fn span(&self) -> Span {
        self.field_name.0.span.merge_spanned(&self.resolved_type)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ResolvedAnonymousStructFieldType {
    pub identifier: Option<ResolvedNode>,

    pub field_type: ResolvedType,
}

pub type ResolvedStructTypeFieldRef = Rc<ResolvedStructTypeField>;

pub type ResolvedArrayRef = Rc<ResolvedArray>;

#[derive(Debug)]
pub struct ResolvedArray {}
#[derive(Debug)]
pub struct ResolvedMutArray {
    pub expression: Box<ResolvedExpression>,
    pub array_type_ref: ResolvedArrayTypeRef,
}

#[derive(Debug)]
pub struct ResolvedMutMap {
    pub expression: Box<ResolvedExpression>,
    pub map_type_ref: ResolvedMapTypeRef,
}

#[derive(Debug)]
pub struct ResolvedMapIndexLookup {
    pub map_type: ResolvedType,
    pub item_type: ResolvedType,
    pub map_type_ref: ResolvedMapTypeRef,
    pub index_expression: Box<ResolvedExpression>,
    pub map_expression: Box<ResolvedExpression>,
}

impl Spanned for ResolvedMapIndexLookup {
    fn span(&self) -> Span {
        todo!()
    }
}

#[derive(Debug)]
pub struct ResolvedArrayItem {
    pub item_type: ResolvedType,
    pub int_expression: ResolvedExpression,
    pub array_expression: ResolvedExpression,
    pub array_type: ResolvedType,
}

impl Spanned for ResolvedArrayItem {
    fn span(&self) -> Span {
        todo!()
    }
}

pub type ResolvedArrayItemRef = Rc<ResolvedArrayItem>;

#[derive(Debug)]
pub struct ResolvedIndexType {
    pub expression: Box<ResolvedExpression>,
    pub resolved_type: ResolvedType,
}

#[derive(Debug)]
pub enum ResolvedPrecisionType {
    Float,
    String,
}

#[derive(Debug)]
pub enum ResolvedFormatSpecifierKind {
    LowerHex,                                            // :x
    UpperHex,                                            // :X
    Binary,                                              // :b
    Float,                                               // :f
    Precision(u32, ResolvedNode, ResolvedPrecisionType), // :..2f or :..5s
}

#[derive(Debug)]
pub struct ResolvedFormatSpecifier {
    pub node: ResolvedNode,
    pub kind: ResolvedFormatSpecifierKind,
}

#[derive(Debug)]
pub enum ResolvedStringPart {
    Literal(ResolvedNode, String),
    Interpolation(ResolvedExpression, Option<ResolvedFormatSpecifier>),
}

impl Spanned for ResolvedStringPart {
    fn span(&self) -> Span {
        match self {
            Self::Literal(node, _) => node.span.clone(),
            Self::Interpolation(expr, _) => expr.span(),
        }
    }
}

#[derive(Debug)]
pub struct ResolvedMutTupleField {
    #[allow(unused)]
    ast: ResolvedExpression,
}

pub type ResolvedFunctionRef = Rc<ResolvedFunction>;

#[derive(Debug, Eq, PartialEq)]
pub enum ResolvedFunction {
    Internal(ResolvedInternalFunctionDefinitionRef),
    External(ResolvedExternalFunctionDefinitionRef),
}

impl ResolvedFunction {
    pub fn name(&self) -> Option<&ResolvedNode> {
        match self {
            Self::Internal(x) => Some(&x.name.0),
            Self::External(y) => y.name.as_ref(),
        }
    }

    #[must_use]
    pub fn signature(&self) -> &FunctionTypeSignature {
        match self {
            Self::Internal(internal) => &internal.signature,
            Self::External(external) => &external.signature,
        }
    }
}

impl Spanned for ResolvedFunction {
    fn span(&self) -> Span {
        match self {
            Self::Internal(def) => def.span(),
            Self::External(def) => def.span(),
        }
    }
}

pub type MutMemberRef = Rc<MutMember>;

#[derive(Debug)]
pub struct MutMember {
    #[allow(unused)]
    expression: ResolvedExpression,
}

pub type MemberRef = Rc<Member>;

#[derive(Debug)]
pub struct Member {
    #[allow(unused)]
    expression: ResolvedExpression,
}

#[derive(Debug)]
pub struct ResolvedBooleanExpression {
    #[allow(unused)]
    pub expression: Box<ResolvedExpression>,
}

#[derive(Debug)]
pub struct ResolvedMatch {
    pub arms: Vec<ResolvedMatchArm>,
    pub expression: Box<ResolvedExpression>,
}

#[derive(Debug)]
pub struct ResolvedMatchArm {
    #[allow(unused)]
    pub pattern: ResolvedPattern,
    pub expression: Box<ResolvedExpression>,
    pub expression_type: ResolvedType,
}

#[derive(Debug)]
pub enum ResolvedPattern {
    Normal(ResolvedNormalPattern, Option<ResolvedBooleanExpression>),
    Wildcard(ResolvedNode),
}

#[derive(Debug)]
pub enum ResolvedNormalPattern {
    PatternList(Vec<ResolvedPatternElement>),
    EnumPattern(
        ResolvedEnumVariantTypeRef,
        Option<Vec<ResolvedPatternElement>>,
    ),
    Literal(ResolvedLiteral),
}

#[derive(Debug)]
pub enum ResolvedPatternElement {
    Variable(ResolvedVariableRef),
    VariableWithFieldIndex(ResolvedVariableRef, usize),
    Wildcard(ResolvedNode),
}

#[derive(Debug)]
pub struct ResolvedIterator {
    pub key_type: Option<ResolvedType>, // It does not have to support a key type
    pub value_type: ResolvedType,
    pub resolved_expression: Box<ResolvedExpression>,
}

#[derive(Debug)]
pub struct ResolvedStructInstantiation {
    pub source_order_expressions: Vec<(usize, ResolvedExpression)>,
    pub struct_type_ref: ResolvedStructTypeRef,
}

#[derive(Debug)]
pub struct ResolvedVariableAssignment {
    pub variable_refs: ResolvedVariableRef,
    pub expression: Box<ResolvedExpression>,
}
impl Spanned for ResolvedVariableAssignment {
    fn span(&self) -> Span {
        self.variable_refs.span()
    }
}

#[derive(Debug)]
pub enum ResolvedCompoundOperatorKind {
    Add,
    Sub,
    Mul,
    Div,
    Modulo,
}

#[derive(Debug)]
pub struct ResolvedCompoundOperator {
    pub node: ResolvedNode,
    pub kind: ResolvedCompoundOperatorKind,
}

impl Spanned for ResolvedCompoundOperator {
    fn span(&self) -> Span {
        self.node.span.clone()
    }
}

#[derive(Debug)]
pub struct ResolvedVariableCompoundAssignment {
    pub variable_ref: ResolvedVariableRef, // compound only support single variable
    pub expression: Box<ResolvedExpression>,
    pub compound_operator: ResolvedCompoundOperator,
}

impl Spanned for ResolvedVariableCompoundAssignment {
    fn span(&self) -> Span {
        todo!()
    }
}

pub fn create_rust_type(name: &str, type_number: TypeNumber) -> ResolvedRustTypeRef {
    let rust_type = ResolvedRustType {
        type_name: name.to_string(),
        number: type_number,
    };
    Rc::new(rust_type)
}

#[derive(Debug)]
pub struct ResolvedGuard {
    pub condition: ResolvedBooleanExpression,
    pub result: ResolvedExpression,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ResolvedRangeMode {
    Inclusive,
    Exclusive,
}

#[derive(Debug)]
pub enum ResolvedExpression {
    // Access Lookup values
    VariableAccess(ResolvedVariableRef),
    ConstantAccess(ResolvedConstantRef),
    FieldAccess(
        Box<ResolvedExpression>,
        ResolvedStructTypeFieldRef,
        Vec<ResolvedAccess>,
    ),

    ArrayAccess(
        Box<ResolvedExpression>,
        ResolvedArrayTypeRef,
        Vec<ResolvedAccess>,
    ), // Read from an array: arr[3]

    MapIndexAccess(ResolvedMapIndexLookup),
    MapRemove(
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
        ResolvedMapTypeRef,
    ),
    MapHas(Box<ResolvedExpression>, Box<ResolvedExpression>),

    InternalFunctionAccess(ResolvedInternalFunctionDefinitionRef),

    ExternalFunctionAccess(ResolvedExternalFunctionDefinitionRef),

    // Mut Location
    MutVariableRef(ResolvedMutVariableRef), // Used when passing with mut keyword. mut are implicitly passed by reference
    MutStructFieldRef(Box<ResolvedExpression>, ResolvedType, Vec<ResolvedAccess>),
    MutArrayIndexRef(
        Box<ResolvedExpression>,
        ResolvedArrayTypeRef,
        Vec<ResolvedAccess>,
    ),
    MutMapIndexRef(
        Box<ResolvedExpression>,
        ResolvedMapTypeRef,
        Box<ResolvedExpression>,
    ),
    MutRustTypeIndexRef(
        Box<ResolvedExpression>,
        ResolvedRustTypeRef,
        ResolvedType,
        Box<ResolvedExpression>,
    ),

    Option(Option<Box<ResolvedExpression>>),
    NoneCoalesceOperator(Box<ResolvedExpression>, Box<ResolvedExpression>),

    // Assignment
    // Since it is a cool language, we can "chain" assignments together. like a = b = c = 1. Even for field assignments, like a.b = c.d = e.f = 1
    InitializeVariable(ResolvedVariableAssignment), // First time assignment
    ReassignVariable(ResolvedVariableAssignment),   // Subsequent assignments

    VariableCompoundAssignment(ResolvedVariableCompoundAssignment),

    ArrayExtend(ResolvedVariableRef, Box<ResolvedExpression>), // Extends an array with another array
    ArrayPush(ResolvedVariableRef, Box<ResolvedExpression>),   // Adds an item to an array

    //    CompoundAssignmentIndex(ResolvedIndexCompoundAssignment),
    ArrayAssignment(ResolvedMutArray, ResolvedIndexType, Box<ResolvedExpression>), // target, index, source. Write to an index in an array: arr[3] = 42

    MapAssignment(ResolvedMutMap, ResolvedIndexType, Box<ResolvedExpression>),
    StructFieldAssignment(
        Box<ResolvedExpression>,
        Vec<ResolvedAccess>,
        Box<ResolvedExpression>,
    ),

    FieldCompoundAssignment(
        Box<ResolvedExpression>,
        Vec<ResolvedAccess>,
        ResolvedCompoundOperator,
        Box<ResolvedExpression>,
    ),

    // Operators
    BinaryOp(ResolvedBinaryOperator),
    UnaryOp(ResolvedUnaryOperator),
    PostfixOp(ResolvedPostfixOperator),

    CoerceOptionToBool(Box<ResolvedExpression>),

    // Calls

    // For calls from returned function values
    FunctionCall(
        FunctionTypeSignature,
        Box<ResolvedExpression>,
        Vec<ResolvedExpression>,
    ),

    StaticCall(ResolvedStaticCall),
    StaticCallGeneric(ResolvedStaticCallGeneric),

    FunctionInternalCall(ResolvedInternalFunctionCall),
    FunctionExternalCall(ResolvedExternalFunctionCall),

    MemberCall(ResolvedMemberCall),
    InterpolatedString(Vec<ResolvedStringPart>),

    // Constructing
    StructInstantiation(ResolvedStructInstantiation),
    Array(ResolvedArrayInstantiation),
    Tuple(Vec<ResolvedExpression>),
    Literal(ResolvedLiteral),
    ExclusiveRange(Box<ResolvedExpression>, Box<ResolvedExpression>),

    InclusiveRange(Box<ResolvedExpression>, Box<ResolvedExpression>),

    // Special if-else variants for optional unwrapping
    IfElseOnlyVariable {
        variable: ResolvedVariableRef,
        optional_expr: Box<ResolvedExpression>,
        true_block: Box<ResolvedExpression>,
        false_block: Box<ResolvedExpression>,
    },

    IfElseAssignExpression {
        variable: ResolvedVariableRef,
        optional_expr: Box<ResolvedExpression>,
        true_block: Box<ResolvedExpression>,
        false_block: Box<ResolvedExpression>,
    },

    Match(ResolvedMatch),
    Guard(
        Vec<ResolvedGuard>,
        Option<Box<ResolvedExpression>>,
        ResolvedType,
    ),
    LetVar(ResolvedVariableRef, Box<ResolvedExpression>),
    ArrayRemoveIndex(ResolvedVariableRef, Box<ResolvedExpression>),
    ArrayClear(ResolvedVariableRef),

    // Integer built in
    IntAbs(Box<ResolvedExpression>),
    IntRnd(Box<ResolvedExpression>),
    IntToFloat(Box<ResolvedExpression>),
    IntClamp(
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
    ),
    IntMin(Box<ResolvedExpression>, Box<ResolvedExpression>),
    IntMax(Box<ResolvedExpression>, Box<ResolvedExpression>),

    // Float built in
    FloatRound(Box<ResolvedExpression>),
    FloatFloor(Box<ResolvedExpression>),
    FloatSign(Box<ResolvedExpression>),
    FloatAbs(Box<ResolvedExpression>),
    FloatRnd(Box<ResolvedExpression>),
    FloatCos(Box<ResolvedExpression>),
    FloatSin(Box<ResolvedExpression>),
    FloatAcos(Box<ResolvedExpression>),
    FloatAsin(Box<ResolvedExpression>),
    FloatAtan2(Box<ResolvedExpression>, Box<ResolvedExpression>),
    FloatSqrt(Box<ResolvedExpression>),
    FloatClamp(
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
    ),
    FloatMin(Box<ResolvedExpression>, Box<ResolvedExpression>),
    FloatMax(Box<ResolvedExpression>, Box<ResolvedExpression>),

    // String built in
    StringLen(Box<ResolvedExpression>),

    // Tuple built in
    Tuple2FloatMagnitude(Box<ResolvedExpression>),

    // --- Special methods
    // TODO: Have a better interface for these "engine" member calls
    SparseAdd(
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
        ResolvedType,
    ),
    SparseRemove(
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
        ResolvedType,
    ),
    SparseNew(Span, ResolvedRustTypeRef, ResolvedType, ResolvedType),
    SparseAccess(
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
        ResolvedType,
    ),

    ForLoop(
        ResolvedForPattern,
        ResolvedIterator,
        Box<ResolvedExpression>,
    ),
    WhileLoop(ResolvedBooleanExpression, Box<ResolvedExpression>),
    Return(Option<Box<ResolvedExpression>>),
    Break(ResolvedNode),
    Continue(ResolvedNode), //
    Block(Vec<ResolvedExpression>),
    If(
        ResolvedBooleanExpression,
        Box<ResolvedExpression>,
        Option<Box<ResolvedExpression>>,
    ),

    // Optional If
    IfOnlyVariable {
        variable: ResolvedVariableRef,
        optional_expr: Box<ResolvedExpression>,
        true_block: Box<ResolvedExpression>,
        false_block: Option<Box<ResolvedExpression>>,
    },
    IfAssignExpression {
        variable: ResolvedVariableRef,
        optional_expr: Box<ResolvedExpression>,
        true_block: Box<ResolvedExpression>,
        false_block: Option<Box<ResolvedExpression>>,
    },

    TupleDestructuring(
        Vec<ResolvedVariableRef>,
        ResolvedTupleTypeRef,
        Box<ResolvedExpression>,
    ),
    ArrayRangeAccess(
        Box<ResolvedExpression>,
        ResolvedArrayTypeRef,
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
        ResolvedRangeMode,
    ),
    StringRangeAccess(
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
        ResolvedRangeMode,
    ),
    AssignArrayRange(
        Box<ResolvedExpression>,
        ResolvedArrayTypeRef,
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
        ResolvedRangeMode,
        Box<ResolvedExpression>,
    ),
    AssignStringRange(
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
        ResolvedRangeMode,
        Box<ResolvedExpression>,
    ),
}

impl ResolvedExpression {
    #[must_use]
    pub fn is_coerce_to_mutable(&self) -> bool {
        info!(?self, "checking mutable");
        match self {
            Self::VariableAccess(var_access) => var_access.is_mutable(),
            Self::FieldAccess(expr, ..) => expr.is_coerce_to_mutable(),
            _ => matches!(
                self,
                Self::MutArrayIndexRef(_, _, _)
                    | Self::MutVariableRef(_)
                    | Self::MutStructFieldRef(_, _, _)
                    | Self::MutMapIndexRef(_, _, _),
            ),
        }
    }

    #[allow(clippy::too_many_lines)]
    pub fn collect_constant_dependencies(&self, deps: &mut SeqSet<ConstantId>) {
        match self {
            Self::ConstantAccess(const_ref) => {
                deps.insert(const_ref.id);
            }
            Self::FieldAccess(expr, _, _accesses) => {
                expr.collect_constant_dependencies(deps);
            }
            Self::ArrayAccess(expr, _, _accesses) => {
                expr.collect_constant_dependencies(deps);
            }
            Self::ArrayRangeAccess(base_expr, _, min_expr, max_expr, _mode) => {
                base_expr.collect_constant_dependencies(deps);
                min_expr.collect_constant_dependencies(deps);
                max_expr.collect_constant_dependencies(deps);
            }
            Self::StringRangeAccess(base_expr, min_expr, max_expr, _mode) => {
                base_expr.collect_constant_dependencies(deps);
                min_expr.collect_constant_dependencies(deps);
                max_expr.collect_constant_dependencies(deps);
            }
            Self::MapIndexAccess(map_index) => {
                map_index.map_expression.collect_constant_dependencies(deps);
            }
            Self::InternalFunctionAccess(_func_def_ref) => {}
            Self::ExternalFunctionAccess(_func_def_ref) => {}
            Self::MutVariableRef(_mut_var_ref) => {}
            Self::MutStructFieldRef(expr, _, _accesses) => {
                expr.collect_constant_dependencies(deps);
            }
            Self::MutArrayIndexRef(expr, _resolved_type, _accesses) => {
                expr.collect_constant_dependencies(deps);
            }
            Self::MutMapIndexRef(expr, _resolved_type, _accesses) => {
                expr.collect_constant_dependencies(deps);
            }
            Self::MutRustTypeIndexRef(expr, _resolved_type, _value_type, _accesses) => {
                expr.collect_constant_dependencies(deps);
            }
            Self::Option(opt_expr) => {
                if let Some(expr) = opt_expr {
                    expr.collect_constant_dependencies(deps);
                }
            }
            Self::NoneCoalesceOperator(base_expr, default_expr) => {
                base_expr.collect_constant_dependencies(deps);
                default_expr.collect_constant_dependencies(deps);
            }
            Self::InitializeVariable(assign) | Self::ReassignVariable(assign) => {
                assign.expression.collect_constant_dependencies(deps);
            }
            Self::VariableCompoundAssignment(compound_assign) => {
                compound_assign
                    .expression
                    .collect_constant_dependencies(deps);
            }
            Self::ArrayExtend(_var_ref, expr) | Self::ArrayPush(_var_ref, expr) => {
                expr.collect_constant_dependencies(deps);
            }
            Self::ArrayAssignment(_mut_array, _, expr) => {
                expr.collect_constant_dependencies(deps);
            }
            Self::MapAssignment(_mut_map, _, expr) => {
                expr.collect_constant_dependencies(deps);
            }
            Self::MapRemove(expr, expr2, _) => {
                expr.collect_constant_dependencies(deps);
                expr2.collect_constant_dependencies(deps);
            }

            Self::AssignArrayRange(expr, _array_type, start_expr, end_expr, _mode, assign) => {
                expr.collect_constant_dependencies(deps);
                start_expr.collect_constant_dependencies(deps);
                end_expr.collect_constant_dependencies(deps);
                assign.collect_constant_dependencies(deps);
            }
            Self::AssignStringRange(expr, start_expr, end_expr, _mode, assign) => {
                expr.collect_constant_dependencies(deps);
                start_expr.collect_constant_dependencies(deps);
                end_expr.collect_constant_dependencies(deps);
                assign.collect_constant_dependencies(deps);
            }

            Self::MapHas(expr, expr2) => {
                expr.collect_constant_dependencies(deps);
                expr2.collect_constant_dependencies(deps);
            }
            Self::StructFieldAssignment(expr, _accesses, source_expr) => {
                expr.collect_constant_dependencies(deps);
                source_expr.collect_constant_dependencies(deps);
            }
            Self::FieldCompoundAssignment(expr, _accesses, _, source_expr) => {
                expr.collect_constant_dependencies(deps);
                source_expr.collect_constant_dependencies(deps);
            }
            Self::BinaryOp(bin_op) => {
                bin_op.left.collect_constant_dependencies(deps);
                bin_op.right.collect_constant_dependencies(deps);
            }
            Self::UnaryOp(unary_op) => {
                unary_op.left.collect_constant_dependencies(deps);
            }
            Self::PostfixOp(postfix_op) => {
                postfix_op.left.collect_constant_dependencies(deps);
            }
            Self::CoerceOptionToBool(expr) => {
                expr.collect_constant_dependencies(deps);
            }
            Self::FunctionCall(_func_type, _func_expr, arguments) => {
                for arg in arguments {
                    arg.collect_constant_dependencies(deps);
                }
            }
            Self::FunctionInternalCall(func_call) => {
                for arg in &func_call.arguments {
                    arg.collect_constant_dependencies(deps);
                }
            }
            Self::FunctionExternalCall(func_call) => {
                for arg in &func_call.arguments {
                    arg.collect_constant_dependencies(deps);
                }
            }
            Self::StaticCall(static_call) => {
                for arg in &static_call.arguments {
                    arg.collect_constant_dependencies(deps);
                }
            }
            Self::StaticCallGeneric(static_call) => {
                for arg in &static_call.arguments {
                    arg.collect_constant_dependencies(deps);
                }
            }
            Self::MemberCall(member_call) => {
                for arg in &member_call.arguments {
                    arg.collect_constant_dependencies(deps);
                }
            }

            Self::InterpolatedString(parts) => {
                for part in parts {
                    match part {
                        ResolvedStringPart::Literal(_, _) => {}
                        ResolvedStringPart::Interpolation(expr, _) => {
                            expr.collect_constant_dependencies(deps);
                        }
                    }
                }
            }
            Self::StructInstantiation(struct_inst) => {
                for (_index, expr) in &struct_inst.source_order_expressions {
                    expr.collect_constant_dependencies(deps);
                }
            }
            Self::Array(array_inst) => {
                for expr in &array_inst.expressions {
                    expr.collect_constant_dependencies(deps);
                }
            }
            Self::Tuple(tuple_exprs) => {
                for expr in tuple_exprs {
                    expr.collect_constant_dependencies(deps);
                }
            }
            Self::Literal(_) => {}
            Self::ExclusiveRange(start_expr, end_expr) => {
                start_expr.collect_constant_dependencies(deps);
                end_expr.collect_constant_dependencies(deps);
            }
            Self::InclusiveRange(start_expr, end_expr) => {
                start_expr.collect_constant_dependencies(deps);
                end_expr.collect_constant_dependencies(deps);
            }
            Self::IfElseOnlyVariable {
                optional_expr,
                true_block,
                false_block,
                ..
            } => {
                optional_expr.collect_constant_dependencies(deps);
                true_block.collect_constant_dependencies(deps);
                false_block.collect_constant_dependencies(deps);
            }
            Self::IfElseAssignExpression {
                optional_expr,
                true_block,
                false_block,
                ..
            } => {
                optional_expr.collect_constant_dependencies(deps);
                true_block.collect_constant_dependencies(deps);
                false_block.collect_constant_dependencies(deps);
            }
            Self::Match(resolved_match) => {
                resolved_match
                    .expression
                    .collect_constant_dependencies(deps);
            }

            Self::Guard(guards, wildcard, _resolved_type) => {
                for guard in guards {
                    guard
                        .condition
                        .expression
                        .collect_constant_dependencies(deps);
                    guard.result.collect_constant_dependencies(deps);
                }
                if let Some(found_wildcard) = wildcard {
                    found_wildcard.collect_constant_dependencies(deps);
                }
            }
            Self::LetVar(_var_ref, expr) => {
                expr.collect_constant_dependencies(deps);
            }
            Self::ArrayRemoveIndex(_var_ref, expr) => {
                expr.collect_constant_dependencies(deps);
            }
            Self::ArrayClear(_var_ref) => {
                //var_ref.collect_constant_dependencies(deps);
            }
            Self::FloatRound(expr)
            | Self::FloatFloor(expr)
            | Self::FloatSign(expr)
            | Self::FloatAbs(expr)
            | Self::FloatAcos(expr)
            | Self::FloatCos(expr)
            | Self::FloatSqrt(expr)
            | Self::FloatClamp(expr, _, _)
            | Self::FloatMin(expr, _)
            | Self::FloatMax(expr, _)
            | Self::FloatSin(expr)
            | Self::FloatAsin(expr)
            | Self::FloatAtan2(expr, _) => {
                expr.collect_constant_dependencies(deps);
            }

            Self::StringLen(expr) => {
                expr.collect_constant_dependencies(deps);
            }

            Self::Tuple2FloatMagnitude(expr) => {
                expr.collect_constant_dependencies(deps);
            }
            Self::SparseAdd(expr1, expr2, _) | Self::SparseRemove(expr1, expr2, _) => {
                expr1.collect_constant_dependencies(deps);
                expr2.collect_constant_dependencies(deps);
            }
            Self::SparseNew(_, _, _, _) => {}
            Self::SparseAccess(expr, expr2, _) => {
                expr.collect_constant_dependencies(deps);
                expr2.collect_constant_dependencies(deps);
            }

            Self::ForLoop(_, _, expr) => {
                expr.collect_constant_dependencies(deps);
            }
            Self::WhileLoop(_, expr) => {
                expr.collect_constant_dependencies(deps);
            }
            Self::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    expr.collect_constant_dependencies(deps);
                }
            }
            Self::Break(_) | Self::Continue(_) => {}
            Self::Block(expressions) => {
                for expr in expressions {
                    expr.collect_constant_dependencies(deps);
                }
            }

            Self::If(cond, true_block, false_block) => {
                cond.expression.collect_constant_dependencies(deps);
                true_block.collect_constant_dependencies(deps);
                if let Some(false_block) = false_block {
                    false_block.collect_constant_dependencies(deps);
                }
            }
            Self::IfOnlyVariable {
                //optional_expr,
                true_block,
                false_block,
                ..
            } => {
                true_block.collect_constant_dependencies(deps);
                if let Some(false_block) = false_block {
                    false_block.collect_constant_dependencies(deps);
                }
            }
            Self::IfAssignExpression {
                optional_expr,
                true_block,
                false_block,
                ..
            } => {
                optional_expr.collect_constant_dependencies(deps);
                true_block.collect_constant_dependencies(deps);
                if let Some(false_block) = false_block {
                    false_block.collect_constant_dependencies(deps);
                }
            }
            Self::TupleDestructuring(_vars, _, expr) => {
                expr.collect_constant_dependencies(deps);
            }
            Self::VariableAccess(_) => {}
            Self::IntAbs(_)
            | Self::IntRnd(_)
            | Self::FloatRnd(_)
            | Self::IntClamp(_, _, _)
            | Self::IntMin(_, _)
            | Self::IntMax(_, _) => {}
            Self::IntToFloat(_) => {}
        }
    }

    /// # Errors
    ///
    pub fn resolution_maybe_expecting_type(
        &self,
        expecting_type: Option<ResolvedType>,
    ) -> Result<ResolvedType, SemanticError> {
        expecting_type.map_or_else(
            || Ok(self.resolution()),
            |found_expecting_type| self.resolution_expecting_type(&found_expecting_type),
        )
    }

    /// # Errors
    ///
    pub fn resolution_expecting_type(
        &self,
        expecting_type: &ResolvedType,
    ) -> Result<ResolvedType, SemanticError> {
        let detected_type = self.resolution();
        if expecting_type.assignable_type(&detected_type) {
            Ok(detected_type)
        } else {
            error!(?detected_type, ?expecting_type, "incompatible");
            Err(SemanticError::IncompatibleTypes)
        }
    }

    #[must_use]
    #[allow(clippy::too_many_lines, clippy::match_same_arms)]
    pub fn resolution(&self) -> ResolvedType {
        let resolution_expression = match self {
            // Lookups
            Self::FieldAccess(_expr, struct_field_ref, _lookups) => {
                struct_field_ref.resolved_type.clone()
            }
            Self::VariableAccess(variable_ref) => variable_ref.resolved_type.clone(),
            Self::ConstantAccess(constant_ref) => constant_ref.resolved_type.clone(),

            Self::InternalFunctionAccess(internal_function_def) => {
                ResolvedType::Function(internal_function_def.signature.clone())
            }
            Self::ExternalFunctionAccess(external_function_def) => {
                ResolvedType::Function(external_function_def.signature.clone())
            }

            // Index Access
            Self::ArrayAccess(_, array_item_ref, _) => array_item_ref.item_type.clone(),
            Self::MapIndexAccess(map_item) => {
                ResolvedType::Optional(Box::new(map_item.item_type.clone()))
            }
            Self::MapRemove(_, _, map_type_ref) => map_type_ref.value_type.clone(),
            Self::MapHas(_, _) => ResolvedType::Bool,

            Self::ArrayRangeAccess(_base_expr, array_type_ref, _min, _max, _mode) => {
                ResolvedType::Array(array_type_ref.clone())
            }
            Self::StringRangeAccess(_base_expr, _min, _max, _mode) => ResolvedType::String,

            // Convert to mutable reference
            Self::MutVariableRef(mut_var_ref) => mut_var_ref.variable_ref.resolved_type.clone(),
            Self::MutStructFieldRef(_base_expr, resulting_type, _access_chain) => {
                resulting_type.clone()
            }
            Self::MutRustTypeIndexRef(_, _rust_type, value_type, _x) => {
                ResolvedType::Optional(Box::new(value_type.clone()))
            }
            Self::MutArrayIndexRef(_base, resolved_array_type_ref, _index) => {
                resolved_array_type_ref.item_type.clone()
            }
            Self::MutMapIndexRef(_base, map_type, _index) => {
                ResolvedType::Optional(Box::new(map_type.value_type.clone()))
            }

            // Variable
            Self::InitializeVariable(_variable_assignment) => ResolvedType::Unit,
            Self::ReassignVariable(_variable_assignments) => ResolvedType::Unit,
            Self::VariableCompoundAssignment(_var_compound_assignment) => ResolvedType::Unit,

            // Assignments
            Self::ArrayAssignment(_, _, _) => ResolvedType::Unit,
            Self::MapAssignment(_c, _a, _d) => ResolvedType::Unit,
            Self::StructFieldAssignment(_struct_field, _lookups, _source_resolution) => {
                ResolvedType::Unit
            }
            Self::FieldCompoundAssignment(
                _resolved_expression,
                _access,
                _op_,
                _source_resolution,
            ) => ResolvedType::Unit,

            Self::AssignArrayRange(
                _base_expr,
                _resolved_array_type_ref,
                _start_expr,
                _end_expr,
                _mode,
                _,
            ) => ResolvedType::Unit,

            Self::AssignStringRange(_base, _start_expr, _end_expr, _mode, _) => ResolvedType::Unit,

            // Operators
            Self::BinaryOp(binary_op) => binary_op.resolved_type.clone(),
            Self::UnaryOp(unary_op) => unary_op.resolved_type.clone(),
            Self::PostfixOp(postfix_op) => postfix_op.resolved_type.clone(),

            // Calls
            Self::FunctionCall(signature, _fn_expr, _arguments) => *signature.return_type.clone(),
            Self::MemberCall(call) => *call.function.signature().return_type.clone().clone(),
            Self::FunctionInternalCall(internal_fn_call) => *internal_fn_call
                .function_definition
                .signature
                .return_type
                .clone(),
            Self::FunctionExternalCall(external_fn_call) => *external_fn_call
                .function_definition
                .signature
                .return_type
                .clone(),
            Self::StaticCall(static_call) => *static_call.function.signature().return_type.clone(),
            Self::StaticCallGeneric(static_call_generic) => {
                *static_call_generic.function.signature().return_type.clone()
            }
            Self::InterpolatedString(_parts) => ResolvedType::String,

            // Instantiation
            Self::StructInstantiation(struct_instantiation) => {
                ResolvedType::Struct(struct_instantiation.struct_type_ref.clone())
            }
            Self::Array(array_instantiation) => array_instantiation.array_type.clone(),
            Self::Tuple(_) => todo!(),
            Self::ExclusiveRange(_, _) => ResolvedType::Iterable(Box::new(ResolvedType::Int)),
            Self::InclusiveRange(_, _) => ResolvedType::Iterable(Box::new(ResolvedType::Int)),

            // Option operations
            Self::Option(inner_opt) => inner_opt.as_ref().map_or_else(
                || todo!("Handle None type inference"),
                |inner_expr| {
                    let inner_type = inner_expr.resolution();
                    ResolvedType::Optional(Box::new(inner_type))
                },
            ),

            Self::NoneCoalesceOperator(_base_expr, default_expr) => default_expr.resolution(),

            Self::IfElseOnlyVariable { true_block, .. } => true_block.resolution(),
            Self::IfElseAssignExpression { true_block, .. } => true_block.resolution(),
            Self::IfOnlyVariable { true_block, .. } => true_block.resolution(),
            Self::IfAssignExpression { true_block, .. } => true_block.resolution(),

            Self::LetVar(_, _) => todo!(),

            // Literals
            Self::Literal(literal) => match literal {
                ResolvedLiteral::BoolLiteral(_value, _node) => ResolvedType::Bool,
                ResolvedLiteral::FloatLiteral(_float_value, _node) => ResolvedType::Float,
                ResolvedLiteral::IntLiteral(_int_value, _node) => ResolvedType::Int,
                ResolvedLiteral::StringLiteral(_string_value, _node) => ResolvedType::String,
                ResolvedLiteral::UnitLiteral(_unit_literal) => ResolvedType::Unit,
                ResolvedLiteral::EnumVariantLiteral(variant_ref, _data) => {
                    ResolvedType::Enum(variant_ref.owner.clone())
                }
                ResolvedLiteral::TupleLiteral(tuple_type_ref, _data) => {
                    ResolvedType::Tuple(tuple_type_ref.clone())
                }
                ResolvedLiteral::Array(array_type_ref, _data, _node) => {
                    ResolvedType::Array(array_type_ref.clone())
                }
                ResolvedLiteral::Map(map_type_ref, _data) => {
                    ResolvedType::Map(map_type_ref.clone())
                }
                ResolvedLiteral::NoneLiteral(_) => ResolvedType::Any, // TODO: MUST FIX THIS: panic!("none is not possible here"),
            },

            // Array member functions
            Self::ArrayExtend(variable_ref, _) => variable_ref.resolved_type.clone(),
            Self::ArrayPush(variable_ref, _) => variable_ref.resolved_type.clone(),
            Self::ArrayRemoveIndex(variable_ref, _) => variable_ref.resolved_type.clone(),
            Self::ArrayClear(variable_ref) => variable_ref.resolved_type.clone(),

            // Sparse member functions
            Self::SparseAdd(_, _, return_type) => return_type.clone(),
            Self::SparseRemove(_, _, return_type) => return_type.clone(),
            Self::SparseNew(_span, _rust_type_ref, _item_type, generic_type) => {
                generic_type.clone()
            }
            Self::SparseAccess(_sparse_expression, _key_expression, expected_type) => {
                expected_type.clone()
            }
            Self::CoerceOptionToBool(_) => ResolvedType::Bool,

            // Float member functions
            Self::FloatFloor(_) => ResolvedType::Int,
            Self::FloatRound(_) => ResolvedType::Int,
            Self::FloatSign(_) => ResolvedType::Float,
            Self::FloatAbs(_)
            | Self::FloatCos(_)
            | Self::FloatSin(_)
            | Self::FloatAsin(_)
            | Self::FloatAcos(_)
            | Self::FloatMin(_, _)
            | Self::FloatAtan2(_, _)
            | Self::FloatSqrt(_)
            | Self::FloatClamp(_, _, _)
            | Self::FloatMax(_, _) => ResolvedType::Float,

            Self::FloatRnd(_) => ResolvedType::Int,

            // Int member functions
            Self::IntAbs(_) | Self::IntClamp(_, _, _) | Self::IntMin(_, _) | Self::IntMax(_, _) => {
                ResolvedType::Int
            }
            Self::IntRnd(_) => ResolvedType::Int,
            Self::IntToFloat(_) => ResolvedType::Float,

            // String
            Self::StringLen(_) => ResolvedType::Int,
            Self::Tuple2FloatMagnitude(_) => ResolvedType::Float,

            // Loops
            Self::ForLoop(_pattern, _iterator_expr, expr) => expr.resolution(),
            Self::WhileLoop(_condition, expr) => expr.resolution(),

            // Control
            Self::Return(ref maybe_expr) => maybe_expr
                .as_ref()
                .map_or(ResolvedType::Unit, |expr| expr.resolution()),
            Self::Break(_) => ResolvedType::Unit,
            Self::Continue(_) => ResolvedType::Unit,

            Self::Block(expressions) => expressions
                .last()
                .map_or_else(|| ResolvedType::Unit, Self::resolution),

            // Matching and comparing
            Self::Match(resolved_match) => resolved_match.arms[0].expression_type.clone(),

            Self::Guard(_guards, _wildcard, resolved_type) => resolved_type.clone(),

            Self::If(_, true_expr, _) => true_expr.resolution(),

            // Other
            Self::TupleDestructuring(_, _tuple_type, _expr) => ResolvedType::Unit,
        };

        resolution_expression
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}

#[allow(clippy::too_many_lines, clippy::match_same_arms)]
impl Spanned for ResolvedExpression {
    fn span(&self) -> Span {
        match self {
            Self::VariableAccess(var_ref) => var_ref.span(),
            Self::ConstantAccess(constant_ref) => constant_ref.span(),
            Self::FieldAccess(base, _field, accesses) => {
                let mut span = base.span();
                for access in accesses {
                    span = span.merge(&access.span());
                }
                span
            }
            Self::ArrayAccess(_, _arr_ref, _access) => todo!(),
            Self::MapIndexAccess(lookup) => lookup.span(),

            Self::InternalFunctionAccess(func) => func.span(),
            Self::ExternalFunctionAccess(func) => func.span(),
            Self::MutVariableRef(var_ref) => var_ref.span(),
            Self::Option(opt_expr) => opt_expr
                .as_ref()
                .map_or_else(Span::dummy, |expr| expr.span()),

            // Assignments
            Self::InitializeVariable(assign) => assign.span(),
            Self::ReassignVariable(assign) => assign.span(),
            Self::VariableCompoundAssignment(assign) => assign.span(),
            Self::ArrayExtend(var_ref, expr) => var_ref.span().merge(&expr.span()),
            Self::ArrayPush(var_ref, expr) => var_ref.span().merge(&expr.span()),
            Self::ArrayAssignment(_array, _index, _expr) => {
                todo!()
            }
            Self::MapAssignment(_map, _key, _value) => {
                todo!()
            }
            Self::StructFieldAssignment(base, accesses, value) => {
                let mut span = base.span();
                for access in accesses {
                    span = span.merge(&access.span());
                }
                span.merge(&value.span())
            }
            Self::FieldCompoundAssignment(base, accesses, op, value) => {
                let mut span = base.span();
                for access in accesses {
                    span = span.merge(&access.span());
                }
                span.merge(&op.span()).merge(&value.span())
            }

            // Operators
            Self::BinaryOp(op) => op.span(),
            Self::UnaryOp(op) => op.span(),
            Self::PostfixOp(op) => op.span(),
            Self::CoerceOptionToBool(expr) => expr.span(),

            // Calls
            Self::FunctionCall(_func_type, expr, _arg) => expr.span(),
            Self::FunctionInternalCall(_call) => todo!(),
            Self::FunctionExternalCall(call) => call.arguments[0].span(),
            Self::StaticCall(call) => call.span(),
            Self::StaticCallGeneric(_call) => todo!(),
            Self::MemberCall(call) => call.function.span(),

            // Blocks and Strings
            Self::Block(statements) => statements
                .first()
                .map(|first| {
                    statements
                        .last()
                        .map(|last| first.span().merge(&last.span()))
                        .unwrap_or_else(|| first.span())
                })
                .unwrap_or_else(Span::dummy),
            Self::InterpolatedString(parts) => parts[0].span(),

            // Constructing
            Self::StructInstantiation(struct_inst) => {
                struct_inst.source_order_expressions[0].1.span()
            }
            Self::Array(array_inst) => array_inst.span(),
            Self::Tuple(exprs) => exprs
                .first()
                .map(|first| {
                    exprs
                        .last()
                        .map(|last| first.span().merge(&last.span()))
                        .unwrap_or_else(|| first.span())
                })
                .unwrap_or_else(Span::dummy),
            Self::Literal(lit) => lit.span(),
            Self::ExclusiveRange(start, end) => start.span().merge(&end.span()),

            // Control Flow
            Self::IfElseOnlyVariable {
                variable,
                optional_expr,
                true_block,
                false_block,
            } => variable
                .span()
                .merge(&optional_expr.span())
                .merge(&true_block.span())
                .merge(&false_block.span()),
            Self::IfElseAssignExpression {
                variable,
                optional_expr,
                true_block,
                false_block,
            } => variable
                .span()
                .merge(&optional_expr.span())
                .merge(&true_block.span())
                .merge(&false_block.span()),
            Self::Match(_match_expr) => todo!(),
            Self::LetVar(var_ref, expr) => var_ref.span().merge(&expr.span()),

            // Array Operations
            Self::ArrayRemoveIndex(var_ref, index) => var_ref.span().merge(&index.span()),
            Self::ArrayClear(var_ref) => var_ref.span(),

            // Float Operations
            Self::FloatRound(expr) => expr.span(),
            Self::FloatFloor(expr) => expr.span(),
            Self::FloatSign(expr) => expr.span(),
            Self::FloatAbs(expr) => expr.span(),
            Self::FloatRnd(float_expr) => float_expr.span(),

            // Int operations
            Self::IntAbs(int_expr) => int_expr.span(),
            Self::IntRnd(int_expr) => int_expr.span(),
            Self::IntToFloat(int_expr) => int_expr.span(),

            // Special Methods
            Self::SparseAdd(expr1, expr2, _) => expr1.span().merge(&expr2.span()),
            Self::SparseRemove(expr1, expr2, _) => expr1.span().merge(&expr2.span()),
            Self::SparseNew(span, _rust_type_ref, _resolved_type, _generic_type) => span.clone(),
            Self::ForLoop(_pattern, _iterator, statements) => statements.span(),
            Self::WhileLoop(_condition, _expression) => todo!(),
            Self::Return(_maybe_expr) => todo!(),
            Self::Break(node) => node.span(),
            Self::Continue(node) => node.span(),
            Self::If(_condition, _true_expr, _false_expr) => todo!(),
            Self::IfOnlyVariable { .. } => todo!(),
            Self::IfAssignExpression { .. } => todo!(),
            Self::MutStructFieldRef(a, _b, _access_chain) => a.span(),
            Self::MutArrayIndexRef(_, _, _) => todo!(),
            Self::TupleDestructuring(_, _, _) => todo!(),
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct ResolvedStringConst(pub ResolvedNode);

#[derive(Debug)]
pub enum ResolvedLiteral {
    FloatLiteral(Fp, ResolvedNode),
    UnitLiteral(ResolvedNode),
    NoneLiteral(ResolvedNode),
    IntLiteral(i32, ResolvedNode),
    StringLiteral(String, ResolvedNode),
    BoolLiteral(bool, ResolvedNode),

    EnumVariantLiteral(ResolvedEnumVariantTypeRef, ResolvedEnumLiteralData),
    TupleLiteral(ResolvedTupleTypeRef, Vec<ResolvedExpression>),
    Array(ResolvedArrayTypeRef, Vec<ResolvedExpression>, ResolvedNode),
    Map(
        ResolvedMapTypeRef,
        Vec<(ResolvedExpression, ResolvedExpression)>,
    ),
}

impl Spanned for ResolvedLiteral {
    fn span(&self) -> Span {
        match self {
            Self::FloatLiteral(_v, node) => node.span.clone(),
            Self::UnitLiteral(_) => Span::dummy(), // TODO: UnitLiteral should have node
            Self::NoneLiteral(node) => node.span.clone(),
            Self::IntLiteral(_, node) => node.span.clone(),
            Self::StringLiteral(_, node) => node.span.clone(),
            Self::BoolLiteral(_, node) => node.span.clone(),
            Self::EnumVariantLiteral(variant_type_ref, _) => variant_type_ref.name.0.span.clone(),
            Self::TupleLiteral(_tuple_type_ref, _tuples) => {
                todo!()
            }
            Self::Array(_array_type_ref, _expressions, node) => node.span.clone(),
            Self::Map(_, _) => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct ResolvedArrayInstantiation {
    pub expressions: Vec<ResolvedExpression>,
    pub item_type: ResolvedType,
    pub array_type: ResolvedType,
    pub array_type_ref: ResolvedArrayTypeRef,
}

impl Spanned for ResolvedArrayInstantiation {
    fn span(&self) -> Span {
        todo!()
    }
}

#[derive(Debug)]
pub enum ResolvedForPattern {
    Single(ResolvedVariableRef),
    Pair(ResolvedVariableRef, ResolvedVariableRef),
}

impl ResolvedForPattern {
    #[must_use]
    pub fn is_mutable(&self) -> bool {
        match self {
            Self::Single(variable) => variable.is_mutable(),
            Self::Pair(a, b) => a.is_mutable() || b.is_mutable(),
        }
    }
}

impl Display for ResolvedForPattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "resolved_for_pattern")
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ResolvedModulePathItem(pub ResolvedNode);
#[derive(Debug, Eq, PartialEq)]
pub struct ResolvedModulePath(pub Vec<ResolvedModulePathItem>);

pub type ResolvedStructTypeRef = Rc<RefCell<ResolvedStructType>>;

pub type TypeNumber = u32;

#[derive(Debug)]
pub struct ResolvedIdentifierName(pub ResolvedNode);

#[derive(Debug, Eq, PartialEq)]
pub struct ResolvedLocalTypeIdentifier(pub ResolvedNode);

pub type ResolvedModulePathRef = Rc<ResolvedModulePath>;

#[derive(Debug)]
pub struct ResolvedConstant {
    pub name: ResolvedNode,
    pub assigned_name: String,
    pub id: ConstantId,
    pub expr: ResolvedExpression,
    pub resolved_type: ResolvedType,
}
pub type ResolvedConstantRef = Rc<ResolvedConstant>;

impl Spanned for ResolvedConstant {
    fn span(&self) -> Span {
        self.name.span.clone()
    }
}

#[derive(Eq, PartialEq)]
pub struct ResolvedStructType {
    pub name: ResolvedNode,
    pub assigned_name: String,
    pub anon_struct_type: ResolvedAnonymousStructType,

    // Resolved
    pub functions: SeqMap<String, ResolvedFunctionRef>,
}

impl Debug for ResolvedStructType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "struct {:?}", self.assigned_name)
    }
}

impl ResolvedStructType {
    pub fn new(
        name: ResolvedNode,
        assigned_name: &str,
        anon_struct_type: ResolvedAnonymousStructType,
    ) -> Self {
        Self {
            //defined_in_module,
            anon_struct_type,
            name,
            assigned_name: assigned_name.to_string(),
            functions: SeqMap::default(),
        }
    }

    pub fn field_index(&self, field_name: &str) -> Option<usize> {
        self.anon_struct_type
            .defined_fields
            .get_index(&field_name.to_string())
    }

    pub fn name(&self) -> &ResolvedNode {
        &self.name
    }

    pub fn add_external_member_function(
        &mut self,
        external_func: ResolvedExternalFunctionDefinitionRef,
    ) -> Result<(), SeqMapError> {
        let name = external_func.assigned_name.clone();
        let func = ResolvedFunction::External(external_func);
        self.functions.insert(name, func.into())?;
        Ok(())
    }

    pub fn get_member_function(&self, function_name: &str) -> Option<&ResolvedFunctionRef> {
        self.functions.get(&function_name.to_string())
    }

    pub fn get_internal_member_function(
        &self,
        function_name: &str,
    ) -> Option<ResolvedInternalFunctionDefinitionRef> {
        let func = self.functions.get(&function_name.to_string())?;
        match &**func {
            ResolvedFunction::Internal(fn_def) => Some(fn_def.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ResolvedUnitType;
pub type ResolvedUnitTypeRef = Rc<ResolvedUnitType>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ResolvedNoneType;
pub type ResolvedNoneTypeRef = Rc<ResolvedNoneType>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ResolvedIntType;
pub type ResolvedIntTypeRef = Rc<ResolvedIntType>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ResolvedFloatType;

pub type ResolvedFloatTypeRef = Rc<ResolvedFloatType>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ResolvedBoolType;
pub type ResolvedBoolTypeRef = Rc<ResolvedBoolType>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ResolvedStringType;
pub type ResolvedStringTypeRef = Rc<ResolvedStringType>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ResolvedExclusiveRangeType;
pub type ResolvedExclusiveRangeTypeRef = Rc<ResolvedExclusiveRangeType>;

pub type ResolvedOptionTypeRef = Rc<crate::ResolvedOptionType>;

#[derive(Debug)]
pub struct ResolvedOptionType {
    pub item_type: ResolvedType,
}

pub type ResolvedArrayTypeRef = Rc<ResolvedArrayType>;

#[derive(Debug, Eq, PartialEq)]
pub struct ResolvedArrayType {
    pub item_type: ResolvedType,
}

pub type ResolvedMapTypeRef = Rc<ResolvedMapType>;

#[derive(Debug, Eq, PartialEq)]
pub struct ResolvedMapType {
    pub key_type: ResolvedType,
    pub value_type: ResolvedType,
}

pub type ResolvedEnumVariantStructTypeRef = Rc<ResolvedEnumVariantStructType>;

#[derive(Debug, Eq, PartialEq)]
pub struct CommonEnumVariantType {
    pub number: TypeNumber,
    pub container_index: u8,
    pub module_path: ResolvedModulePath,
    pub variant_name: ResolvedLocalTypeIdentifier,
    pub assigned_name: String,
    pub enum_ref: ResolvedEnumTypeRef,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ResolvedAnonymousStructType {
    pub defined_fields: SeqMap<String, ResolvedAnonymousStructFieldType>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ResolvedEnumVariantStructType {
    pub common: CommonEnumVariantType,

    pub anon_struct: ResolvedAnonymousStructType,
}

pub type ResolvedEnumVariantTupleTypeRef = Rc<ResolvedEnumVariantTupleType>;

#[derive(Debug, Eq, PartialEq)]
pub struct ResolvedEnumVariantTupleType {
    pub common: CommonEnumVariantType,

    pub fields_in_order: Vec<ResolvedType>,
}

pub type ResolvedTupleTypeRef = Rc<ResolvedTupleType>;

#[derive(Debug, Eq, PartialEq)]
pub struct ResolvedTupleType(pub Vec<ResolvedType>);

impl ResolvedTupleType {
    pub fn new(types: Vec<ResolvedType>) -> Self {
        Self(types)
    }
}

pub type ResolvedEnumTypeRef = Rc<RefCell<ResolvedEnumType>>;

#[derive(Debug, Eq, PartialEq)]
pub struct ResolvedEnumType {
    pub name: ResolvedLocalTypeIdentifier,
    pub assigned_name: String,
    pub module_path: Vec<String>,
    pub number: TypeNumber,

    pub variants: SeqMap<String, ResolvedEnumVariantTypeRef>,
}

impl ResolvedEnumType {
    pub fn new(
        name: ResolvedLocalTypeIdentifier,
        assigned_name: &str,
        module_path: Vec<String>,
        number: TypeNumber,
    ) -> Self {
        Self {
            name,
            assigned_name: assigned_name.to_string(),
            module_path,
            number,
            variants: SeqMap::new(),
        }
    }

    pub fn name(&self) -> &ResolvedLocalTypeIdentifier {
        &self.name
    }

    pub fn get_variant(&self, name: &str) -> Option<&ResolvedEnumVariantTypeRef> {
        self.variants.get(&name.to_string())
    }

    pub fn get_variant_from_index(&self, index: usize) -> Option<&ResolvedEnumVariantTypeRef> {
        Some(self.variants.values().collect::<Vec<_>>()[index])
    }
}

pub type ResolvedEnumVariantTypeRef = Rc<ResolvedEnumVariantType>;

#[derive(Debug, Eq, PartialEq)]
pub struct ResolvedEnumVariantType {
    pub owner: ResolvedEnumTypeRef,
    pub data: ResolvedEnumVariantContainerType,
    pub name: ResolvedLocalTypeIdentifier,
    pub assigned_name: String,
    pub number: TypeNumber,
    pub container_index: u8,
}

pub type ResolvedEnumVariantStructFieldTypeRef = Rc<ResolvedEnumVariantStructFieldType>;

#[derive(Debug)]
pub struct ResolvedEnumVariantStructFieldType {
    pub name: ResolvedLocalIdentifier,
    pub enum_variant: ResolvedEnumVariantTypeRef,
    pub resolved_type: ResolvedType,

    pub field_index: usize,
}

pub type ResolvedEnumVariantTupleFieldTypeRef = Rc<ResolvedEnumVariantTupleFieldType>;

#[derive(Debug)]
pub struct ResolvedEnumVariantTupleFieldType {
    pub name: ResolvedLocalIdentifier,
    pub enum_variant: ResolvedEnumVariantTypeRef,
    pub resolved_type: ResolvedType,

    pub field_index: usize,
}

impl ResolvedEnumVariantType {
    pub fn new(
        owner: ResolvedEnumTypeRef,
        name: ResolvedLocalTypeIdentifier,
        assigned_name: &str,
        data: ResolvedEnumVariantContainerType,
        number: TypeNumber,
        container_index: u8,
    ) -> Self {
        Self {
            owner,
            data,
            name,
            assigned_name: assigned_name.to_string(),
            number,
            container_index,
        }
    }

    pub fn container(&self) -> &ResolvedEnumVariantContainerType {
        &self.data
    }

    pub fn name(&self) -> &ResolvedLocalTypeIdentifier {
        &self.name
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ResolvedEnumVariantContainerType {
    Struct(ResolvedEnumVariantStructTypeRef),
    Tuple(ResolvedEnumVariantTupleTypeRef),
    Nothing,
}

#[derive(Debug)]
pub struct ResolvedImplMember {}

#[derive(Debug)]
pub struct ImplType {
    pub members: SeqMap<String, ResolvedImplMember>,
    pub associated_with_struct_type: ResolvedStructTypeRef,
}

impl ImplType {
    pub fn new(
        members: SeqMap<String, ResolvedImplMember>,
        associated_with_struct_type: ResolvedStructTypeRef,
    ) -> Self {
        Self {
            members,
            associated_with_struct_type,
        }
    }
}

#[derive(Debug)]
pub enum ResolvedUseItem {
    Identifier(ResolvedNode),
    TypeIdentifier(ResolvedNode),
}

#[derive(Debug)]
pub struct ResolvedUse {
    pub path: Vec<ResolvedNode>,
    pub items: Vec<ResolvedUseItem>,
}

#[derive(Debug)]
pub enum ResolvedDefinition {
    StructType(ResolvedStructTypeRef),
    EnumType(ResolvedEnumTypeRef),
    Function(),
    ExternalFunction(),
    ImplType(ResolvedType),
    FunctionDef(ResolvedFunction),
    Alias(ResolvedType),
    Comment(ResolvedNode),
    Use(ResolvedUse),
    Constant(ResolvedNode, ResolvedConstantRef),
}

// Mutable part
#[derive(Debug)]
pub struct ResolvedProgramState {
    pub array_types: Vec<ResolvedArrayTypeRef>,
    pub number: TypeNumber,
    pub external_function_number: ExternalFunctionId,
}

impl ResolvedProgramState {
    pub fn new() -> Self {
        Self {
            array_types: Vec::new(),
            number: 0,
            external_function_number: 0,
        }
    }

    pub fn allocate_number(&mut self) -> TypeNumber {
        self.number += 1;
        self.number
    }

    pub fn allocate_external_function_id(&mut self) -> ExternalFunctionId {
        self.external_function_number += 1;
        self.external_function_number
    }
}

#[derive(Debug)]
pub enum ResolvedEnumLiteralData {
    Nothing,
    Tuple(Vec<ResolvedExpression>),
    Struct(Vec<(usize, ResolvedExpression)>),
}

#[derive(Debug)]
pub struct ResolvedStaticCallGeneric {
    pub function: Rc<ResolvedFunction>,
    pub arguments: Vec<ResolvedExpression>,
    pub generic_types: Vec<ResolvedType>,
}
