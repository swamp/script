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
use std::cell::RefCell;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;

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
        if self.file_id == 0xffff {
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

#[derive(Clone)]
pub struct ResolvedParameter {
    pub name: ResolvedNode,
    pub resolved_type: ResolvedType,
    pub is_mutable: Option<ResolvedNode>,
}

impl Debug for ResolvedParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ResolvedParameter")
    }
}

impl ResolvedParameter {
    #[inline]
    #[must_use]
    pub const fn is_mutable(&self) -> bool {
        self.is_mutable.is_some()
    }
}

#[derive(Debug)]
pub struct ResolvedFunctionSignature {
    pub first_parameter_is_self: bool,
    pub parameters: Vec<ResolvedParameter>,
    pub return_type: ResolvedType,
}

#[derive(Debug)]
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
pub enum ResolvedType {
    // Primitives
    Int(ResolvedIntTypeRef),
    Float(ResolvedFloatTypeRef),
    String(ResolvedStringTypeRef),
    Bool(ResolvedBoolTypeRef),
    Unit(ResolvedUnitTypeRef),

    Array(ResolvedArrayTypeRef),
    Tuple(ResolvedTupleTypeRef),
    Struct(ResolvedStructTypeRef),
    Map(ResolvedMapTypeRef),

    Generic(Box<ResolvedType>, Vec<ResolvedType>),

    Enum(ResolvedEnumTypeRef),
    EnumVariant(ResolvedEnumVariantTypeRef),

    FunctionInternal(ResolvedInternalFunctionDefinitionRef),
    FunctionExternal(ResolvedExternalFunctionDefinitionRef),

    ExclusiveRange(ResolvedExclusiveRangeTypeRef),

    Optional(Box<ResolvedType>),

    RustType(ResolvedRustTypeRef),

    Any,
}

impl Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::Int(_) => write!(f, "Int"),
            Self::Float(_) => write!(f, "Float"),
            Self::String(_) => write!(f, "String"),
            Self::Bool(_) => write!(f, "Bool"),
            Self::Unit(_) => write!(f, "()"),
            Self::Array(array_ref) => write!(f, "[{}]", &array_ref.item_type.to_string()),
            Self::Tuple(tuple) => write!(f, "({})", comma(&tuple.0)),
            Self::Struct(struct_ref) => write!(f, "{}", struct_ref.borrow().assigned_name),
            Self::Map(_) => todo!(),
            Self::Generic(_, _) => todo!(),
            Self::Enum(_) => todo!(),
            Self::EnumVariant(_) => todo!(),
            Self::FunctionInternal(_) => todo!(),
            Self::FunctionExternal(_) => todo!(),
            Self::ExclusiveRange(_) => todo!(),
            Self::Optional(_) => todo!(),
            Self::RustType(_) => todo!(),
            Self::Any => todo!(),
        }
    }
}

impl Spanned for ResolvedType {
    fn span(&self) -> Span {
        match self {
            // Primitives
            Self::Int(_type_ref) => Span::dummy(),
            Self::Float(_type_ref) => Span::dummy(),
            Self::String(_type_ref) => todo!(),
            Self::Bool(_type_ref) => Span::dummy(),
            Self::Unit(_type_ref) => todo!(),

            // Compound Types
            Self::Array(_type_ref) => todo!(),
            Self::Tuple(tuple_ref) => tuple_ref.0[0].span(),
            Self::Struct(type_ref) => type_ref.borrow().name.span.clone(),
            Self::Map(type_ref) => type_ref.key_type.span(),

            // Generic Types
            Self::Generic(base_type, type_params) => base_type.span().merge_iter(type_params),

            // Enum Types
            Self::Enum(_type_ref) => todo!(),
            Self::EnumVariant(_type_ref) => todo!(),

            // Function Types
            Self::FunctionInternal(_func_ref) => todo!(),
            Self::FunctionExternal(_func_ref) => todo!(),

            // Range Type
            Self::ExclusiveRange(_type_ref) => todo!(),

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
}

impl ResolvedType {
    pub fn expect_struct_type(&self) -> Result<ResolvedStructTypeRef, SemanticError> {
        match self {
            ResolvedType::Struct(struct_type_ref) => Ok(struct_type_ref.clone()),
            _ => Err(SemanticError::ResolveNotStruct),
        }
    }

    pub fn same_type(&self, other: &ResolvedType) -> bool {
        match (self, other) {
            (Self::Any, _) => true,
            (_, Self::Any) => true,
            (Self::Int(_), Self::Int(_)) => true,
            (Self::Float(_), Self::Float(_)) => true,
            (Self::String(_), Self::String(_)) => true,
            (Self::Bool(_), Self::Bool(_)) => true,
            (Self::Unit(_), Self::Unit(_)) => true,
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
            (Self::FunctionInternal(_), Self::FunctionInternal(_)) => true,
            (Self::ExclusiveRange(_), Self::ExclusiveRange(_)) => true,
            (Self::EnumVariant(a), Self::EnumVariant(b)) => a.owner.number == b.owner.number,
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
    if !Rc::ptr_eq(a, b) {
        return false;
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

#[derive(Debug)]
pub struct ResolvedLocalIdentifier(pub ResolvedNode);

#[derive()]
pub struct ResolvedInternalFunctionDefinition {
    pub body: ResolvedExpression,
    pub name: ResolvedLocalIdentifier,
    pub signature: ResolvedFunctionSignature,
}

impl Spanned for ResolvedInternalFunctionDefinition {
    fn span(&self) -> Span {
        self.name.0.span.clone()
    }
}

impl Debug for ResolvedInternalFunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "InternalFuncDef")
    }
}

pub type ResolvedInternalFunctionDefinitionRef = Rc<ResolvedInternalFunctionDefinition>;

pub type ExternalFunctionId = u32;

pub struct ResolvedExternalFunctionDefinition {
    pub name: ResolvedNode,
    pub signature: ResolvedFunctionSignature,
    pub id: ExternalFunctionId,
}
impl Debug for ResolvedExternalFunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "external fn")
    }
}

impl Spanned for ResolvedExternalFunctionDefinition {
    fn span(&self) -> Span {
        self.name.span.clone()
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

#[derive(Debug, Clone)]
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

#[derive(Debug)]
pub enum ResolvedFunction {
    Internal(ResolvedInternalFunctionDefinitionRef),
    External(ResolvedExternalFunctionDefinitionRef),
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
    pub display_type_ref: ResolvedType,
    pub has_rest: bool,
}

#[derive(Debug)]
pub struct ResolvedVariableAssignment {
    pub variable_refs: Vec<ResolvedVariableRef>, // Support single or multiple variables
    pub expression: Box<ResolvedExpression>,
}
impl Spanned for ResolvedVariableAssignment {
    fn span(&self) -> Span {
        self.variable_refs[0].span()
    }
}

#[derive(Debug)]
pub enum ResolvedCompoundOperatorKind {
    Add,
    Sub,
    Mul,
    Div,
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
pub enum ResolvedExpression {
    // Access Lookup values
    VariableAccess(ResolvedVariableRef),
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

    InternalFunctionAccess(ResolvedInternalFunctionDefinitionRef),
    ExternalFunctionAccess(ResolvedExternalFunctionDefinitionRef),

    MutVariableRef(ResolvedMutVariableRef), // Used when passing with mut keyword. mut are implicitly passed by reference
    MutStructFieldRef(Box<ResolvedExpression>, Vec<ResolvedAccess>),
    MutArrayIndexRef(Box<ResolvedExpression>, Vec<ResolvedAccess>),

    Option(Option<Box<ResolvedExpression>>),

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
    FunctionInternalCall(ResolvedInternalFunctionCall), // ResolvedFunctionReference, Vec<ResolvedExpression>
    FunctionExternalCall(ResolvedExternalFunctionCall),
    StaticCall(ResolvedStaticCall),
    StaticCallGeneric(ResolvedStaticCallGeneric),
    MutMemberCall(MutMemberRef, Vec<ResolvedExpression>),
    MemberCall(ResolvedMemberCall),

    InterpolatedString(ResolvedStringTypeRef, Vec<ResolvedStringPart>),

    // Constructing
    StructInstantiation(ResolvedStructInstantiation),
    Array(ResolvedArrayInstantiation),
    Tuple(Vec<ResolvedExpression>),
    Literal(ResolvedLiteral),
    //Map(HashMap<ResolvedExpression, ResolvedExpression>), // Not implemented yet. Maybe call this a dictionary or similar, to avoid confusion with map()
    ExclusiveRange(
        ResolvedExclusiveRangeTypeRef,
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
    ),

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
    LetVar(ResolvedVariableRef, Box<ResolvedExpression>),
    ArrayRemoveIndex(ResolvedVariableRef, Box<ResolvedExpression>),
    ArrayClear(ResolvedVariableRef),

    FloatRound(Box<ResolvedExpression>),
    FloatFloor(Box<ResolvedExpression>),
    FloatSign(Box<ResolvedExpression>),
    FloatAbs(Box<ResolvedExpression>),

    // --- Special methods
    // TODO: Have a better interface for these "engine" member calls
    SparseAdd(Box<ResolvedExpression>, Box<ResolvedExpression>),
    SparseRemove(Box<ResolvedExpression>, Box<ResolvedExpression>),
    SparseNew(ResolvedRustTypeRef, ResolvedType),

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
}

pub trait Spanned {
    fn span(&self) -> Span;
}

impl Spanned for ResolvedExpression {
    fn span(&self) -> Span {
        match self {
            Self::VariableAccess(var_ref) => var_ref.span(),
            Self::FieldAccess(base, field, accesses) => {
                let mut span = base.span().merge(&field.span());
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
                .map(|expr| expr.span())
                .unwrap_or_else(Span::dummy),

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
            Self::FunctionInternalCall(_call) => todo!(),
            Self::FunctionExternalCall(call) => call.arguments[0].span(),
            Self::StaticCall(call) => call.span(),
            Self::StaticCallGeneric(_call) => todo!(),
            Self::MutMemberCall(_member_ref, _args) => {
                todo!()
            }
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
            Self::InterpolatedString(_str_ref, parts) => parts[0].span(),

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
            Self::ExclusiveRange(_range_ref, _start, _end) => {
                todo!()
            }

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

            // Special Methods
            Self::SparseAdd(expr1, expr2) => expr1.span().merge(&expr2.span()),
            Self::SparseRemove(expr1, expr2) => expr1.span().merge(&expr2.span()),
            Self::SparseNew(_rust_type_ref, _resolved_type) => {
                todo!()
            }
            Self::ForLoop(_pattern, _iterator, statements) => statements.span(),
            Self::WhileLoop(_condition, _expression) => todo!(),
            Self::Return(_maybe_expr) => todo!(),
            Self::Break(node) => node.span(),
            Self::Continue(node) => node.span(),
            Self::If(_condition, _true_expr, _false_expr) => todo!(),
            Self::IfOnlyVariable { .. } => todo!(),
            Self::IfAssignExpression { .. } => todo!(),
            Self::MutStructFieldRef(_, _) => todo!(),
            Self::MutArrayIndexRef(_, _) => todo!(),
            Self::TupleDestructuring(_, _, _) => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct ResolvedStringConst(pub ResolvedNode);

#[derive(Debug)]
pub enum ResolvedLiteral {
    FloatLiteral(Fp, ResolvedNode, ResolvedFloatTypeRef),
    UnitLiteral(ResolvedUnitTypeRef),
    NoneLiteral(ResolvedNode),
    IntLiteral(i32, ResolvedNode, ResolvedIntTypeRef),
    StringLiteral(String, ResolvedNode, ResolvedStringTypeRef),
    BoolLiteral(bool, ResolvedNode, ResolvedBoolTypeRef),
    EnumVariantLiteral(ResolvedEnumVariantTypeRef, ResolvedEnumLiteralData),
    TupleLiteral(ResolvedTupleTypeRef, Vec<ResolvedExpression>),
    Array(ResolvedArrayTypeRef, Vec<ResolvedExpression>),
    Map(
        ResolvedMapTypeRef,
        Vec<(ResolvedExpression, ResolvedExpression)>,
    ),
}

impl Spanned for ResolvedLiteral {
    fn span(&self) -> Span {
        match self {
            ResolvedLiteral::FloatLiteral(_v, node, _) => node.span.clone(),
            ResolvedLiteral::UnitLiteral(_) => Span::dummy(), // TODO: UnitLiteral should have node
            ResolvedLiteral::NoneLiteral(node) => node.span.clone(),
            ResolvedLiteral::IntLiteral(_, node, _) => node.span.clone(),
            ResolvedLiteral::StringLiteral(_, node, _) => node.span.clone(),
            ResolvedLiteral::BoolLiteral(_, node, _) => node.span.clone(),
            ResolvedLiteral::EnumVariantLiteral(variant_type_ref, _) => {
                variant_type_ref.name.0.span.clone()
            }
            ResolvedLiteral::TupleLiteral(_tuple_type_ref, _tuples) => {
                todo!()
            }
            ResolvedLiteral::Array(_, _) => todo!(),
            ResolvedLiteral::Map(_, _) => todo!(),
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

#[derive(Debug)]
pub struct ResolvedModulePathItem(pub ResolvedNode);
#[derive(Debug)]
pub struct ResolvedModulePath(pub Vec<ResolvedModulePathItem>);

pub type ResolvedStructTypeRef = Rc<RefCell<ResolvedStructType>>;

pub type TypeNumber = u32;

#[derive(Debug)]
pub struct ResolvedIdentifierName(pub ResolvedNode);

#[derive(Debug)]
pub struct ResolvedLocalTypeIdentifier(pub ResolvedNode);

pub type ResolvedModulePathRef = Rc<ResolvedModulePath>;

#[derive(Debug)]
pub struct ResolvedStructType {
    pub name: ResolvedNode,
    pub assigned_name: String,
    pub anon_struct_type: ResolvedAnonymousStructType,

    // Resolved
    pub functions: SeqMap<String, ResolvedFunctionRef>,
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
        function_name: &str,
        external_func: ResolvedExternalFunctionDefinitionRef,
    ) -> Result<(), SeqMapError> {
        let func = ResolvedFunction::External(external_func);
        self.functions
            .insert(function_name.to_string(), func.into())?;
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

#[derive(Debug)]
pub struct ResolvedArrayType {
    pub item_type: ResolvedType,
}

pub type ResolvedMapTypeRef = Rc<ResolvedMapType>;

#[derive(Debug)]
pub struct ResolvedMapType {
    pub key_type: ResolvedType,
    pub value_type: ResolvedType,
}

pub type ResolvedEnumVariantStructTypeRef = Rc<ResolvedEnumVariantStructType>;

#[derive(Debug)]
pub struct CommonEnumVariantType {
    pub number: TypeNumber,
    pub module_path: ResolvedModulePath,
    pub variant_name: ResolvedLocalTypeIdentifier,
    pub assigned_name: String,
    pub enum_ref: ResolvedEnumTypeRef,
}

#[derive(Debug, Clone)]
pub struct ResolvedAnonymousStructType {
    pub defined_fields: SeqMap<String, ResolvedAnonymousStructFieldType>,
}

#[derive(Debug)]
pub struct ResolvedEnumVariantStructType {
    pub common: CommonEnumVariantType,

    pub anon_struct: ResolvedAnonymousStructType,
}

pub type ResolvedEnumVariantTupleTypeRef = Rc<ResolvedEnumVariantTupleType>;

#[derive(Debug)]
pub struct ResolvedEnumVariantTupleType {
    pub common: CommonEnumVariantType,

    pub fields_in_order: Vec<ResolvedType>,
}

pub type ResolvedTupleTypeRef = Rc<ResolvedTupleType>;

#[derive(Debug)]
pub struct ResolvedTupleType(pub Vec<ResolvedType>);

impl ResolvedTupleType {
    pub fn new(types: Vec<ResolvedType>) -> Self {
        Self(types)
    }
}

pub type ResolvedEnumTypeRef = Rc<ResolvedEnumType>;

#[derive(Debug)]
pub struct ResolvedEnumType {
    pub name: ResolvedLocalTypeIdentifier,
    pub assigned_name: String,
    pub module_path: Vec<String>,
    pub number: TypeNumber,
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
        }
    }

    pub fn name(&self) -> &ResolvedLocalTypeIdentifier {
        &self.name
    }
}

pub type ResolvedEnumVariantTypeRef = Rc<ResolvedEnumVariantType>;

#[derive(Debug)]
pub struct ResolvedEnumVariantType {
    pub owner: ResolvedEnumTypeRef,
    pub data: ResolvedEnumVariantContainerType,
    pub name: ResolvedLocalTypeIdentifier,
    pub assigned_name: String,
    pub number: TypeNumber,
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
    ) -> Self {
        Self {
            owner,
            data,
            name,
            assigned_name: assigned_name.to_string(),
            number,
        }
    }

    pub fn container(&self) -> &ResolvedEnumVariantContainerType {
        &self.data
    }

    pub fn name(&self) -> &ResolvedLocalTypeIdentifier {
        &self.name
    }
}

#[derive(Debug, Clone)]
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
    EnumType(ResolvedEnumTypeRef, Vec<ResolvedEnumVariantTypeRef>),
    Function(),
    ExternalFunction(),
    ImplType(ResolvedType),
    FunctionDef(ResolvedFunction),
    Alias(ResolvedType),
    Comment(ResolvedNode),
    Use(ResolvedUse),
}

// Immutable part
#[derive(Debug)]
pub struct ResolvedProgramTypes {
    pub int_type: ResolvedIntTypeRef,
    pub float_type: ResolvedFloatTypeRef,
    pub string_type: ResolvedStringTypeRef,
    pub bool_type: ResolvedBoolTypeRef,
    pub unit_type: ResolvedUnitTypeRef,
    pub none_type: ResolvedNoneTypeRef,
    pub exclusive_range_type: ResolvedExclusiveRangeTypeRef,
}

impl ResolvedProgramTypes {
    pub fn new() -> Self {
        Self {
            int_type: Rc::new(ResolvedIntType {}),
            float_type: Rc::new(ResolvedFloatType),
            string_type: Rc::new(ResolvedStringType),
            bool_type: Rc::new(ResolvedBoolType),
            unit_type: Rc::new(ResolvedUnitType),
            none_type: Rc::new(ResolvedNoneType),
            exclusive_range_type: Rc::new(ResolvedExclusiveRangeType),
        }
    }

    pub fn unit_type(&self) -> ResolvedType {
        ResolvedType::Unit(self.unit_type.clone())
    }

    pub fn int_type(&self) -> ResolvedType {
        ResolvedType::Int(self.int_type.clone())
    }

    pub fn float_type(&self) -> ResolvedType {
        ResolvedType::Float(self.float_type.clone())
    }

    pub fn string_type(&self) -> ResolvedType {
        ResolvedType::String(self.string_type.clone())
    }
    pub fn bool_type(&self) -> ResolvedType {
        ResolvedType::Bool(self.bool_type.clone())
    }
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
