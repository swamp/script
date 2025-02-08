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
use std::cmp::PartialEq;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;

#[derive(Clone, Eq, PartialEq, Default)]
pub struct ResolvedNode {
    pub span: Span,
    pub markdown_doc: Option<Span>,
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
        if self.parameters.len() != other.parameters.len()
            || !self.return_type.same_type(&other.return_type)
        {
            return false;
        }

        for (param, other_param) in self.parameters.iter().zip(other.parameters.clone()) {
            // If either param.resolved_type or other_param.resolved_type is None,
            // treat them as equal automatically. Otherwise compare them.
            match (&param.resolved_type, &other_param.resolved_type) {
                // If either is None, skip the check and continue
                (None, _) | (_, None) => {}
                // If both are Some, compare them
                (Some(p_type), Some(o_type)) => {
                    if !p_type.same_type(o_type) {
                        return false;
                    }
                }
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

pub type ResolvedRustTypeRef = Rc<ResolvedRustType>;

#[derive(Debug, Clone)]
pub struct ResolvedTypeWithMut {
    pub resolved_type: ResolvedType,
    pub is_mutable: bool,
}

#[derive(Debug, Clone)]
pub struct ResolvedTypeForParameter {
    pub name: String,
    pub resolved_type: Option<ResolvedType>,
    pub is_mutable: bool,
    pub node: Option<ResolvedParameterNode>,
}

impl Display for ResolvedTypeForParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(
            f,
            "{}{}: {:?}",
            if self.is_mutable { "mut " } else { "" },
            self.name,
            self.resolved_type
        )
    }
}

impl Eq for ResolvedTypeForParameter {}

impl PartialEq for ResolvedTypeForParameter {
    fn eq(&self, other: &Self) -> bool {
        // Compare resolved_type in a way that treats either None as automatically equal:
        let types_equal = match (&self.resolved_type, &other.resolved_type) {
            (None, _) | (_, None) => true,
            (Some(t1), Some(t2)) => t1.same_type(t2),
        };

        types_equal && (self.is_mutable == other.is_mutable)
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
    Generic(Box<ResolvedType>, Vec<ResolvedType>),

    Function(FunctionTypeSignature),
    Iterable(Box<ResolvedType>),

    Optional(Box<ResolvedType>),
    RustType(ResolvedRustTypeRef),
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
            Self::Generic(base, parameters) => write!(f, "{base:?}<{parameters:?}>"),
            Self::Enum(enum_type_ref) => write!(f, "{:?}", enum_type_ref.borrow().assigned_name),
            //            Self::EnumVariant(enum_type_variant) => {
            //              write!(f, "{:?}", enum_type_variant.assigned_name)
            //        }
            Self::Function(function_type_signature) => {
                write!(f, "{:?}", function_type_signature)
            }
            Self::Iterable(type_generated) => write!(f, "Iterable<{type_generated:?}>"),
            Self::Optional(base_type) => write!(f, "{base_type:?}?"),
            Self::RustType(rust_type) => write!(f, "{:?}?", rust_type.type_name),
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
    WasNotImmutable,
    WasNotMutable,
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
            markdown_doc: None,
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

pub type ResolvedExternalFunctionDefinitionRef = Rc<crate::ResolvedExternalFunctionDefinition>;

#[derive(Debug)]
pub struct ResolvedVariable {
    pub name: ResolvedNode,
    pub resolved_type: ResolvedType,
    pub mutable_node: Option<ResolvedNode>,

    pub scope_index: usize,
    pub variable_index: usize,
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

//type ResolvedMutVariableRef = Rc<ResolvedMutVariable>;

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
    pub node: ResolvedNode,
}

#[derive()]
pub struct ResolvedInternalFunctionCall {
    pub arguments: Vec<ResolvedArgumentExpressionOrLocation>,

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
pub struct ResolvedExternalFunctionCall {
    pub arguments: Vec<ResolvedArgumentExpressionOrLocation>,
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
    pub arguments: Vec<ResolvedArgumentExpressionOrLocation>,
}

#[derive(Debug)]
pub struct ResolvedStructTypeField {
    pub struct_type_ref: ResolvedStructTypeRef,
    pub field_name: ResolvedNode,
    pub index: usize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ResolvedAnonymousStructFieldType {
    pub identifier: Option<ResolvedNode>,

    pub field_type: ResolvedType,
}

impl Display for ResolvedAnonymousStructFieldType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{:?}:{}", self.identifier, self.field_type)
    }
}

#[derive(Debug)]
pub struct ResolvedMapIndexLookup {
    pub map_type: ResolvedType,
    pub item_type: ResolvedType,
    pub map_type_ref: ResolvedMapTypeRef,
    pub index_expression: Box<ResolvedExpression>,
    pub map_expression: Box<ResolvedExpression>,
}

#[derive(Debug)]
pub struct ResolvedArrayItem {
    pub item_type: ResolvedType,
    pub int_expression: ResolvedExpression,
    pub array_expression: ResolvedExpression,
    pub array_type: ResolvedType,
}

pub type ResolvedArrayItemRef = Rc<ResolvedArrayItem>;

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

pub type ResolvedFunctionRef = Rc<ResolvedFunction>;

#[derive(Debug, Eq, PartialEq)]
pub enum ResolvedFunction {
    Internal(ResolvedInternalFunctionDefinitionRef),
    External(ResolvedExternalFunctionDefinitionRef),
}

impl ResolvedFunction {
    #[must_use]
    pub fn name(&self) -> Option<&ResolvedNode> {
        match self {
            Self::Internal(x) => Some(&x.name.0),
            Self::External(y) => y.name.as_ref(),
        }
    }

    #[must_use]
    pub fn node(&self) -> ResolvedNode {
        match self {
            Self::Internal(x) => x.name.0.clone(),
            Self::External(_y) => ResolvedNode::new_unknown(),
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
pub struct ResolvedIterable {
    pub key_type: Option<ResolvedType>, // It does not have to support a key type
    pub value_type: ResolvedType,

    pub resolved_expression: Box<ResolvedMutOrImmutableExpression>,
}

#[derive(Debug)]
pub struct ResolvedStructInstantiation {
    pub source_order_expressions: Vec<(usize, ResolvedExpression)>,
    pub struct_type_ref: ResolvedStructTypeRef,
}

#[derive(Debug, Eq, PartialEq)]
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

#[derive(Debug)]
pub struct ResolvedVariableCompoundAssignment {
    pub variable_ref: ResolvedVariableRef, // compound only support single variable
    pub expression: Box<ResolvedExpression>,
    pub compound_operator: ResolvedCompoundOperator,
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
    pub condition: Option<ResolvedBooleanExpression>,
    pub result: ResolvedExpression,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ResolvedRangeMode {
    Inclusive,
    Exclusive,
}

#[derive(Debug)]
pub struct ResolvedPostfix {
    pub node: ResolvedNode,
    pub ty: ResolvedType,
    pub kind: ResolvedPostfixKind,
}

#[derive(Debug)]
pub struct ResolvedRange {
    pub min: ResolvedExpression,
    pub max: ResolvedExpression,
    pub mode: ResolvedRangeMode,
}

#[derive(Debug)]
pub enum ResolvedPostfixKind {
    StructField(ResolvedStructTypeRef, usize),
    ArrayIndex(ResolvedArrayTypeRef, ResolvedExpression),
    ArrayRangeIndex(ResolvedArrayTypeRef, ResolvedRange),
    StringIndex(ResolvedExpression),
    StringRangeIndex(ResolvedRange),
    MapIndex(ResolvedMapTypeRef, ResolvedExpression),
    RustTypeIndexRef(ResolvedRustTypeRef, ResolvedExpression),
    MemberCall(
        ResolvedFunctionRef,
        Vec<ResolvedArgumentExpressionOrLocation>,
    ),
    FunctionCall(Vec<ResolvedArgumentExpressionOrLocation>),
    OptionUnwrap, // ? operator
    NoneCoalesce(ResolvedExpression),

    // --- sparse built in
    // TODO: Have a better interface for these "engine" member calls
    SparseAdd(Box<ResolvedExpression>),
    SparseRemove(Box<ResolvedExpression>),
    SparseAccess(Box<ResolvedExpression>),

    ArrayRemoveIndex(Box<ResolvedExpression>),
    ArrayClear,

    // Map built in
    MapRemove(Box<ResolvedExpression>, ResolvedMapTypeRef),
    MapHas(Box<ResolvedExpression>),

    // Integer built in
    IntAbs,
    IntRnd,
    IntToFloat,
    IntClamp(Box<ResolvedExpression>, Box<ResolvedExpression>),
    IntMin(Box<ResolvedExpression>),
    IntMax(Box<ResolvedExpression>),

    // Float built in
    FloatRound,
    FloatFloor,
    FloatSign,
    FloatAbs,
    FloatRnd,
    FloatCos,
    FloatSin,
    FloatAcos,
    FloatAsin,
    FloatAtan2(Box<ResolvedExpression>),
    FloatSqrt,
    FloatClamp(Box<ResolvedExpression>, Box<ResolvedExpression>),
    FloatMin(Box<ResolvedExpression>),
    FloatMax(Box<ResolvedExpression>),

    // String built in
    StringLen,

    // Tuple built in
    Tuple2FloatMagnitude,
}

#[derive(Debug)]
pub enum ResolvedLocationAccessKind {
    FieldIndex(ResolvedStructTypeRef, usize),
    ArrayIndex(ResolvedArrayTypeRef, ResolvedExpression),
    ArrayRange(ResolvedArrayTypeRef, ResolvedRange),
    StringIndex(ResolvedExpression),
    StringRange(ResolvedRange),
    MapIndex(ResolvedMapTypeRef, ResolvedExpression),
    MapIndexInsertIfNonExisting(ResolvedMapTypeRef, ResolvedExpression),
    RustTypeIndex(ResolvedRustTypeRef, ResolvedExpression),
}

#[derive(Debug)]
pub struct ResolvedLocationAccess {
    pub node: ResolvedNode,
    pub ty: ResolvedType,
    pub kind: ResolvedLocationAccessKind,
}

#[derive(Debug)]
pub struct ResolvedSingleLocationExpression {
    pub kind: ResolvedSingleLocationExpressionKind,
    pub node: ResolvedNode,
    pub ty: ResolvedType,

    pub starting_variable: ResolvedVariableRef,
    pub access_chain: Vec<ResolvedLocationAccess>,
}

#[derive(Debug)]
pub struct ResolvedSingleMutLocationExpression(pub ResolvedSingleLocationExpression);

#[derive(Debug)]
pub enum ResolvedSingleLocationExpressionKind {
    MutVariableRef,
    MutStructFieldRef(ResolvedStructTypeRef, usize),
    MutArrayIndexRef(ResolvedArrayTypeRef),
    MutMapIndexRef(ResolvedMapTypeRef),
    MutRustTypeIndexRef(ResolvedRustTypeRef),
}

#[derive(Debug)]
pub struct ResolvedSliceLocationExpression {
    pub start: Box<ResolvedExpression>,
    pub range_start: Box<ResolvedExpression>,
    pub range_end: Box<ResolvedExpression>,
    pub mode: ResolvedRangeMode,
    pub ty: ResolvedType,
}

#[derive(Debug)]
pub struct ResolvedMutOrImmutableExpression {
    pub expression_or_location: ResolvedArgumentExpressionOrLocation,
    pub is_mutable: Option<ResolvedNode>,
}

impl ResolvedMutOrImmutableExpression {}

impl ResolvedMutOrImmutableExpression {
    pub fn expect_immutable(self) -> Result<ResolvedExpression, SemanticError> {
        match self.expression_or_location {
            ResolvedArgumentExpressionOrLocation::Expression(expr) => Ok(expr),
            ResolvedArgumentExpressionOrLocation::Location(_) => {
                Err(SemanticError::WasNotImmutable)
            }
        }
    }

    pub fn expect_immutable_ref(&self) -> Result<&ResolvedExpression, SemanticError> {
        match &self.expression_or_location {
            ResolvedArgumentExpressionOrLocation::Expression(expr) => Ok(expr),
            ResolvedArgumentExpressionOrLocation::Location(_) => {
                Err(SemanticError::WasNotImmutable)
            }
        }
    }

    pub fn ty(&self) -> &ResolvedType {
        match &self.expression_or_location {
            ResolvedArgumentExpressionOrLocation::Expression(expr) => &expr.ty,
            ResolvedArgumentExpressionOrLocation::Location(loc) => &loc.ty,
        }
    }
}

#[derive(Debug)]
pub enum ResolvedArgumentExpressionOrLocation {
    Expression(ResolvedExpression),
    Location(ResolvedSingleLocationExpression),
}

#[derive()]
pub struct ResolvedExpression {
    pub ty: ResolvedType,
    pub node: ResolvedNode,
    pub kind: ResolvedExpressionKind,
}

impl Debug for ResolvedExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{:?}{},{:?}", self.node, self.ty, self.kind)
    }
}

#[derive(Debug)]
pub struct ResolvedWhenBinding {
    pub variable: ResolvedVariableRef,
    pub expr: ResolvedMutOrImmutableExpression,
}

#[derive(Debug)]
pub enum ResolvedExpressionKind {
    // Access Lookup values
    ConstantAccess(ResolvedConstantRef),
    VariableAccess(ResolvedVariableRef),
    FieldAccess(Box<ResolvedExpression>, ResolvedStructTypeField),
    ArrayAccess(
        Box<ResolvedExpression>,
        ResolvedArrayTypeRef,
        Box<ResolvedExpression>, // int index lookup
    ), // Read from an array: arr[3]
    MapIndexAccess(
        Box<ResolvedExpression>,
        ResolvedMapTypeRef,
        Box<ResolvedExpression>,
    ),
    StringRangeAccess(Box<ResolvedExpression>, Box<ResolvedRange>),
    ArrayRangeAccess(Box<ResolvedExpression>, Box<ResolvedRange>),

    // ----
    InternalFunctionAccess(ResolvedInternalFunctionDefinitionRef),
    ExternalFunctionAccess(ResolvedExternalFunctionDefinitionRef),

    // Adding to a collection
    MapAssignment(
        Box<ResolvedSingleMutLocationExpression>,
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
    ), // Motivation: Can not use location since adding is more complex

    // Operators
    BinaryOp(ResolvedBinaryOperator),
    UnaryOp(ResolvedUnaryOperator),
    PostfixChain(Box<ResolvedExpression>, Vec<ResolvedPostfix>),

    // Conversion
    // the `?` operator. unwraps the value, unless it is none
    //NoneCoalesceOperator(Box<ResolvedExpression>, Box<ResolvedExpression>),
    CoerceOptionToBool(Box<ResolvedExpression>),

    // Calls

    // For calls from returned function values
    FunctionCall(
        FunctionTypeSignature,
        Box<ResolvedExpression>,
        Vec<ResolvedArgumentExpressionOrLocation>,
    ),

    MemberCall(ResolvedMemberCall),
    InterpolatedString(Vec<ResolvedStringPart>),

    // Constructing
    VariableDefinition(ResolvedVariableRef, Box<ResolvedMutOrImmutableExpression>), // First time assignment
    VariableReassignment(ResolvedVariableRef, Box<ResolvedMutOrImmutableExpression>),

    StructInstantiation(ResolvedStructInstantiation),
    Array(ResolvedArrayInstantiation),
    Tuple(Vec<ResolvedExpression>),
    Literal(ResolvedLiteral),
    Option(Option<Box<ResolvedExpression>>), // Wrapping an expression in `Some()`
    Range(
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
        ResolvedRangeMode,
    ),

    // Control
    ForLoop(
        ResolvedForPattern,
        ResolvedIterable,
        Box<ResolvedExpression>,
    ),
    WhileLoop(ResolvedBooleanExpression, Box<ResolvedExpression>),
    Return(Option<Box<ResolvedExpression>>),
    Break,
    Continue, //

    Block(Vec<ResolvedExpression>),

    // Match and compare
    Match(ResolvedMatch),
    Guard(Vec<ResolvedGuard>),
    If(
        ResolvedBooleanExpression,
        Box<ResolvedExpression>,
        Option<Box<ResolvedExpression>>,
    ),

    When(
        Vec<ResolvedWhenBinding>,
        Box<ResolvedExpression>,
        Option<Box<ResolvedExpression>>,
    ),

    TupleDestructuring(
        Vec<ResolvedVariableRef>,
        ResolvedTupleTypeRef,
        Box<ResolvedExpression>,
    ),

    Assignment(
        Box<ResolvedSingleMutLocationExpression>,
        Box<ResolvedExpression>,
    ),
    AssignmentSlice(
        Box<ResolvedSliceLocationExpression>,
        Box<ResolvedExpression>,
    ),
    CompoundAssignment(
        ResolvedSingleMutLocationExpression,
        ResolvedCompoundOperatorKind,
        Box<ResolvedExpression>,
    ),

    // --------------------------------------------------------------------
    // Built In members
    // --------------------------------------------------------------------

    // array built in
    ArrayExtend(ResolvedSingleMutLocationExpression, Box<ResolvedExpression>), // Extends an array with another array
    ArrayPush(ResolvedSingleMutLocationExpression, Box<ResolvedExpression>), // Adds an item to an array

    // Sparse Built in
    SparseNew(ResolvedRustTypeRef, ResolvedType), // item type
}

#[derive(Debug)]
pub struct ResolvedStringConst(pub ResolvedNode);

#[derive(Debug)]
pub enum ResolvedLiteral {
    FloatLiteral(Fp),
    NoneLiteral,
    IntLiteral(i32),
    StringLiteral(String),
    BoolLiteral(bool),

    EnumVariantLiteral(ResolvedEnumVariantTypeRef, ResolvedEnumLiteralData),
    TupleLiteral(ResolvedTupleTypeRef, Vec<ResolvedExpression>),
    Array(ResolvedArrayTypeRef, Vec<ResolvedExpression>),
    Map(
        ResolvedMapTypeRef,
        Vec<(ResolvedExpression, ResolvedExpression)>,
    ),
}

#[derive(Debug)]
pub struct ResolvedArrayInstantiation {
    pub expressions: Vec<ResolvedExpression>,
    pub item_type: ResolvedType,
    pub array_type: ResolvedType,
    pub array_type_ref: ResolvedArrayTypeRef,
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

pub type ResolvedStructTypeRef = Rc<RefCell<ResolvedStructType>>;

pub fn same_struct_ref(a: &ResolvedStructTypeRef, b: &ResolvedStructTypeRef) -> bool {
    Rc::ptr_eq(a, b)
}

pub type TypeNumber = u32;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ResolvedLocalTypeIdentifier(pub ResolvedNode);

#[derive(Debug)]
pub struct ResolvedConstant {
    pub name: ResolvedNode,
    pub assigned_name: String,
    pub id: ConstantId,
    pub expr: ResolvedExpression,
    pub resolved_type: ResolvedType,
}
pub type ResolvedConstantRef = Rc<ResolvedConstant>;

#[derive(Debug, Eq, PartialEq)]
pub struct ResolvedAliasType {
    pub name: ResolvedNode,
    pub assigned_name: String,
    pub referenced_type: ResolvedType,
}
pub type ResolvedAliasTypeRef = Rc<ResolvedAliasType>;

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

    pub fn add_external_member_function_changed(
        &mut self,
        external_func: ResolvedExternalFunctionDefinitionRef,
    ) -> Result<(), SeqMapError> {
        let name = external_func.assigned_name.clone();
        let func = ResolvedFunction::External(external_func);
        self.functions.insert(name, func.into())?;
        Ok(())
    }

    pub fn fetch_external_function_id(&self, function_name: &str) -> ExternalFunctionId {
        let resolved_function_ref = self
            .get_member_function(function_name)
            .expect("must have external function");

        match &**resolved_function_ref {
            ResolvedFunction::Internal(_internal_fn) => {
                panic!("expected external fn, but found internal")
            }
            ResolvedFunction::External(external_fn) => external_fn.id,
        }
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

pub type ResolvedOptionTypeRef = Rc<crate::ResolvedOptionType>;

#[derive(Debug)]
pub struct ResolvedOptionType {
    pub item_type: ResolvedType,
}

pub type ResolvedArrayTypeRef = Rc<ResolvedArrayType>;

pub fn same_array_ref(a: &ResolvedArrayTypeRef, b: &ResolvedArrayTypeRef) -> bool {
    Rc::ptr_eq(a, b)
}

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

#[derive(Clone, Eq, PartialEq)]
pub struct ResolvedAnonymousStructType {
    pub defined_fields: SeqMap<String, ResolvedAnonymousStructFieldType>,
}

impl Debug for ResolvedAnonymousStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{}", comma_seq(&self.defined_fields))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ResolvedEnumVariantStructType {
    pub common: ResolvedEnumVariantCommon,
    pub anon_struct: ResolvedAnonymousStructType,
}

pub type ResolvedEnumVariantTupleTypeRef = Rc<ResolvedEnumVariantTupleType>;

#[derive(Debug, Eq, PartialEq)]
pub struct ResolvedEnumVariantTupleType {
    pub common: ResolvedEnumVariantCommon,
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

#[derive(Eq, PartialEq)]
pub struct ResolvedEnumType {
    pub name: ResolvedLocalTypeIdentifier,
    pub assigned_name: String,
    pub module_path: Vec<String>,
    pub number: TypeNumber,

    pub variants: SeqMap<String, ResolvedEnumVariantTypeRef>,
}

impl Debug for ResolvedEnumType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
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

impl ResolvedEnumType {
    #[must_use]
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

    #[must_use]
    pub const fn name(&self) -> &ResolvedLocalTypeIdentifier {
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

#[derive(Eq, PartialEq, Clone)]
pub struct ResolvedEnumVariantCommon {
    pub name: ResolvedLocalTypeIdentifier,
    pub assigned_name: String,
    pub number: TypeNumber,
    pub container_index: u8,
    pub owner: ResolvedEnumTypeRef,
}

impl Debug for ResolvedEnumVariantCommon {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "<{}>{}::{}",
            self.number,
            self.owner.borrow().assigned_name,
            self.assigned_name
        )
    }
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

#[derive(Debug, Eq, PartialEq)]
pub struct ResolvedEnumVariantTupleFieldType {
    pub name: ResolvedLocalIdentifier,
    pub enum_variant: ResolvedEnumVariantTypeRef,
    pub resolved_type: ResolvedType,

    pub field_index: usize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ResolvedEnumVariantSimpleType {
    pub common: ResolvedEnumVariantCommon,
}

pub type ResolvedEnumVariantSimpleTypeRef = Rc<ResolvedEnumVariantSimpleType>;

#[derive(Clone, Eq, PartialEq)]
pub enum ResolvedEnumVariantType {
    Struct(ResolvedEnumVariantStructTypeRef),
    Tuple(ResolvedEnumVariantTupleTypeRef),
    Nothing(ResolvedEnumVariantSimpleTypeRef),
}
impl ResolvedEnumVariantType {
    pub fn common(&self) -> &ResolvedEnumVariantCommon {
        match self {
            Self::Tuple(tuple) => &tuple.common,
            Self::Struct(c) => &c.common,
            Self::Nothing(c) => &c.common,
        }
    }
}

impl Debug for ResolvedEnumVariantType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::Struct(x) => write!(f, "{{ {x:?} }}"),
            Self::Tuple(x) => write!(f, "({x:?})"),
            Self::Nothing(_x) => Ok(()),
        }
    }
}

#[derive(Debug)]
pub struct ResolvedImplMember {}

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
pub struct ResolvedMod {
    pub path: Vec<ResolvedNode>,
}

#[derive(Debug)]
pub enum ResolvedDefinition {
    StructType(ResolvedStructTypeRef),
    AliasType(ResolvedAliasTypeRef),
    EnumType(ResolvedEnumTypeRef),
    ImplType(ResolvedType),
    FunctionDef(ResolvedFunction),
    Alias(ResolvedType),
    Comment(ResolvedNode),
    Use(ResolvedUse),
    Mod(ResolvedMod),
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

#[derive()]
pub enum ResolvedEnumLiteralData {
    Nothing,
    Tuple(Vec<ResolvedExpression>),
    Struct(Vec<(usize, ResolvedExpression)>),
}

impl Debug for ResolvedEnumLiteralData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::Nothing => Ok(()),
            Self::Tuple(x) => write!(f, "{x:?}"),
            Self::Struct(s) => write!(f, "{s:?}"),
        }
    }
}
