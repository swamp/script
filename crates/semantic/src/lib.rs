/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

pub use fixed32::Fp;
use seq_fmt::comma;
use seq_map::{SeqMap, SeqMapError};
use std::cell::RefCell;
use std::cmp::PartialEq;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Clone, Hash, Eq, PartialEq, Default)]
pub struct Node {
    pub span: Span,
    pub markdown_doc: Option<Span>,
}

impl Debug for Node {
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
    #[must_use]
    pub fn end(&self) -> u32 {
        self.offset + self.length as u32
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}:{} ({})>", self.offset, self.length, self.file_id)
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct ParameterNode {
    pub name: Node,
    pub is_mutable: Option<Node>,
}

impl Debug for ParameterNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct Signature {
    pub parameters: Vec<TypeForParameter>,
    pub return_type: Box<Type>,
}

impl Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "({}) -> {}", comma(&self.parameters), self.return_type)
    }
}

impl Debug for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "({}) -> {}", comma(&self.parameters), self.return_type)
    }
}

impl Signature {
    #[must_use]
    pub fn same_type(&self, other: &Signature) -> bool {
        if self.parameters.len() != other.parameters.len()
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

pub const TYPE_NUMBER_FFI_VALUE: u32 = u32::MAX; // TODO: HACK

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct ExternalType {
    pub type_name: String, // To identify the specific Rust type
    pub number: u32,       // For type comparison
}

pub type ExternalTypeRef = Rc<ExternalType>;

#[derive(Debug, Clone)]
pub struct TypeWithMut {
    pub resolved_type: Type,
    pub is_mutable: bool,
}

#[derive(Clone, Hash, Eq, PartialEq)]
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
            self.resolved_type,
        )
    }
}

impl Debug for TypeForParameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(
            f,
            "{}{}: {:?}",
            if self.is_mutable { "mut " } else { "" },
            self.name,
            self.resolved_type,
        )
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct TypeParameterName {
    pub resolved_node: Node,
    pub assigned_name: String,
}

#[derive(Clone, Debug)]
pub enum IteratorYieldType {
    Value(Type),
    KeyValue(Type, Type),
}

#[derive(Clone, Debug)]
pub struct IteratorTypeDetails {
    pub yield_type: IteratorYieldType,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum ParameterizedTypeKind {
    Struct(StructTypeRef),
    Enum(HashableEnumTypeRef),
    Parameterized(Box<ParameterizedType>),
}

impl ParameterizedTypeKind {
    pub(crate) fn same_type(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Struct(a), Self::Struct(b)) => a.number == b.number,
            (Self::Enum(a), Self::Enum(b)) => a.0.borrow().number == b.0.borrow().number,
            (Self::Parameterized(a), Self::Parameterized(b)) => a.same_type(b),
            _ => false,
        }
    }

    pub fn name(&self) -> String {
        match self {
            Self::Struct(struct_type_ref) => struct_type_ref.assigned_name.clone(),
            Self::Enum(enum_type_ref) => enum_type_ref.0.borrow().assigned_name.clone(),
            Self::Parameterized(inner) => inner.base.name(),
        }
    }
}
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct ParameterizedType {
    pub base: ParameterizedTypeKind,
    pub parameters: Vec<Type>,
}

impl ParameterizedType {
    pub(crate) fn same_type(&self, other: &Self) -> bool {
        self.base.same_type(&other.base) && same_types(&self.parameters, &other.parameters)
    }

    pub fn name(&self) -> String {
        self.base.name()
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct TypeVariable {
    pub name: String,
    pub number: TypeNumber,
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum Type {
    // built-in Primitives
    Int,
    Float,
    String,
    Bool,
    Unit,

    Tuple(TupleTypeRef),
    Struct(StructTypeRef),
    Enum(HashableEnumTypeRef),

    Function(Signature),

    Range, // Only integers for now

    Optional(Box<Type>),

    Slice(Box<Type>),
    SlicePair(Box<Type>, Box<Type>),
    Parameterized(ParameterizedType),

    Variable(TypeVariable),

    External(ExternalTypeRef),
}

impl Type {
    #[must_use]
    pub const fn is_concrete(&self) -> bool {
        !matches!(self, Self::Parameterized(..))
    }
}
//pub type TypeRef = Rc<Type>;

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "()"),
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::Bool => write!(f, "Bool"),
            Self::String => write!(f, "String"),
            Self::Tuple(tuple_type_ref) => write!(f, "({:?})", tuple_type_ref.0),
            Self::Struct(struct_type_ref) => {
                write!(f, "{} {{ ", struct_type_ref.assigned_name)?;
                for (name, field) in &struct_type_ref.anon_struct_type.defined_fields {
                    write!(f, "{name}: {}, ", field.field_type)?;
                }
                write!(f, "}}")
            }
            Self::Enum(enum_type_ref) => {
                write!(f, "{:?}", enum_type_ref.0.borrow().assigned_name)?;

                for (name, variant) in &enum_type_ref.0.borrow().variants {
                    write!(f, "{name}: {variant:?}, ")?;
                }
                write!(f, "}}")
            }
            Self::Function(function_type_signature) => {
                write!(f, "{function_type_signature:?}",)
            }
            Self::Optional(base_type) => write!(f, "{base_type:?}?"),
            Self::External(external_type) => write!(
                f,
                "External({}<0x{:04X}>)",
                external_type.type_name, external_type.number
            ),
            Self::Range => write!(f, "Range"),
            Self::Slice(_) => write!(f, "Slice"),
            Self::SlicePair(_, _) => write!(f, "SlicePair"),
            Self::Parameterized(ty) => write!(f, "Parameterized {ty:?}"),
            Self::Variable(var) => write!(f, "Variable {var:?}"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Self::Int => write!(f, "Int"),
            Self::Float => write!(f, "Float"),
            Self::Bool => write!(f, "Bool"),
            Self::String => write!(f, "String"),
            Self::Unit => write!(f, "()"),
            Self::Tuple(tuple) => write!(f, "({})", comma(&tuple.0)),
            Self::Struct(struct_ref) => write!(f, "{}", struct_ref.assigned_name),
            Self::Enum(enum_type) => write!(f, "{}", enum_type.0.borrow().assigned_name),
            Self::Function(signature) => write!(f, "function {signature}"),
            Self::Optional(base_type) => write!(f, "{base_type}?"),
            Self::External(external_type) => write!(f, "ExternalType<{}>", external_type.type_name),
            Self::Range => write!(f, "Range"),
            Self::Slice(_) => write!(f, "Slice"),
            Self::SlicePair(_, _) => write!(f, "SlicePair"),
            Self::Parameterized(_) => write!(f, "Parameterized"),
            Self::Variable(_) => write!(f, "Var"),
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
    UnknownGeneric,
    UnknownLookupModule,
    UnknownIntrinsic,
    UnknownImplOnType,
    DuplicateSymbolName,
    DuplicateNamespaceLink(String),
    DuplicateGenericType(String),
    UnknownTypeVariable,
    WrongParameterCount,
}

impl Type {
    pub fn expect_struct_type(&self) -> Result<StructTypeRef, SemanticError> {
        match self {
            Type::Struct(struct_type_ref) => Ok(struct_type_ref.clone()),
            _ => Err(SemanticError::ResolveNotStruct),
        }
    }

    pub fn assignable_type(&self, other: &Type) -> bool {
        if self.same_type(other) {
            true
        } else if let Self::Optional(inner_type) = self {
            inner_type.same_type(other)
        } else {
            false
        }
    }
    pub fn same_type(&self, other: &Type) -> bool {
        if let Self::External(some_struct) = other {
            if some_struct.number == TYPE_NUMBER_FFI_VALUE {
                return true;
            }
        }

        if let Self::External(some_struct) = self {
            if some_struct.number == TYPE_NUMBER_FFI_VALUE {
                return true;
            }
        }

        match (self, other) {
            // Intrinsic Types
            (Self::String, Self::String) => true,
            (Self::Int, Self::Int) => true,
            (Self::Float, Self::Float) => true,
            (Self::Bool, Self::Bool) => true,
            (Self::Unit, Self::Unit) => true,

            // Parameterized Intrinsic
            (Self::Slice(a), Self::Slice(b)) => a.same_type(b),
            (Self::SlicePair(a1, a2), Self::SlicePair(b1, b2)) => {
                a1.same_type(b1) && a2.same_type(b2)
            }

            (Self::Parameterized(a), Self::Parameterized(b)) => a.same_type(b),

            (Self::Function(a), Self::Function(b)) => a.same_type(b),
            (Self::Struct(a), Self::Struct(b)) => compare_struct_types(a, b),
            (Self::Tuple(a), Self::Tuple(b)) => {
                if a.0.len() != b.0.len() {
                    return false;
                }
                a.0.iter().zip(b.0.iter()).all(|(a, b)| a.same_type(b))
            }
            (Self::Enum(_), Self::Enum(_)) => true,
            (Self::Optional(inner_type_a), Self::Optional(inner_type_b)) => {
                inner_type_a.same_type(inner_type_b)
            }
            (Self::External(type_ref_a), Self::External(type_ref_b)) => {
                type_ref_a.number == type_ref_b.number
            }

            _ => false,
        }
    }
}

pub fn same_types(a: &[Type], b: &[Type]) -> bool {
    if a.len() != b.len() {
        return false;
    }

    for (a, b) in a.iter().zip(b) {
        if !a.same_type(b) {
            return false;
        }
    }

    true
}

fn compare_struct_types(a: &StructTypeRef, b: &StructTypeRef) -> bool {
    let a_borrow = a;
    let b_borrow = b;
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

impl Node {
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
pub struct LocalIdentifier(pub Node);

//#[derive(Debug)]
pub struct InternalFunctionDefinition {
    pub body: Expression,
    pub name: LocalIdentifier,
    pub signature: Signature,
}

impl Debug for InternalFunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}\n{:?}", self.signature, self.body)
    }
}

impl PartialEq<Self> for InternalFunctionDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for InternalFunctionDefinition {}

pub type InternalFunctionDefinitionRef = Rc<InternalFunctionDefinition>;

pub type ExternalFunctionId = u32;

pub type ConstantId = u32;

pub struct ExternalFunctionDefinition {
    pub name: Option<Node>,
    pub assigned_name: String,
    pub signature: Signature,
    pub id: ExternalFunctionId,
}

impl PartialEq<Self> for ExternalFunctionDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for ExternalFunctionDefinition {}

impl Debug for ExternalFunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "external fn")
    }
}

pub type ExternalFunctionDefinitionRef = Rc<crate::ExternalFunctionDefinition>;

//#[derive(Debug)]
#[derive(Clone)]
pub struct IntrinsicFunctionDefinition {
    pub name: String,
    pub signature: Signature,
    pub intrinsic: IntrinsicFunction,
}

impl Debug for IntrinsicFunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}\n{:?}", self.intrinsic, self.signature)
    }
}

impl PartialEq<Self> for IntrinsicFunctionDefinition {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for IntrinsicFunctionDefinition {}

pub type IntrinsicFunctionDefinitionRef = Rc<IntrinsicFunctionDefinition>;

#[derive(Debug)]
pub struct Variable {
    pub name: Node,
    pub resolved_type: Type,
    pub mutable_node: Option<Node>,

    pub scope_index: usize,
    pub variable_index: usize,
}

impl Variable {
    #[must_use]
    pub const fn is_mutable(&self) -> bool {
        self.mutable_node.is_some()
    }
}

pub type VariableRef = Rc<Variable>;

#[derive(Debug)]
pub enum BinaryOperatorKind {
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
pub struct BinaryOperator {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub kind: BinaryOperatorKind,
    pub node: Node,
}

#[derive(Debug)]
pub enum UnaryOperatorKind {
    Not,
    Negate,
}
#[derive(Debug)]
pub struct UnaryOperator {
    pub left: Box<Expression>,
    pub kind: UnaryOperatorKind,
    pub node: Node,
}

#[derive()]
pub struct InternalFunctionCall {
    pub arguments: Vec<ArgumentExpressionOrLocation>,

    pub function_definition: InternalFunctionDefinitionRef,
    pub function_expression: Box<Expression>,
}

impl Debug for InternalFunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "InFuncCall({:?} {:?})",
            self.function_expression, self.arguments
        )
    }
}

#[derive(Debug)]
pub struct ExternalFunctionCall {
    pub arguments: Vec<ArgumentExpressionOrLocation>,
    pub function_definition: ExternalFunctionDefinitionRef,
    pub function_expression: Box<Expression>,
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

#[derive(Debug)]
pub struct MemberCall {
    pub function: FunctionRef,
    pub arguments: Vec<ArgumentExpressionOrLocation>,
}

#[derive(Debug, Hash, Clone, Eq, PartialEq)]
pub struct StructTypeField {
    pub identifier: Option<Node>,
    pub field_type: Type,
}

impl Display for StructTypeField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{:?}:{}", self.identifier, self.field_type)
    }
}

#[derive(Debug)]
pub struct ArrayItem {
    pub item_type: Type,
    pub int_expression: Expression,
    pub array_expression: Expression,
    pub array_type: Type,
}

pub type ArrayItemRef = Rc<ArrayItem>;

#[derive(Debug)]
pub enum PrecisionType {
    Float,
    String,
}

#[derive(Debug)]
pub enum FormatSpecifierKind {
    LowerHex,                            // :x
    UpperHex,                            // :X
    Binary,                              // :b
    Float,                               // :f
    Precision(u32, Node, PrecisionType), // :..2f or :..5s
}

#[derive(Debug)]
pub struct FormatSpecifier {
    pub node: Node,
    pub kind: FormatSpecifierKind,
}

#[derive(Debug)]
pub enum StringPart {
    Literal(Node, String),
    Interpolation(Expression, Option<FormatSpecifier>),
}

pub type FunctionRef = Rc<Function>;

#[derive(Debug, Eq, PartialEq)]
pub enum Function {
    Internal(InternalFunctionDefinitionRef),
    External(ExternalFunctionDefinitionRef),
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Internal(_int) => write!(f, "")?,
            Self::External(_) => write!(f, "external ")?,
        };

        write!(f, "{}", self.signature())
    }
}

impl Function {
    #[must_use]
    pub fn name(&self) -> Option<&Node> {
        match self {
            Self::Internal(x) => Some(&x.name.0),
            Self::External(y) => y.name.as_ref(),
        }
    }

    #[must_use]
    pub fn node(&self) -> Node {
        match self {
            Self::Internal(x) => x.name.0.clone(),
            Self::External(_y) => Node::new_unknown(),
        }
    }

    #[must_use]
    pub fn signature(&self) -> &Signature {
        match self {
            Self::Internal(internal) => &internal.signature,
            Self::External(external) => &external.signature,
        }
    }
}

#[derive(Debug)]
pub struct BooleanExpression {
    #[allow(unused)]
    pub expression: Box<Expression>,
}

#[derive(Debug)]
pub struct Match {
    pub arms: Vec<MatchArm>,
    pub expression: Box<Expression>,
}

#[derive(Debug)]
pub struct MatchArm {
    #[allow(unused)]
    pub pattern: Pattern,
    pub expression: Box<Expression>,
    pub expression_type: Type,
}

#[derive(Debug)]
pub enum Pattern {
    Normal(NormalPattern, Option<BooleanExpression>),
    Wildcard(Node),
}

#[derive(Debug)]
pub enum NormalPattern {
    PatternList(Vec<PatternElement>),
    EnumPattern(EnumVariantTypeRef, Option<Vec<PatternElement>>),
    Literal(Literal),
}

#[derive(Debug)]
pub enum PatternElement {
    Variable(VariableRef),
    VariableWithFieldIndex(VariableRef, usize),
    Wildcard(Node),
}

#[derive(Debug)]
pub struct Iterable {
    pub key_type: Option<Type>, // It does not have to support a key type
    pub value_type: Type,

    pub resolved_expression: Box<MutOrImmutableExpression>,
}

#[derive(Debug)]
pub struct StructInstantiation {
    pub source_order_expressions: Vec<(usize, Expression)>,
    pub struct_type_ref: StructTypeRef,
}

#[derive(Debug, Eq, PartialEq)]
pub enum CompoundOperatorKind {
    Add,
    Sub,
    Mul,
    Div,
    Modulo,
}

#[derive(Debug)]
pub struct CompoundOperator {
    pub node: Node,
    pub kind: CompoundOperatorKind,
}

#[must_use]
pub fn create_rust_type(name: &str, type_number: TypeNumber) -> ExternalTypeRef {
    let rust_type = ExternalType {
        type_name: name.to_string(),
        number: type_number,
    };
    Rc::new(rust_type)
}

#[derive(Debug)]
pub struct Guard {
    pub condition: Option<BooleanExpression>,
    pub result: Expression,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RangeMode {
    Inclusive,
    Exclusive,
}

#[derive(Debug)]
pub struct Postfix {
    pub node: Node,
    pub ty: Type,
    pub kind: PostfixKind,
}

#[derive(Debug)]
pub struct Range {
    pub min: Expression,
    pub max: Expression,
    pub mode: RangeMode,
}

#[derive(Debug)]
pub enum PostfixKind {
    StructField(StructTypeRef, usize),
    ArrayIndex(ArrayTypeRef, Expression),
    ArrayRangeIndex(ArrayTypeRef, Range),
    StringIndex(Expression),
    StringRangeIndex(Range),
    MapIndex(MapTypeRef, Expression),

    RustTypeIndexRef(ExternalTypeRef, Expression), // TODO: Remove this

    MemberCall(FunctionRef, Vec<ArgumentExpressionOrLocation>),
    FunctionCall(Vec<ArgumentExpressionOrLocation>),

    OptionUnwrap, // ? operator
    NoneCoalesce(Expression),
}

#[derive(Debug)]
pub enum LocationAccessKind {
    FieldIndex(StructTypeRef, usize),
    ArrayIndex(ArrayTypeRef, Expression),
    ArrayRange(ArrayTypeRef, Range),
    StringIndex(Expression),
    StringRange(Range),
    MapIndex(MapTypeRef, Expression),
    MapIndexInsertIfNonExisting(MapTypeRef, Expression),
    RustTypeIndex(ExternalTypeRef, Expression),
}

#[derive(Debug)]
pub struct LocationAccess {
    pub node: Node,
    pub ty: Type,
    pub kind: LocationAccessKind,
}

#[derive(Debug)]
pub struct SingleLocationExpression {
    pub kind: SingleLocationExpressionKind,
    pub node: Node,
    pub ty: Type,

    pub starting_variable: VariableRef,
    pub access_chain: Vec<LocationAccess>,
}

#[derive(Debug)]
pub struct SingleMutLocationExpression(pub SingleLocationExpression);

#[derive(Debug)]
pub enum SingleLocationExpressionKind {
    MutVariableRef,
    MutStructFieldRef(StructTypeRef, usize),
    MutArrayIndexRef(ArrayTypeRef),
    MutMapIndexRef(MapTypeRef),
    MutRustTypeIndexRef(ExternalTypeRef),
}

#[derive(Debug)]
pub struct SliceLocationExpression {
    pub start: Box<Expression>,
    pub range_start: Box<Expression>,
    pub range_end: Box<Expression>,
    pub mode: RangeMode,
    pub ty: Type,
}

#[derive(Debug)]
pub struct MutOrImmutableExpression {
    pub expression_or_location: ArgumentExpressionOrLocation,
    pub is_mutable: Option<Node>,
}

impl MutOrImmutableExpression {}

impl MutOrImmutableExpression {
    pub fn expect_immutable(self) -> Result<Expression, SemanticError> {
        match self.expression_or_location {
            ArgumentExpressionOrLocation::Expression(expr) => Ok(expr),
            ArgumentExpressionOrLocation::Location(_) => Err(SemanticError::WasNotImmutable),
        }
    }

    pub fn expect_immutable_ref(&self) -> Result<&Expression, SemanticError> {
        match &self.expression_or_location {
            ArgumentExpressionOrLocation::Expression(expr) => Ok(expr),
            ArgumentExpressionOrLocation::Location(_) => Err(SemanticError::WasNotImmutable),
        }
    }

    pub fn ty(&self) -> &Type {
        match &self.expression_or_location {
            ArgumentExpressionOrLocation::Expression(expr) => &expr.ty,
            ArgumentExpressionOrLocation::Location(loc) => &loc.ty,
        }
    }
}

#[derive(Debug)]
pub enum ArgumentExpressionOrLocation {
    Expression(Expression),
    Location(SingleLocationExpression),
}

#[derive()]
pub struct Expression {
    pub ty: Type,
    pub node: Node,
    pub kind: ExpressionKind,
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{:?}{},{:?}", self.node, self.ty, self.kind)
    }
}

#[derive(Debug)]
pub struct WhenBinding {
    pub variable: VariableRef,
    pub expr: MutOrImmutableExpression,
}

#[derive(Debug, Clone)]
pub enum IntrinsicFunction {
    FloatRound,
    FloatFloor,
    FloatSqrt,
    FloatSign,
    FloatAbs,
    FloatRnd,
    FloatCos,
    FloatSin,
    FloatAcos,
    FloatAsin,
    FloatAtan2,
    FloatMin,
    FloatMax,
    FloatClamp,

    // Int
    IntAbs,
    IntRnd,
    IntMax,
    IntMin,
    IntToFloat,

    // String
    StringLen,

    // Vec
    VecFromSlice,
    VecPush,
    VecRemove,
    VecClear,
    VecCreate,

    // Map
    MapCreate,
    MapFromSlice,
    MapHas,
    MapRemove,

    Float2Magnitude,
    VecSubscript,
    VecSubscriptMut,
    VecIter,
    VecIterMut,
    MapIter,
    MapIterMut,
    MapSubscript,
    MapSubscriptMut,
}

impl fmt::Display for IntrinsicFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = match self {
            // Float
            Self::FloatRound => "float_round",
            Self::FloatFloor => "float_floor",
            Self::FloatSqrt => "float_sqrt",
            Self::FloatSign => "float_sign",
            Self::FloatAbs => "float_abs",
            Self::FloatRnd => "float_rnd",
            Self::FloatCos => "float_cos",
            Self::FloatSin => "float_sin",
            Self::FloatAcos => "float_acos",
            Self::FloatAsin => "float_asin",
            Self::FloatAtan2 => "float_atan2",
            Self::FloatMin => "float_min",
            Self::FloatMax => "float_max",
            Self::FloatClamp => "float_clamp",

            // Int
            Self::IntAbs => "int_abs",
            Self::IntRnd => "int_rnd",
            Self::IntMax => "int_max",
            Self::IntMin => "int_min",
            Self::IntToFloat => "int_to_float",

            // String
            Self::StringLen => "string_len",

            // Vec
            Self::VecFromSlice => "vec_from_slice",
            Self::VecPush => "vec_push",
            Self::VecRemove => "vec_remove",
            Self::VecClear => "vec_clear",
            Self::VecCreate => "vec_create",
            Self::VecSubscriptMut => "vec_subscript_mut",
            Self::VecSubscript => "vec_subscript",
            Self::VecIter => "vec_iter",
            Self::VecIterMut => "vec_iter_mut",

            // Map
            Self::MapCreate => "map_create",
            Self::MapFromSlice => "map_from_slice",
            Self::MapHas => "map_has",
            Self::MapRemove => "map_remove",
            Self::MapSubscriptMut => "map_subscript_mut",
            Self::MapSubscript => "map_subscript",
            Self::MapIter => "map_iter",
            Self::MapIterMut => "map_iter_mut",

            // Other
            Self::Float2Magnitude => "float2_magnitude",
        };

        write!(f, "{name}")
    }
}

impl TryFrom<&str> for IntrinsicFunction {
    type Error = SemanticError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let x = match value {
            "float_floor" => Self::FloatFloor,
            "float_sqrt" => Self::FloatSqrt,
            "float_round" => Self::FloatRound,
            _ => return Err(SemanticError::UnknownIntrinsic),
        };

        Ok(x)
    }
}

#[derive(Debug)]
pub enum ExpressionKind {
    // Access Lookup values
    ConstantAccess(ConstantRef),
    VariableAccess(VariableRef),
    IntrinsicFunctionAccess(IntrinsicFunctionDefinitionRef),
    InternalFunctionAccess(InternalFunctionDefinitionRef),
    ExternalFunctionAccess(ExternalFunctionDefinitionRef),

    PostfixChain(Box<Expression>, Vec<Postfix>),

    // Operators
    BinaryOp(BinaryOperator),
    UnaryOp(UnaryOperator),

    // Conversion
    // the `?` operator. unwraps the value, unless it is none
    //NoneCoalesceOperator(Box<Expression>, Box<Expression>),
    CoerceOptionToBool(Box<Expression>),

    InterpolatedString(Vec<StringPart>),

    // Constructing

    // Literals
    StructInstantiation(StructInstantiation), // TODO: Should move to Literal
    Literal(Literal),

    Option(Option<Box<Expression>>), // Wrapping an expression in `Some()`
    Range(Box<Expression>, Box<Expression>, RangeMode),

    // Control
    ForLoop(ForPattern, Iterable, Box<Expression>),
    WhileLoop(BooleanExpression, Box<Expression>),
    Return(Option<Box<Expression>>),
    Break,
    Continue, //

    Block(Vec<Expression>),

    // Match and compare
    Match(Match),
    Guard(Vec<Guard>),
    If(BooleanExpression, Box<Expression>, Option<Box<Expression>>),
    When(Vec<WhenBinding>, Box<Expression>, Option<Box<Expression>>),

    // Variable definition and assignment
    VariableDefinition(VariableRef, Box<MutOrImmutableExpression>), // First time assignment
    VariableReassignment(VariableRef, Box<MutOrImmutableExpression>),
    TupleDestructuring(Vec<VariableRef>, TupleTypeRef, Box<Expression>),
    Assignment(Box<SingleMutLocationExpression>, Box<Expression>),
    AssignmentSlice(Box<SliceLocationExpression>, Box<Expression>),
    CompoundAssignment(
        SingleMutLocationExpression,
        CompoundOperatorKind,
        Box<Expression>,
    ),

    // --------------------------------------------------------------------
    // Built In members
    // --------------------------------------------------------------------

    // array built in
    ArrayExtend(SingleMutLocationExpression, Box<Expression>), // Extends an array with another array. TODO: probably intrinsic
    ArrayPush(SingleMutLocationExpression, Box<Expression>), // Adds an item to an array TODO: probably intrinsic

    // To create rust types
    RustValueInstantiation(ExternalTypeRef, Type), // type parameter (item type) // TODO: Remove this
    IntrinsicCall(IntrinsicFunction, Vec<ArgumentExpressionOrLocation>),
}

#[derive(Debug)]
pub enum Literal {
    FloatLiteral(Fp),
    NoneLiteral,
    IntLiteral(i32),
    StringLiteral(String),
    BoolLiteral(bool),

    EnumVariantLiteral(EnumVariantTypeRef, EnumLiteralData),
    TupleLiteral(TupleTypeRef, Vec<Expression>),
    Array(ArrayTypeRef, Vec<Expression>),
    Map(MapTypeRef, Vec<(Expression, Expression)>),
}

#[derive(Debug)]
pub struct ArrayInstantiation {
    pub expressions: Vec<Expression>,
    pub item_type: Type,
    pub array_type: Type,
    pub array_type_ref: ArrayTypeRef,
}

#[derive(Debug)]
pub enum ForPattern {
    Single(VariableRef),
    Pair(VariableRef, VariableRef),
}

impl ForPattern {
    #[must_use]
    pub fn is_mutable(&self) -> bool {
        match self {
            Self::Single(variable) => variable.is_mutable(),
            Self::Pair(a, b) => a.is_mutable() || b.is_mutable(),
        }
    }
}

impl Display for ForPattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "resolved_for_pattern")
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ModulePathItem(pub Node);

pub type StructTypeRef = Rc<StructType>;

pub fn same_struct_ref(a: &StructTypeRef, b: &StructTypeRef) -> bool {
    Rc::ptr_eq(a, b)
}

pub type TypeNumber = u32;

#[derive(Debug, Hash, Clone, Eq, PartialEq)]
pub struct LocalTypeIdentifier(pub Node);

#[derive(Debug)]
pub struct Constant {
    pub name: Node,
    pub assigned_name: String,
    pub id: ConstantId,
    pub expr: Expression,
    pub resolved_type: Type,
}
pub type ConstantRef = Rc<Constant>;

#[derive(Debug)]
pub struct AliasType {
    pub name: Node,
    pub assigned_name: String,
    pub referenced_type: Type,
}
pub type AliasTypeRef = Rc<AliasType>;

/*
pub struct TypeCreator {
    pub arguments: Vec<Type>,
    pub generator_id: usize,
}

 */

#[derive(Debug)]
pub struct Impl {
    pub functions: SeqMap<String, FunctionRef>,
}

#[derive(Debug)]
pub struct AssociatedImpls {
    pub functions: SeqMap<Type, Impl>,
}

impl AssociatedImpls {}

impl Default for AssociatedImpls {
    fn default() -> Self {
        Self::new()
    }
}

impl AssociatedImpls {
    #[must_use]
    pub fn new() -> AssociatedImpls {
        Self {
            functions: SeqMap::default(),
        }
    }
}

impl AssociatedImpls {
    pub fn prepare(&mut self, ty: &Type) {
        self.functions
            .insert(
                ty.clone(),
                Impl {
                    functions: SeqMap::default(),
                },
            )
            .expect("should work");
    }
    #[must_use]
    pub fn get_member_function(&self, ty: &Type, function_name: &str) -> Option<&FunctionRef> {
        let maybe_found_impl = self.functions.get(&ty);
        if let Some(found_impl) = maybe_found_impl {
            if let Some(func) = found_impl.functions.get(&function_name.to_string()) {
                return Some(func);
            }
        }
        None
    }

    pub fn add_member_function(
        &mut self,
        ty: &Type,
        name: &str,
        func: FunctionRef,
    ) -> Result<(), SemanticError> {
        let maybe_found_impl = self.functions.get_mut(&ty);
        if let Some(found_impl) = maybe_found_impl {
            found_impl
                .functions
                .insert(name.to_string(), func)
                .expect("todo");
            Ok(())
        } else {
            Err(SemanticError::UnknownImplOnType)
        }
    }

    #[must_use]
    pub fn get_internal_member_function(
        &self,
        ty: &Type,
        function_name: &str,
    ) -> Option<InternalFunctionDefinitionRef> {
        if let Some(func) = self.get_member_function(ty, function_name) {
            match &**func {
                Function::Internal(fn_def) => Some(fn_def.clone()),
                Function::External(_) => None,
            };
        }
        None
    }

    #[must_use]
    pub fn fetch_external_function_id(
        &self,
        ty: &Type,
        function_name: &str,
    ) -> Option<ExternalFunctionId> {
        if let Some(func) = self.get_member_function(ty, function_name) {
            match &**func {
                Function::External(fn_def) => Some(fn_def.clone()),
                Function::Internal(_) => None,
            };
        }
        None
    }
}

#[derive(Hash, Eq, PartialEq)]
pub struct StructType {
    pub name: Node,
    pub assigned_name: String,
    pub anon_struct_type: AnonymousStructType,
    pub number: TypeNumber,
}

impl Debug for StructType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "struct {:?}", self.assigned_name)
    }
}

impl StructType {
    #[must_use]
    pub fn new(
        name: Node,
        assigned_name: &str,
        type_number: TypeNumber,
        anon_struct_type: AnonymousStructType,
    ) -> Self {
        Self {
            anon_struct_type,
            number: type_number,
            name,
            assigned_name: assigned_name.to_string(),
        }
    }

    pub fn field_index(&self, field_name: &str) -> Option<usize> {
        self.anon_struct_type
            .defined_fields
            .get_index(&field_name.to_string())
    }

    #[must_use]
    pub fn name(&self) -> &Node {
        &self.name
    }
}

pub type OptionTypeRef = Rc<OptionType>;

#[derive(Debug)]
pub struct OptionType {
    pub item_type: Type,
}

pub type ArrayTypeRef = Rc<ArrayType>;

#[must_use]
pub fn same_array_ref(a: &ArrayTypeRef, b: &ArrayTypeRef) -> bool {
    Rc::ptr_eq(a, b)
}

#[derive(Debug)]
pub struct ArrayType {
    pub item_type: Type,
}

pub type MapTypeRef = Rc<MapType>;

#[derive(Debug)]
pub struct MapType {
    pub key_type: Type,
    pub value_type: Type,
}

pub type EnumVariantStructTypeRef = Rc<EnumVariantStructType>;

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct AnonymousStructType {
    pub defined_fields: SeqMap<String, StructTypeField>,
}

impl Debug for AnonymousStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{}", comma_seq(&self.defined_fields))
    }
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct EnumVariantStructType {
    pub common: EnumVariantCommon,
    pub anon_struct: AnonymousStructType,
}

pub type EnumVariantTupleTypeRef = Rc<EnumVariantTupleType>;

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct EnumVariantTupleType {
    pub common: EnumVariantCommon,
    pub fields_in_order: Vec<Type>,
}

pub type TupleTypeRef = Rc<TupleType>;

#[derive(Debug, Hash, Eq, PartialEq)]
pub struct TupleType(pub Vec<Type>);

impl TupleType {
    #[must_use]
    pub fn new(types: Vec<Type>) -> Self {
        Self(types)
    }
}

pub type EnumTypeRef = Rc<RefCell<EnumType>>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct HashableEnumTypeRef(pub Rc<RefCell<EnumType>>);

impl Hash for HashableEnumTypeRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u32(self.0.borrow().number)
    }
}

#[derive(Hash, Eq, PartialEq)]
pub struct EnumType {
    pub name: LocalTypeIdentifier,
    pub assigned_name: String,
    pub number: TypeNumber,

    pub variants: SeqMap<String, EnumVariantTypeRef>,
}

impl Debug for EnumType {
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

impl EnumType {
    #[must_use]
    pub fn new(name: LocalTypeIdentifier, assigned_name: &str, number: TypeNumber) -> Self {
        Self {
            name,
            assigned_name: assigned_name.to_string(),
            number,
            variants: SeqMap::new(),
        }
    }

    #[must_use]
    pub const fn name(&self) -> &LocalTypeIdentifier {
        &self.name
    }

    pub fn get_variant(&self, name: &str) -> Option<&EnumVariantTypeRef> {
        self.variants.get(&name.to_string())
    }

    pub fn get_variant_from_index(&self, index: usize) -> Option<&EnumVariantTypeRef> {
        Some(self.variants.values().collect::<Vec<_>>()[index])
    }
}

pub type EnumVariantTypeRef = Rc<EnumVariantType>;

#[derive(Clone, Eq, PartialEq)]
pub struct EnumVariantCommon {
    pub name: LocalTypeIdentifier,
    pub assigned_name: String,
    pub number: TypeNumber,
    pub container_index: u8,
    pub owner: EnumTypeRef,
}

impl Hash for EnumVariantCommon {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u8(self.container_index);
        state.write_u32(self.owner.borrow().number);
    }
}

impl Debug for EnumVariantCommon {
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

pub type EnumVariantStructFieldTypeRef = Rc<EnumVariantStructFieldType>;

#[derive(Debug)]
pub struct EnumVariantStructFieldType {
    pub name: LocalIdentifier,
    pub enum_variant: EnumVariantTypeRef,
    pub resolved_type: Type,

    pub field_index: usize,
}

pub type EnumVariantTupleFieldTypeRef = Rc<EnumVariantTupleFieldType>;

#[derive(Debug)]
pub struct EnumVariantTupleFieldType {
    pub name: LocalIdentifier,
    pub enum_variant: EnumVariantTypeRef,
    pub resolved_type: Type,

    pub field_index: usize,
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct EnumVariantSimpleType {
    pub common: EnumVariantCommon,
}

pub type EnumVariantSimpleTypeRef = Rc<EnumVariantSimpleType>;

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum EnumVariantType {
    Struct(EnumVariantStructTypeRef),
    Tuple(EnumVariantTupleTypeRef),
    Nothing(EnumVariantSimpleTypeRef),
}
impl EnumVariantType {
    pub fn common(&self) -> &EnumVariantCommon {
        match self {
            Self::Tuple(tuple) => &tuple.common,
            Self::Struct(c) => &c.common,
            Self::Nothing(c) => &c.common,
        }
    }
}

impl Debug for EnumVariantType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::Struct(x) => write!(f, "{{ {x:?} }}"),
            Self::Tuple(x) => write!(f, "({x:?})"),
            Self::Nothing(_x) => Ok(()),
        }
    }
}

#[derive(Debug)]
pub struct ImplMember {}

#[derive(Debug)]
pub enum UseItem {
    Identifier(Node),
    TypeIdentifier(Node),
}

/*
#[derive(Debug)]
pub enum Definition {
    StructType(StructTypeRef),
    GenericType, // TODO: Remove this
    AliasType(AliasTypeRef),
    EnumType(EnumTypeRef),
    ImplType(Type),
    FunctionDef(Function),
    Alias(Type),
    Comment(Node),
    Use(Use),
    Mod(Mod),
    Constant(Node, ConstantRef),
}
*/

#[derive(Debug, Clone)]
pub struct MonomorphizationCache {
    pub cache: SeqMap<String, Type>,
}

impl MonomorphizationCache {
    pub fn new() -> Self {
        Self {
            cache: SeqMap::default(),
        }
    }

    pub fn complete_name(path: &[String], base_name: &str, argument_types: &[Type]) -> String {
        format!(
            "{}::{}<{}>",
            path.join("::"),
            base_name,
            comma(argument_types)
        )
    }
    pub fn add(
        &mut self,
        path: &[String],
        name: &str,
        ty: Type,
        argument_type: &[Type],
    ) -> Result<(), SeqMapError> {
        let name = Self::complete_name(path, name, argument_type);
        self.cache.insert(name, ty)
    }

    pub fn is_empty(&self) -> bool {
        self.cache.is_empty()
    }

    pub fn get(&self, path: &[String], base_name: &str, argument_type: &[Type]) -> Option<&Type> {
        let name = Self::complete_name(path, base_name, argument_type);
        self.cache.get(&name)
    }
}

// Mutable part
#[derive(Debug)]
pub struct ProgramState {
    pub array_types: Vec<ArrayTypeRef>,
    pub number: TypeNumber,
    pub external_function_number: ExternalFunctionId,
    pub monomorphization_cache: MonomorphizationCache,
    pub associated_impls: AssociatedImpls,
}

impl Default for ProgramState {
    fn default() -> Self {
        Self::new()
    }
}

impl ProgramState {
    #[must_use]
    pub fn new() -> Self {
        Self {
            array_types: Vec::new(),
            number: 16,
            external_function_number: 0,
            monomorphization_cache: MonomorphizationCache::new(),
            associated_impls: AssociatedImpls::new(),
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
pub enum EnumLiteralData {
    Nothing,
    Tuple(Vec<Expression>),
    Struct(Vec<(usize, Expression)>),
}

impl Debug for EnumLiteralData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match self {
            Self::Nothing => Ok(()),
            Self::Tuple(x) => write!(f, "{x:?}"),
            Self::Struct(s) => write!(f, "{s:?}"),
        }
    }
}
