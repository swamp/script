/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod modules;
pub mod ns;
pub mod prelude;
pub mod symtbl;

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
pub struct Node {
    pub span: Span,
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Signature {
    pub parameters: Vec<TypeForParameter>,
    pub return_type: Box<Type>,
}

impl Display for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "({})->{}", comma(&self.parameters), self.return_type)
    }
}

impl Signature {
    pub fn same_type(&self, other: &Signature) -> bool {
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

#[derive(Debug, Clone)]
pub struct TypeForParameter {
    pub name: String,
    pub resolved_type: Option<Type>,
    pub is_mutable: bool,
    pub node: Option<ParameterNode>,
}

impl Display for TypeForParameter {
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

impl Eq for TypeForParameter {}

impl PartialEq for TypeForParameter {
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
pub enum Type {
    // Primitives
    Int,
    Float,
    String,
    Bool,
    Unit,

    // Containers
    Array(ArrayTypeRef),
    Tuple(TupleTypeRef),
    Struct(StructTypeRef),
    Map(MapTypeRef),

    Enum(EnumTypeRef),
    Generic(Box<Type>, Vec<Type>),

    Function(Signature),
    Iterable(Box<Type>),

    Optional(Box<Type>),
    External(ExternalTypeRef),
}

impl Debug for Type {
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
            Self::External(rust_type) => write!(f, "{:?}?", rust_type.type_name),
        }
    }
}

impl Display for Type {
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
            Self::External(rust_type) => write!(f, "RustType {}", rust_type.type_name),
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
    DuplicateSymbolName,
    DuplicateNamespaceLink(String),
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
            (Self::External(type_ref_a), Self::External(type_ref_b)) => {
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

fn compare_struct_types(a: &StructTypeRef, b: &StructTypeRef) -> bool {
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

impl Node {
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
pub struct MutVariable {
    pub variable_ref: VariableRef,
}

//type MutVariableRef = Rc<MutVariable>;

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
pub struct MemberCall {
    pub function: FunctionRef,
    pub arguments: Vec<ArgumentExpressionOrLocation>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
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
pub struct MapIndexLookup {
    pub map_type: Type,
    pub item_type: Type,
    pub map_type_ref: MapTypeRef,
    pub index_expression: Box<Expression>,
    pub map_expression: Box<Expression>,
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

#[derive(Debug)]
pub struct VariableCompoundAssignment {
    pub variable_ref: VariableRef, // compound only support single variable
    pub expression: Box<Expression>,
    pub compound_operator: CompoundOperator,
}

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
    ExternalTypeIndexRef(ExternalTypeRef, Expression),
    MemberCall(FunctionRef, Vec<ArgumentExpressionOrLocation>),
    FunctionCall(Vec<ArgumentExpressionOrLocation>),
    OptionUnwrap, // ? operator
    NoneCoalesce(Expression),

    // --- sparse built in
    // TODO: Have a better interface for these "engine" member calls
    SparseAdd(Box<Expression>),
    SparseRemove(Box<Expression>),
    SparseAccess(Box<Expression>),

    ArrayRemoveIndex(Box<Expression>),
    ArrayAdd(Box<Expression>),
    ArrayClear,

    // Map built in
    MapRemove(Box<Expression>, MapTypeRef),
    MapHas(Box<Expression>),

    // Integer built in
    IntAbs,
    IntRnd,
    IntToFloat,
    IntClamp(Box<Expression>, Box<Expression>),
    IntMin(Box<Expression>),
    IntMax(Box<Expression>),

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
    FloatAtan2(Box<Expression>),
    FloatSqrt,
    FloatClamp(Box<Expression>, Box<Expression>),
    FloatMin(Box<Expression>),
    FloatMax(Box<Expression>),

    // String built in
    StringLen,

    // Tuple built in
    Tuple2FloatMagnitude,
    ArrayLen,
    ArrayIsEmpty,
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
    ExternalTypeIndex(ExternalTypeRef, Expression),
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
    MutExternalTypeIndexRef(ExternalTypeRef),
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

#[derive(Debug)]
pub enum ExpressionKind {
    // Access Lookup values
    ConstantAccess(ConstantRef),
    VariableAccess(VariableRef),
    FieldAccess(Box<Expression>, usize),
    ArrayAccess(
        Box<Expression>,
        ArrayTypeRef,
        Box<Expression>, // int index lookup
    ), // Read from an array: arr[3]
    MapIndexAccess(Box<Expression>, MapTypeRef, Box<Expression>),
    StringRangeAccess(Box<Expression>, Box<Range>),
    ArrayRangeAccess(Box<Expression>, Box<Range>),

    // ----
    InternalFunctionAccess(InternalFunctionDefinitionRef),
    ExternalFunctionAccess(ExternalFunctionDefinitionRef),

    // Adding to a collection
    MapAssignment(
        Box<SingleMutLocationExpression>,
        Box<Expression>,
        Box<Expression>,
    ), // Motivation: Can not use location since adding is more complex

    // Operators
    BinaryOp(BinaryOperator),
    UnaryOp(UnaryOperator),
    PostfixChain(Box<Expression>, Vec<Postfix>),

    // Conversion
    // the `?` operator. unwraps the value, unless it is none
    //NoneCoalesceOperator(Box<Expression>, Box<Expression>),
    CoerceOptionToBool(Box<Expression>),

    // Calls

    // For calls from returned function values
    FunctionCall(
        Signature,
        Box<Expression>,
        Vec<ArgumentExpressionOrLocation>,
    ),

    MemberCall(MemberCall),
    InterpolatedString(Vec<StringPart>),

    // Constructing
    VariableDefinition(VariableRef, Box<MutOrImmutableExpression>), // First time assignment
    VariableReassignment(VariableRef, Box<MutOrImmutableExpression>),

    StructInstantiation(StructInstantiation),
    Array(ArrayInstantiation),
    Tuple(Vec<Expression>),
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
    ArrayExtend(SingleMutLocationExpression, Box<Expression>), // Extends an array with another array
    ArrayPush(SingleMutLocationExpression, Box<Expression>),   // Adds an item to an array

    // Sparse Built in
    SparseNew(ExternalTypeRef, Type), // item type
}

#[derive(Debug)]
pub struct StringConst(pub Node);

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

pub type StructTypeRef = Rc<RefCell<StructType>>;

pub fn same_struct_ref(a: &StructTypeRef, b: &StructTypeRef) -> bool {
    Rc::ptr_eq(a, b)
}

pub type TypeNumber = u32;

#[derive(Debug, Clone, Eq, PartialEq)]
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

#[derive(Eq, PartialEq)]
pub struct StructType {
    pub name: Node,
    pub assigned_name: String,
    pub anon_struct_type: AnonymousStructType,

    //
    pub functions: SeqMap<String, FunctionRef>,
}

impl Debug for StructType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "struct {:?}", self.assigned_name)
    }
}

impl StructType {
    pub fn new(name: Node, assigned_name: &str, anon_struct_type: AnonymousStructType) -> Self {
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

    pub fn name(&self) -> &Node {
        &self.name
    }

    pub fn add_external_member_function(
        &mut self,
        external_func: ExternalFunctionDefinitionRef,
    ) -> Result<(), SeqMapError> {
        let name = external_func.assigned_name.clone();
        let func = Function::External(external_func);
        self.functions.insert(name, func.into())?;
        Ok(())
    }

    pub fn get_member_function(&self, function_name: &str) -> Option<&FunctionRef> {
        self.functions.get(&function_name.to_string())
    }

    pub fn get_internal_member_function(
        &self,
        function_name: &str,
    ) -> Option<InternalFunctionDefinitionRef> {
        let func = self.functions.get(&function_name.to_string())?;
        match &**func {
            Function::Internal(fn_def) => Some(fn_def.clone()),
            _ => None,
        }
    }
}

pub type OptionTypeRef = Rc<crate::OptionType>;

#[derive(Debug)]
pub struct OptionType {
    pub item_type: Type,
}

pub type ArrayTypeRef = Rc<ArrayType>;

pub fn same_array_ref(a: &ArrayTypeRef, b: &ArrayTypeRef) -> bool {
    Rc::ptr_eq(a, b)
}

#[derive(Debug, Eq, PartialEq)]
pub struct ArrayType {
    pub item_type: Type,
}

pub type MapTypeRef = Rc<MapType>;

#[derive(Debug, Eq, PartialEq)]
pub struct MapType {
    pub key_type: Type,
    pub value_type: Type,
}

pub type EnumVariantStructTypeRef = Rc<EnumVariantStructType>;

#[derive(Clone, Eq, PartialEq)]
pub struct AnonymousStructType {
    pub defined_fields: SeqMap<String, StructTypeField>,
}

impl Debug for AnonymousStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        write!(f, "{}", comma_seq(&self.defined_fields))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct EnumVariantStructType {
    pub common: EnumVariantCommon,
    pub anon_struct: AnonymousStructType,
}

pub type EnumVariantTupleTypeRef = Rc<EnumVariantTupleType>;

#[derive(Debug, Eq, PartialEq)]
pub struct EnumVariantTupleType {
    pub common: EnumVariantCommon,
    pub fields_in_order: Vec<Type>,
}

pub type TupleTypeRef = Rc<TupleType>;

#[derive(Debug, Eq, PartialEq)]
pub struct TupleType(pub Vec<Type>);

impl TupleType {
    pub fn new(types: Vec<Type>) -> Self {
        Self(types)
    }
}

pub type EnumTypeRef = Rc<RefCell<EnumType>>;

#[derive(Eq, PartialEq)]
pub struct EnumType {
    pub name: LocalTypeIdentifier,
    pub assigned_name: String,
    pub module_path: Vec<String>,
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
    pub fn new(
        name: LocalTypeIdentifier,
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

#[derive(Eq, PartialEq, Clone)]
pub struct EnumVariantCommon {
    pub name: LocalTypeIdentifier,
    pub assigned_name: String,
    pub number: TypeNumber,
    pub container_index: u8,
    pub owner: EnumTypeRef,
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

#[derive(Debug, Eq, PartialEq)]
pub struct EnumVariantTupleFieldType {
    pub name: LocalIdentifier,
    pub enum_variant: EnumVariantTypeRef,
    pub resolved_type: Type,

    pub field_index: usize,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct EnumVariantSimpleType {
    pub common: EnumVariantCommon,
}

pub type EnumVariantSimpleTypeRef = Rc<EnumVariantSimpleType>;

#[derive(Clone, Eq, PartialEq)]
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

#[derive(Debug)]
pub struct Use {
    pub path: Vec<Node>,
    pub items: Vec<UseItem>,
}

// Mutable part
#[derive(Debug)]
pub struct ProgramState {
    pub array_types: Vec<ArrayTypeRef>,
    pub number: TypeNumber,
    pub external_function_number: ExternalFunctionId,
    // It is just so we don't have to do another dendency check of the
    // modules, we know that these constants have been
    // evaluated in order already
    pub constants_in_dependency_order: Vec<ConstantRef>,
}

impl ProgramState {
    pub fn new() -> Self {
        Self {
            array_types: Vec::new(),
            number: 0,
            external_function_number: 0,
            constants_in_dependency_order: Vec::new(),
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
