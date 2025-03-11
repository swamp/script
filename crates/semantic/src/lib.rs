/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod inst_cache;
pub mod intr;
pub mod prelude;

use crate::inst_cache::InstantiationCache;
use crate::intr::IntrinsicFunction;
use crate::prelude::IntrinsicFunctionDefinitionRef;

pub use fixed32::Fp;
use seq_map::SeqMap;
use std::cmp::PartialEq;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use swamp_script_node::Node;
use swamp_script_types::prelude::*;
use tracing::{error, info};

#[derive(Debug, Clone)]
pub struct TypeWithMut {
    pub resolved_type: Type,
    pub is_mutable: bool,
}

#[derive(Debug, Clone)]
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
    DuplicateSymbolName(String),
    DuplicateNamespaceLink(String),
    MismatchedTypes { expected: Type, found: Vec<Type> },
    UnknownImplOnType,
    UnknownTypeVariable,
}

#[derive(Debug, Eq, PartialEq)]
pub struct LocalIdentifier(pub Node);

//#[derive(Debug,Clone)]
pub struct InternalFunctionDefinition {
    pub body: Expression,
    pub name: LocalIdentifier,
    pub assigned_name: String,
    pub signature: Signature,
}

impl Default for InternalFunctionDefinition {
    fn default() -> Self {
        Self {
            body: Expression {
                ty: Type::Never,
                node: Default::default(),
                kind: ExpressionKind::Break,
            },
            name: LocalIdentifier(Default::default()),
            assigned_name: "".to_string(),
            signature: Signature {
                parameters: vec![],
                return_type: Box::new(Type::Never),
            },
        }
    }
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct MutVariable {
    pub variable_ref: VariableRef,
}

//type MutVariableRef = Rc<MutVariable>;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct BinaryOperator {
    pub left: Box<Expression>,
    pub right: Box<Expression>,
    pub kind: BinaryOperatorKind,
    pub node: Node,
}

#[derive(Debug, Clone)]
pub enum UnaryOperatorKind {
    Not,
    Negate,
}
#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct ExternalFunctionCall {
    pub arguments: Vec<ArgumentExpressionOrLocation>,
    pub function_definition: ExternalFunctionDefinitionRef,
    pub function_expression: Box<Expression>,
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

#[derive(Debug, Clone)]
pub struct MemberCall {
    pub function: FunctionRef,
    pub arguments: Vec<ArgumentExpressionOrLocation>,
}

#[derive(Debug, Clone)]
pub struct ArrayItem {
    pub item_type: Type,
    pub int_expression: Expression,
    pub array_expression: Expression,
    pub array_type: Type,
}

pub type ArrayItemRef = Rc<ArrayItem>;

#[derive(Debug, Clone)]
pub enum PrecisionType {
    Float,
    String,
}

#[derive(Debug, Clone)]
pub enum FormatSpecifierKind {
    LowerHex,                            // :x
    UpperHex,                            // :X
    Binary,                              // :b
    Float,                               // :f
    Precision(u32, Node, PrecisionType), // :..2f or :..5s
}

#[derive(Debug, Clone)]
pub struct FormatSpecifier {
    pub node: Node,
    pub kind: FormatSpecifierKind,
}

#[derive(Debug, Clone)]
pub enum StringPart {
    Literal(Node, String),
    Interpolation(Expression, Option<FormatSpecifier>),
}

pub type FunctionRef = Rc<Function>;

#[derive(Debug, Eq, Clone, PartialEq)]
pub enum Function {
    Internal(InternalFunctionDefinitionRef),
    External(ExternalFunctionDefinitionRef),
}

impl Function {
    #[must_use]
    pub fn name(&self) -> String {
        match self {
            Self::Internal(x) => x.assigned_name.clone(),
            Self::External(y) => y.assigned_name.clone(),
        }
    }

    #[must_use]
    pub fn maybe_node(&self) -> Option<&Node> {
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

#[derive(Debug, Clone)]
pub struct BooleanExpression {
    #[allow(unused)]
    pub expression: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Match {
    pub arms: Vec<MatchArm>,
    pub expression: Box<MutOrImmutableExpression>,
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    #[allow(unused)]
    pub pattern: Pattern,
    pub expression: Box<Expression>,
    pub expression_type: Type,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Normal(NormalPattern, Option<BooleanExpression>),
    Wildcard(Node),
}

#[derive(Debug, Clone)]
pub enum NormalPattern {
    PatternList(Vec<PatternElement>),
    EnumPattern(EnumVariantType, Option<Vec<PatternElement>>),
    Literal(Literal),
}

#[derive(Debug, Clone)]
pub enum PatternElement {
    Variable(VariableRef),
    VariableWithFieldIndex(VariableRef, usize),
    Wildcard(Node),
}

#[derive(Debug, Clone)]
pub struct Iterable {
    pub key_type: Option<Type>, // It does not have to support a key type
    pub value_type: Type,

    pub resolved_expression: Box<MutOrImmutableExpression>,
}

#[derive(Debug, Clone)]
pub struct StructInstantiation {
    pub source_order_expressions: Vec<(usize, Expression)>,
    pub struct_type_ref: NamedStructType,
}

#[derive(Debug, Clone)]
pub struct AnonymousStructLiteral {
    pub source_order_expressions: Vec<(usize, Expression)>,
    pub anonymous_struct_type: AnonymousStructType,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CompoundOperatorKind {
    Add,
    Sub,
    Mul,
    Div,
    Modulo,
}

#[derive(Debug, Clone)]
pub struct CompoundOperator {
    pub node: Node,
    pub kind: CompoundOperatorKind,
}

#[derive(Debug, Clone)]
pub struct VariableCompoundAssignment {
    pub variable_ref: VariableRef, // compound only support single variable
    pub expression: Box<Expression>,
    pub compound_operator: CompoundOperator,
}

pub fn create_rust_type(name: &str, external_number: u32) -> ExternalType {
    ExternalType {
        type_name: name.to_string(),
        number: external_number,
    }
}

#[derive(Debug, Clone)]
pub struct Guard {
    pub condition: Option<BooleanExpression>,
    pub result: Expression,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum RangeMode {
    Inclusive,
    Exclusive,
}

#[derive(Debug, Clone)]
pub struct Postfix {
    pub node: Node,
    pub ty: Type,
    pub kind: PostfixKind,
}

#[derive(Debug, Clone)]
pub struct Range {
    pub min: Expression,
    pub max: Expression,
    pub mode: RangeMode,
}

#[derive(Debug, Clone)]
pub enum PostfixKind {
    StructField(AnonymousStructType, usize),
    //    ExternalTypeIndexRef(ExternalTypeRef, Expression),
    MemberCall(FunctionRef, Vec<ArgumentExpressionOrLocation>),
    FunctionCall(Vec<ArgumentExpressionOrLocation>),
    OptionUnwrap, // ? operator
    NoneCoalesce(Expression),
    //IntrinsicCallEx(IntrinsicFunction, Vec<ArgumentExpressionOrLocation>),
    IntrinsicCall(IntrinsicFunction, Vec<Expression>),
}

#[derive(Debug, Clone)]
pub enum LocationAccessKind {
    FieldIndex(AnonymousStructType, usize),
    IntrinsicCallMut(IntrinsicFunction, Vec<Expression>),
}

#[derive(Debug, Clone)]
pub struct LocationAccess {
    pub node: Node,
    pub ty: Type,
    pub kind: LocationAccessKind,
}

#[derive(Debug, Clone)]
pub struct SingleLocationExpression {
    pub kind: SingleLocationExpressionKind,
    pub node: Node,
    pub ty: Type,

    pub starting_variable: VariableRef,
    pub access_chain: Vec<LocationAccess>,
}

#[derive(Debug, Clone)]
pub struct SingleMutLocationExpression(pub SingleLocationExpression);

#[derive(Debug, Clone)]
pub enum SingleLocationExpressionKind {
    MutVariableRef,
    MutStructFieldRef(NamedStructType, usize),
}

#[derive(Debug, Clone)]
pub struct SliceLocationExpression {
    pub start: Box<Expression>,
    pub range_start: Box<Expression>,
    pub range_end: Box<Expression>,
    pub mode: RangeMode,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct MutOrImmutableExpression {
    pub expression_or_location: ArgumentExpressionOrLocation,
    pub is_mutable: Option<Node>,
}

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

#[derive(Debug, Clone)]
pub enum ArgumentExpressionOrLocation {
    Expression(Expression),
    Location(SingleLocationExpression),
}

#[derive(Clone)]
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

#[derive(Debug, Clone)]
pub struct WhenBinding {
    pub variable: VariableRef,
    pub expr: MutOrImmutableExpression,
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    // Access Lookup values
    ConstantAccess(ConstantRef),
    VariableAccess(VariableRef),
    FieldAccess(Box<Expression>, usize),
    ArrayAccess(
        Box<Expression>,
        Type,
        Box<Expression>, // int index lookup
    ), // Read from an array: arr[3]
    MapIndexAccess(Box<Expression>, Type, Type, Box<Expression>),
    StringRangeAccess(Box<Expression>, Box<Range>),
    ArrayRangeAccess(Box<Expression>, Box<Range>),

    // ----
    IntrinsicFunctionAccess(IntrinsicFunctionDefinitionRef),
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

    //MemberCall(MemberCall),
    InterpolatedString(Vec<StringPart>),

    // Constructing
    VariableDefinition(VariableRef, Box<MutOrImmutableExpression>), // First time assignment
    VariableReassignment(VariableRef, Box<MutOrImmutableExpression>),

    StructInstantiation(StructInstantiation),
    AnonymousStructLiteral(AnonymousStructLiteral),
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

    TupleDestructuring(Vec<VariableRef>, Vec<Type>, Box<Expression>),

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
    IntrinsicCallMut(
        IntrinsicFunction,
        SingleMutLocationExpression,
        Vec<Expression>,
    ),

    //IntrinsicCall(IntrinsicFunction, Vec<Expression>),
    IntrinsicCallEx(IntrinsicFunction, Vec<ArgumentExpressionOrLocation>),
}

#[derive(Debug, Clone)]
pub struct StringConst(pub Node);

#[derive(Debug, Clone)]
pub enum Literal {
    FloatLiteral(Fp),
    NoneLiteral,
    IntLiteral(i32),
    StringLiteral(String),
    BoolLiteral(bool),

    EnumVariantLiteral(EnumVariantType, EnumLiteralData),
    TupleLiteral(Vec<Type>, Vec<Expression>),

    Slice(Type, Vec<Expression>),
    SlicePair(Type, Vec<(Expression, Expression)>),
}

#[derive(Debug, Clone)]
pub struct ArrayInstantiation {
    pub expressions: Vec<Expression>,
    pub item_type: Type,
    pub array_type: Type,
    pub array_type_ref: Type,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LocalTypeIdentifier(pub Node);

#[derive(Debug, Clone)]
pub struct Constant {
    pub name: Node,
    pub assigned_name: String,
    pub id: ConstantId,
    pub expr: Expression,
    pub resolved_type: Type,
}
pub type ConstantRef = Rc<Constant>;

pub type OptionTypeRef = Rc<crate::OptionType>;

#[derive(Debug, Clone)]
pub struct OptionType {
    pub item_type: Type,
}

/*
pub fn sort_struct_fields(
    unordered_seq_map: &SeqMap<String, StructTypeField>,
) -> SeqMap<String, StructTypeField> {
    let mut sorted_pairs: Vec<(&String, &StructTypeField)> = unordered_seq_map.iter().collect();
    sorted_pairs.sort_by(|a, b| a.0.cmp(b.0));
    let mut ordered_seq_map = SeqMap::new();

    for (name, field) in sorted_pairs {
        ordered_seq_map.insert(name, field).unwrap() // We know already that the key fields are unique
    }

    ordered_seq_map
}

 */

#[derive(Debug, Clone)]
pub struct ImplMember {}

#[derive(Debug, Clone)]
pub enum UseItem {
    Identifier(Node),
    TypeIdentifier(Node),
}

#[derive(Debug, Clone)]
pub struct Use {
    pub path: Vec<Node>,
    pub items: Vec<UseItem>,
}

#[derive(Debug, Clone)]
pub struct ImplFunctions {
    pub functions: SeqMap<String, FunctionRef>,
}

impl Default for ImplFunctions {
    fn default() -> Self {
        Self::new()
    }
}

impl ImplFunctions {
    #[must_use]
    pub fn new() -> Self {
        Self {
            functions: SeqMap::default(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AssociatedImpls {
    pub functions: SeqMap<Type, ImplFunctions>,
}

impl Default for AssociatedImpls {
    fn default() -> Self {
        Self::new()
    }
}

impl AssociatedImpls {
    #[must_use]
    pub fn new() -> Self {
        Self {
            functions: SeqMap::default(),
        }
    }
}

impl AssociatedImpls {
    pub fn prepare(&mut self, ty: &Type) {
        self.functions
            .insert(ty.clone(), ImplFunctions::new())
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
        info!(
            ?ty,
            function_name, "could not find member, this is what I have"
        );
        None
    }

    pub fn api_get_external_function(
        &self,
        ty: &Type,
        function_name: &str,
    ) -> Option<&ExternalFunctionDefinitionRef> {
        if let Some(found) = self.get_member_function(ty, function_name) {
            if let Function::External(ext_fn) = &**found {
                return Some(ext_fn);
            }
        }
        None
    }

    pub fn api_fetch_external_function_id(
        &self,
        ty: &Type,
        function_name: &str,
    ) -> ExternalFunctionId {
        self.api_get_external_function(ty, function_name)
            .unwrap()
            .id
    }

    pub fn get_internal_member_function(
        &self,
        ty: &Type,
        function_name: &str,
    ) -> Option<&InternalFunctionDefinitionRef> {
        if let Some(found) = self.get_member_function(ty, function_name) {
            if let Function::Internal(int_fn) = &**found {
                return Some(int_fn);
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
            error!(%ty, ?name, "wasn't prepared");
            Err(SemanticError::UnknownImplOnType)
        }
    }

    pub fn add_external_member_function(
        &mut self,
        ty: &Type,
        func: ExternalFunctionDefinition,
    ) -> Result<(), SemanticError> {
        self.add_member_function(
            ty,
            &func.assigned_name.clone(),
            Function::External(func.into()).into(),
        )
    }

    pub fn add_external_struct_member_function(
        &mut self,
        named_struct_type: &NamedStructType,
        func: Function,
    ) -> Result<(), SemanticError> {
        self.add_member_function(
            &Type::NamedStruct(named_struct_type.clone()),
            &func.name().clone(),
            func.into(),
        )
    }

    pub fn add_external_struct_member_function_external(
        &mut self,
        named_struct_type: NamedStructType,
        func: ExternalFunctionDefinition,
    ) -> Result<(), SemanticError> {
        self.add_member_function(
            &Type::NamedStruct(named_struct_type.clone()),
            &func.assigned_name.clone(),
            Function::External(func.into()).into(),
        )
    }

    pub fn add_external_struct_member_function_external_ref(
        &mut self,
        named_struct_type: NamedStructType,
        func: ExternalFunctionDefinitionRef,
    ) -> Result<(), SemanticError> {
        self.add_member_function(
            &Type::NamedStruct(named_struct_type.clone()),
            &func.assigned_name.clone(),
            Function::External(func.into()).into(),
        )
    }
}

// Mutable part
#[derive(Debug, Clone)]
pub struct ProgramState {
    pub external_function_number: ExternalFunctionId,
    // It is just so we don't have to do another dependency check of the
    // modules, we know that these constants have been
    // evaluated in order already
    pub constants_in_dependency_order: Vec<ConstantRef>,
    pub associated_impls: AssociatedImpls,
    pub instantiation_cache: InstantiationCache,
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
            external_function_number: 0,
            constants_in_dependency_order: Vec::new(),
            associated_impls: AssociatedImpls::new(),
            instantiation_cache: InstantiationCache::new(),
        }
    }

    pub fn allocate_external_function_id(&mut self) -> ExternalFunctionId {
        self.external_function_number += 1;
        self.external_function_number
    }
}

#[derive(Clone)]
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
