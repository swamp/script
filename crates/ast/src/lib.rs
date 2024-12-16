/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use seq_fmt::comma;
use seq_map::SeqMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;
use swamp_script_node::SpanWithoutFileId;

// Common metadata that can be shared across all AST nodes
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Node {
    pub span: SpanWithoutFileId,
    // TODO: Add comments and attributes
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.span)
    }
}

/// Identifiers ================
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct QualifiedTypeIdentifier {
    pub name: LocalTypeIdentifier,
    pub module_path: Option<ModulePath>,
}

impl QualifiedTypeIdentifier {
    pub fn new(name: LocalTypeIdentifier, module_path: Vec<ModulePathItem>) -> Self {
        let module_path = if module_path.is_empty() {
            None
        } else {
            Some(ModulePath(module_path))
        };

        Self { name, module_path }
    }
}

impl Display for QualifiedTypeIdentifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(module_path) = &self.module_path {
            write!(f, "{module_path}::",)?;
        }
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct LocalTypeIdentifier(pub Node);

impl LocalTypeIdentifier {
    pub fn new(node: Node) -> Self {
        Self(node)
    }
}

impl Display for LocalTypeIdentifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LocalIdentifier(pub Node);

impl LocalIdentifier {
    #[must_use]
    pub fn new(node: Node) -> Self {
        Self(node)
    }
}

impl Display for LocalIdentifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Debug for LocalIdentifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct MemberFunctionIdentifier(pub Node);

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct IdentifierName(pub Node);

impl Display for IdentifierName {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct FieldName(pub Node);

impl Display for FieldName {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// =========================

#[derive(Clone)]
pub struct StringConst(pub Node);

impl Display for StringConst {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl Debug for StringConst {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ModulePathItem {
    pub node: Node,
}

impl Display for ModulePathItem {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.node)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct ModulePath(pub Vec<ModulePathItem>);

impl ModulePath {
    pub fn new() -> Self {
        Self(vec![])
    }
}

impl Display for ModulePath {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for x in &self.0 {
            write!(f, "::{x}")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Import {
    pub module_path: ModulePath,
    pub items: ImportItems,
}

impl Display for Import {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "import")
    }
}

#[derive(Debug, Clone)]
pub enum ImportItems {
    Module, // import the whole module (use last path item as namespace)
    Specific(Vec<LocalTypeIdentifier>), // import { sin, cos } from math
}

#[derive(Clone, Debug, Eq, PartialEq, Default)]
pub struct StructType {
    pub identifier: LocalTypeIdentifier,
    pub fields: SeqMap<FieldName, Type>,
}

impl StructType {
    #[must_use]
    pub const fn new(identifier: LocalTypeIdentifier, fields: SeqMap<FieldName, Type>) -> Self {
        Self { identifier, fields }
    }
}

impl Display for StructType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "struct {} {}", self.identifier, self.fields)
    }
}

#[derive(Debug)]
pub enum Definition {
    StructDef(StructType),

    EnumDef(
        LocalTypeIdentifier,
        SeqMap<LocalTypeIdentifier, EnumVariant>,
    ),

    FunctionDef(LocalIdentifier, Function),
    ImplDef(LocalTypeIdentifier, SeqMap<IdentifierName, Function>),
    TypeAlias(LocalTypeIdentifier, Type),
    Import(Import),
    // Other
    Comment(Node),
}

#[derive(Debug, Clone)]
pub struct ForVar {
    pub identifier: LocalIdentifier,
    pub is_mut: Option<Node>,
}

impl Display for ForVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "mut:{:?} {}", self.is_mut, self.identifier)
    }
}

#[derive(Debug, Clone)]
pub enum ForPattern {
    Single(ForVar),
    Pair(ForVar, ForVar),
}

impl Display for ForPattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Single(v) => write!(f, "{v}"),
            Self::Pair(a, b) => write!(f, "{a}, {b}"),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    ForLoop(ForPattern, Expression, bool, Vec<Statement>),
    WhileLoop(Expression, Vec<Statement>),
    Return(Expression),
    Break(Node),
    Continue(Node),
    Expression(Expression), // Used for expressions with side effects (mutation, i/o) TODO: Remove this
    Block(Vec<Statement>),
    If(Expression, Vec<Statement>, Option<Vec<Statement>>),
}

#[derive(Clone, Eq, PartialEq)]
pub struct VariableNotMut {
    pub name: LocalIdentifier,
}

#[derive(Clone, Eq, PartialEq)]
pub struct Variable {
    pub name: Node,
    pub is_mutable: Option<Node>,
}

impl Debug for Variable {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if let Some(x) = &self.is_mutable {
            write!(f, "{} {}", x, self.name)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

impl Variable {
    pub fn new(name: Node, is_mutable: Option<Node>) -> Self {
        Self { name, is_mutable }
    }
}

#[derive(Debug)]
pub struct MutVariableRef(pub Variable); // Just wraps a variable when passed with mut keyword

#[derive(Debug, Eq, PartialEq)]
pub struct Parameter {
    pub variable: Variable,
    pub param_type: Type,
    //    pub is_mutable: Option<Node>,
}

#[derive(Debug)]
pub struct FunctionSignature {
    pub name: LocalIdentifier,
    pub params: Vec<Parameter>,
    pub self_parameter: Option<SelfParameter>,
    pub return_type: Option<Type>,
}

#[derive()]
pub struct FunctionData {
    pub signature: FunctionSignature,
    pub body: Vec<Statement>,
}

#[derive(Debug)]
pub enum Function {
    Internal(FunctionData),
    External(FunctionSignature),
}

#[derive(Debug)]
pub enum ImplItem {
    Member(ImplMember),
    Function(ImplFunction),
}

#[derive()]
pub enum ImplMember {
    Internal(ImplMemberData),
    External(ImplMemberSignature),
}

#[derive(Debug)]
pub struct ImplMemberSignature {
    pub name: LocalIdentifier,
    pub self_param: SelfParameter,
    pub params: Vec<Parameter>,
    pub return_type: Type,
}

#[derive(Debug)]
pub enum ImplFunction {
    Internal(FunctionData),
    External(FunctionSignature),
}

impl Debug for ImplMember {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Internal(data) => write!(f, "internal: {data:?}"),
            Self::External(data) => write!(f, "external: {data:?}"),
        }
    }
}

#[derive()]
pub struct ImplMemberData {
    pub name: LocalIdentifier,
    pub self_param: SelfParameter,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub body: Vec<Statement>, // Will be empty for external members
}

pub type ImplMemberRef = Rc<ImplMember>;

impl Debug for SelfParameter {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.is_mutable.is_some() {
            write!(f, "mut self")
        } else {
            write!(f, "self")
        }
    }
}

impl Debug for ImplMemberData {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{:?}, {:?}, {:?}, {:?}",
            self.self_param, self.params, self.return_type, self.body
        )
    }
}

#[derive(Clone)]
pub struct SelfParameter {
    pub is_mutable: Option<Node>,
    pub self_node: Node,
}

impl Debug for FunctionData {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}, {:?}", self.signature, self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompoundOperator {
    Add(Node), // +=
    Sub(Node), // -=
    Mul(Node), // *=
    Div(Node), // /=
}

/// Expressions are things that "converts" to a value when evaluated.
#[derive(Debug)]
pub enum Expression {
    // Access / Lookup values
    FieldAccess(Box<Expression>, FieldName),
    VariableAccess(Variable),
    MutRef(MutVariableRef), // Used when passing with mut keyword. mut are implicitly passed by reference
    IndexAccess(Box<Expression>, Box<Expression>), // Read from an array or map: arr[3]

    // Assignment ----

    // Since it is a cool language, we can "chain" assignments together. like a = b = c = 1. Even for field assignments, like a.b = c.d = e.f = 1
    VariableAssignment(Variable, Box<Expression>),
    MultiVariableAssignment(Vec<Variable>, Box<Expression>),
    IndexCompoundAssignment(
        Box<Expression>,
        Box<Expression>,
        CompoundOperator,
        Box<Expression>,
    ),
    VariableCompoundAssignment(Variable, CompoundOperator, Box<Expression>),
    FieldCompoundAssignment(
        Box<Expression>,
        FieldName,
        CompoundOperator,
        Box<Expression>,
    ),

    IndexAssignment(Box<Expression>, Box<Expression>, Box<Expression>), // target, index, source. Write to an index in an array or map: arr[3] = 42
    FieldAssignment(Box<Expression>, FieldName, Box<Expression>),

    // Operators ----
    BinaryOp(Box<Expression>, BinaryOperator, Box<Expression>),
    UnaryOp(UnaryOperator, Box<Expression>),

    // Postfix operators
    PostfixOp(PostfixOperator, Box<Expression>),

    // Calls ----
    FunctionCall(Box<Expression>, Vec<Expression>),
    StaticCall(
        LocalTypeIdentifier,
        MemberFunctionIdentifier,
        Vec<Expression>,
    ), // Type::func(args)
    StaticCallGeneric(
        LocalTypeIdentifier,
        MemberFunctionIdentifier,
        Vec<Expression>,
        Vec<Type>, // Generic arguments
    ),
    MemberCall(Box<Expression>, MemberFunctionIdentifier, Vec<Expression>),
    Block(Vec<Statement>),

    InterpolatedString(Vec<StringPart>),

    // Constructing
    StructInstantiation(QualifiedTypeIdentifier, SeqMap<FieldName, Expression>),
    ExclusiveRange(Box<Expression>, Box<Expression>),
    Literal(Literal),

    // Comparing
    IfElse(Box<Expression>, Box<Expression>, Box<Expression>),
    Match(Box<Expression>, Vec<MatchArm>),
}

#[derive(Debug)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expression: Expression,
}

// Are constructed by themselves
#[derive()]
pub enum Literal {
    Int(Node),
    Float(Node),
    String(Node),
    Bool(Node),
    EnumVariant(
        QualifiedTypeIdentifier,
        LocalTypeIdentifier,
        EnumLiteralData,
    ), // EnumTypeName::Identifier tuple|struct
    Tuple(Vec<Expression>),
    Array(Vec<Expression>),
    Map(Vec<(Expression, Expression)>),
    Unit, // ()
    None, // none
}

pub fn seq_map_to_string<K, V>(map: &SeqMap<K, V>) -> String
where
    K: Debug + Eq + Hash + Clone,
    V: Debug,
{
    let entries = map
        .iter()
        .map(|(k, v)| format!("{:?}: {:?}", k, v))
        .collect::<Vec<String>>()
        .join(", ");

    entries.trim().to_string()
}

impl Debug for Literal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::EnumVariant(enum_type, variant_name, data) => match data {
                EnumLiteralData::Nothing => {
                    write!(f, "EnumVariant({enum_type}::{variant_name})")
                }
                EnumLiteralData::Tuple(expressions) => write!(
                    f,
                    "EnumVariant({enum_type}::{variant_name}({expressions:?}))"
                ),
                EnumLiteralData::Struct(expressions) => write!(
                    f,
                    "EnumVariant({}::{}{{ {:?} }})",
                    enum_type,
                    variant_name,
                    expressions
                ),
            },

            Self::Int(v) => write!(f, "Int({v})"),
            Self::Float(v) => write!(f, "Float({v})"),
            Self::String(v) => write!(f, "String({v})"),
            Self::Bool(v) => write!(f, "Bool({v})"),
            Self::Tuple(v) => write!(f, "Tuple({v:?})"),
            Self::Unit => write!(f, "()"),
            Self::None => write!(f, "none"),
            Self::Array(v) => write!(f, "Array({v:?})"),
            Self::Map(v) => write!(f, "Map({v:?})"),
        }
    }
}

#[derive(Debug)]
pub struct AnonymousStructField {
    pub field_name: FieldName,
    pub expression: Expression,
}


#[derive(Debug)]
pub struct AnonymousStructTypeField {
    pub field_name: FieldName,
    pub field_type: Type,
}


#[derive()]
pub enum EnumLiteralData {
    Nothing,
    Tuple(Vec<Expression>),
    Struct(Vec<AnonymousStructField>),
}

impl Debug for EnumLiteralData {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Nothing => write!(f, ""),
            Self::Tuple(types) => write!(f, "{types:?}"),
            Self::Struct(types) => write!(f, "{types:?}"),
        }
    }
}

#[derive( Debug, Default)]
pub struct AnonymousStructType {
    pub fields: Vec<AnonymousStructTypeField>,
}

impl Display for AnonymousStructType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{ {:?} }}", self.fields)
    }
}


#[derive()]
pub enum EnumVariant {
    Simple(Node),
    Tuple(Vec<Type>),
    Struct(AnonymousStructType),
}

impl Debug for EnumVariant {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Simple(_) => write!(f, ""),
            Self::Tuple(types) => write!(f, "{types:?}"),
            Self::Struct(anon_struct) => write!(f, "{anon_struct:?}"),
        }
    }
}

impl Display for EnumVariant {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Simple(_) => write!(f, ""),
            Self::Tuple(types) => write!(f, "{}", comma(types)),
            Self::Struct(anon_struct) => write!(f, "{anon_struct}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    // Primitives
    Int(Node),
    Float(Node),
    String(Node),
    Bool(Node),
    Unit(Node),
    Any(Node),
    Generic(Box<Type>, Vec<Type>),
    Struct(QualifiedTypeIdentifier),
    Array(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Enum(QualifiedTypeIdentifier),
    TypeReference(QualifiedTypeIdentifier),
    Optional(Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(_) => write!(f, "Int"),
            Self::Float(_) => write!(f, "Float"),
            Self::String(_) => write!(f, "String"),
            Self::Bool(_) => write!(f, "Bool"),
            Self::Unit(_) => write!(f, "()"),
            Self::Any(_) => write!(f, "ANY"),
            Self::Generic(a, b) => write!(f, "{a} {}", comma(b)),
            Self::Struct(a) => write!(f, "{a}"),
            Self::Array(t) => write!(f, "[{t}]"),
            Self::Map(a, b) => write!(f, "[{a}: {b}]"),
            Self::Tuple(types) => write!(f, "( {} )", comma(types)),
            Self::Enum(a) => write!(f, "{a}"),
            Self::TypeReference(a) => write!(f, "ref {a}"),
            Self::Optional(a) => write!(f, "{a}?"),
        }
    }
}

// Takes a left and right side expression
#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add(Node),
    Subtract(Node),
    Multiply(Node),
    Divide(Node),
    Modulo(Node),
    LogicalOr(Node),
    LogicalAnd(Node),
    Equal(Node),
    NotEqual(Node),
    LessThan(Node),
    LessEqual(Node),
    GreaterThan(Node),
    GreaterEqual(Node),
    RangeExclusive(Node),
}

// Only takes one expression argument
#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Not(Node),
    Negate(Node),
}

// Only takes one expression argument
#[derive(Debug, Clone)]
pub enum PostfixOperator {
    Unwrap(Node), // option_operator
}

impl Display for CompoundOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            Self::Add(_) => "+=",
            Self::Sub(_) => "-=",
            Self::Mul(_) => "*=",
            Self::Div(_) => "/=",
        };
        write!(f, "{}", symbol)
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            Self::Add(_) => "+",
            Self::Subtract(_) => "-",
            Self::Multiply(_) => "*",
            Self::Divide(_) => "/",
            Self::Modulo(_) => "%",
            Self::LogicalOr(_) => "||",
            Self::LogicalAnd(_) => "&&",
            Self::Equal(_) => "==",
            Self::NotEqual(_) => "!=",
            Self::LessThan(_) => "<",
            Self::LessEqual(_) => "<=",
            Self::GreaterThan(_) => ">",
            Self::GreaterEqual(_) => ">=",
            Self::RangeExclusive(_) => "..",
        };
        write!(f, "{}", symbol)
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            Self::Not(_) => "!",
            Self::Negate(_) => "-",
        };
        write!(f, "{}", symbol)
    }
}

impl Display for PostfixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            Self::Unwrap(_) => "?", // or "!" depending on your language design
        };
        write!(f, "{}", symbol)
    }
}

// Patterns are used in matching and destructuring
#[derive(Debug)]
pub enum Pattern {
    PatternList(Vec<PatternElement>), // TODO: Change to SetVec
    EnumPattern(LocalTypeIdentifier, Option<Vec<PatternElement>>), // TODO: Change to SetVec
    Literal(Literal),
}

#[derive(Debug)]
pub enum PatternElement {
    Variable(Node),
    Expression(Expression),
    Wildcard(Node),
}

#[derive(Debug)]
pub enum StringPart {
    Literal(Node),
    Interpolation(Box<Expression>, Option<FormatSpecifier>),
}

#[derive(Debug, Clone)]
pub enum FormatSpecifier {
    Debug(Node),                    // :?
    LowerHex(Node),                 // :x
    UpperHex(Node),                 // :X
    Binary(Node),                   // :b
    Float(Node),                    // :f
    Precision(u32, Node, PrecisionType), // :..2f or :..5s
}

impl Display for FormatSpecifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Debug(_) => write!(f, "?"),
            Self::LowerHex(_) => write!(f, "x"),
            Self::UpperHex(_) => write!(f, "X"),
            Self::Binary(_) => write!(f, "b"),
            Self::Float(_) => write!(f, "f"),
            Self::Precision(number, precision_type, _) => {
                write!(f, "{number}{precision_type}")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum PrecisionType {
    Float(Node),
    String(Node),
}

impl Display for PrecisionType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug)]
pub struct Module {
    statements: Vec<Statement>,
    pub definitions: Vec<Definition>,
}

impl Module {
    #[must_use]
    pub fn new(definitions: Vec<Definition>, statements: Vec<Statement>) -> Self {
        Self {
            statements,
            definitions,
        }
    }

    #[must_use]
    pub const fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    #[must_use]
    pub const fn definitions(&self) -> &Vec<Definition> {
        &self.definitions
    }
}
