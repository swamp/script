/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use fixed32::Fp;
use seq_map::SeqMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;

// Common metadata that can be shared across all AST nodes
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Node {
    pub span: Span,
    // TODO: Add comments and attributes
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.span.start, self.span.end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Position {
    pub offset: usize, // Octet offset into file
    pub line: usize,   // 0-based line number
    pub column: usize, // 0-based column number
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct QualifiedTypeIdentifier {
    pub name: LocalTypeIdentifier,
    pub module_path: Option<ModulePath>,
}

impl QualifiedTypeIdentifier {
    pub fn new(name: LocalTypeIdentifier, module_path: Vec<String>) -> Self {
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
        write!(f, "{}", self.name.text)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LocalTypeIdentifier {
    pub node: Node,
    pub text: String,
}

impl LocalTypeIdentifier {
    pub fn new(node: Node, str: &str) -> Self {
        Self {
            node,
            text: str.to_string(),
        }
    }
}

impl Display for LocalTypeIdentifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.text)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct LocalIdentifier {
    pub node: Node,
    pub text: String,
}

impl LocalIdentifier {
    #[must_use]
    pub fn new(node: Node, str: &str) -> Self {
        Self {
            node,
            text: str.to_string(),
        }
    }
}

impl Display for LocalIdentifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{} <{}>", self.text, self.node)
    }
}

#[derive(Clone)]
pub struct StringConst(pub String);

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
pub struct ModulePath(pub Vec<String>);

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

#[derive(Debug, Clone)]
pub enum ImportItems {
    Module, // import the whole module (use last path item as namespace)
    Specific(Vec<LocalTypeIdentifier>), // import { sin, cos } from math
}

#[derive(Clone, Debug)]
pub struct StructType {
    pub identifier: LocalTypeIdentifier,
    pub fields: SeqMap<IdentifierName, Type>,
}

impl StructType {
    #[must_use]
    pub const fn new(
        identifier: LocalTypeIdentifier,
        fields: SeqMap<IdentifierName, Type>,
    ) -> Self {
        Self { identifier, fields }
    }
}

#[derive(Debug, Clone)]
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
    Comment(String),
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(Pattern, Expression),
    ForLoop(Pattern, Expression, Vec<Statement>),
    WhileLoop(Expression, Vec<Statement>),
    Return(Expression),
    Break,                  // Return with void
    Continue,               // Continue iterating in the closest loop
    Expression(Expression), // Used for expressions with side effects (mutation, i/o)
    Block(Vec<Statement>),
    If(Expression, Vec<Statement>, Option<Vec<Statement>>),
}

#[derive(Clone)]
pub struct Variable {
    pub name: String,
    pub is_mutable: bool,
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.is_mutable {
            write!(f, "mut {}", self.name)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

impl Debug for Variable {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.is_mutable {
            write!(f, "mut {}", self.name)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

impl Variable {
    pub fn new(name: &str, is_mutable: bool) -> Self {
        Self {
            name: name.to_string(),
            is_mutable,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MutVariableRef(pub Variable); // Just wraps a variable when passed with mut keyword

#[derive(Clone)]
pub struct Parameter {
    pub variable: Variable,
    pub param_type: Type,
    pub is_mutable: bool,
    pub is_self: bool,
}

impl Display for Parameter {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {:?}", self.variable, self.param_type)
    }
}

impl Debug for Parameter {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}: {:?}", self.variable, self.param_type)
    }
}

#[derive(Clone, Debug)]
pub struct FunctionSignature {
    pub name: LocalIdentifier,
    pub params: Vec<Parameter>, // first param can be self (check is_self)
    pub return_type: Type,
}

#[derive(Clone)]
pub struct FunctionData {
    pub signature: FunctionSignature,
    pub body: Vec<Statement>,
}

impl Display for FunctionData {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}, {:?}", self.signature, self.body)
    }
}

#[derive(Debug, Clone)]
pub enum Function {
    Internal(FunctionData),
    External(FunctionSignature),
}

#[derive(Debug, Clone)]
pub enum ImplItem {
    Member(ImplMember),
    Function(ImplFunction),
}

#[derive(Clone)]
pub enum ImplMember {
    Internal(ImplMemberData),
    External(ImplMemberSignature),
}

#[derive(Clone, Debug)]
pub struct ImplMemberSignature {
    pub name: LocalIdentifier,
    pub self_param: SelfParameter,
    pub params: Vec<Parameter>,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
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

#[derive(Clone)]
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
        if self.is_mutable {
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
    pub is_mutable: bool,
}

impl Debug for FunctionData {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:?}, {:?}", self.signature, self.body)
    }
}

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct IdentifierName(pub String);

impl Display for IdentifierName {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Expressions are things that "converts" to a value when evaluated.
#[derive(Debug, Clone)]
pub enum Expression {
    // Access / Lookup values
    FieldAccess(Box<Expression>, LocalIdentifier),
    VariableAccess(Variable),
    MutRef(MutVariableRef), // Used when passing with mut keyword. mut are implicitly passed by reference
    ArrayAccess(Box<Expression>, Box<Expression>), // Read from an array: arr[3]

    // Assignment ----

    // Since it is a cool language, we can "chain" assignments together. like a = b = c = 1. Even for field assignments, like a.b = c.d = e.f = 1
    VariableAssignment(Variable, Box<Expression>),
    ArrayAssignment(Box<Expression>, Box<Expression>, Box<Expression>), // target, index, source. Write to an index in an array: arr[3] = 42
    FieldAssignment(Box<Expression>, LocalIdentifier, Box<Expression>),

    // Operators ----
    BinaryOp(Box<Expression>, BinaryOperator, Box<Expression>),
    UnaryOp(UnaryOperator, Box<Expression>),

    // Postfix operators
    PostfixOp(PostfixOperator, Box<Expression>),

    // Calls ----
    FunctionCall(Box<Expression>, Vec<Expression>),
    StaticCall(LocalTypeIdentifier, LocalIdentifier, Vec<Expression>), // Type::func(args)
    MemberCall(Box<Expression>, LocalIdentifier, Vec<Expression>),
    Block(Vec<Statement>),

    InterpolatedString(Vec<StringPart>),

    // Constructing
    StructInstantiation(QualifiedTypeIdentifier, SeqMap<IdentifierName, Expression>),
    ExclusiveRange(Box<Expression>, Box<Expression>),
    Literal(Literal),

    // Comparing
    IfElse(Box<Expression>, Box<Expression>, Box<Expression>),
    Match(Box<Expression>, Vec<MatchArm>),
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expression: Expression,
}

// Are constructed by themselves
#[derive(Clone)]
pub enum Literal {
    Int(i32),
    Float(Fp),
    String(StringConst),
    Bool(bool),
    EnumVariant(
        QualifiedTypeIdentifier,
        LocalTypeIdentifier,
        EnumLiteralData,
    ), // EnumTypeName::Identifier tuple|struct
    Tuple(Vec<Expression>),
    Array(Vec<Expression>),
    Map(SeqMap<IdentifierName, Expression>),
    Unit, // ()
    None, // none
}

/*

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Literal::Int(value) => write!(f, "{}", value),
            Literal::Float(value) => write!(f, "{}", value),
            Literal::String(value) => write!(f, "{}", value),
            Literal::Bool(value) => write!(f, "{}", value),
            Literal::EnumVariant(a, b, c) => write!(f, "{} {} {}", a, b, c),
            Literal::Tuple(value) => write!(f, "{}", value),
            Literal::Array(value) => write!(f, "{}", value),
            Literal::Map(value) => write!(f, "{}", value),
            Literal::Unit => write!(f, "()"),
        }
    }
}

 */

/*
impl Debug for Literal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Literal::Int(value) => write!(f, "{}", value),
            Literal::Float(value) => write!(f, "{}", value),
            Literal::String(value) => write!(f, "{}", value),
            Literal::Bool(value) => write!(f, "{}", value),
            Literal::EnumVariant(a, b, c) => write!(f, "{} {} {:?}", a, b, c),
            Literal::Tuple(value) => write!(f, "{}", value),
            Literal::Array(value) => write!(f, "{}", value),
            Literal::Map(value) => write!(f, "{}", value),
            Literal::Unit => write!(f, "()"),
        }
    }
}
*/

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
                    "EnumVariant({}::{}{{ {} }})",
                    enum_type,
                    variant_name,
                    seq_map_to_string(expressions)
                ),
            },

            Self::Int(v) => write!(f, "Int({v})"),
            Self::Float(v) => write!(f, "Float({v})"),
            Self::String(v) => write!(f, "String({})", v.0),
            Self::Bool(v) => write!(f, "Bool({v})"),
            Self::Tuple(v) => write!(f, "Tuple({v:?})"),
            Self::Unit => write!(f, "()"),
            Self::None => write!(f, "none"),
            Self::Array(v) => write!(f, "Array({v:?})"),
            Self::Map(v) => write!(f, "Map({v:?})"),
        }
    }
}

#[derive(Clone)]
pub enum EnumLiteralData {
    Nothing,
    Tuple(Vec<Expression>),
    Struct(SeqMap<LocalTypeIdentifier, Expression>),
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

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct AnonymousStruct {
    pub fields: SeqMap<IdentifierName, Type>,
}

impl AnonymousStruct {
    #[must_use]
    pub const fn new(fields: SeqMap<IdentifierName, Type>) -> Self {
        Self { fields }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum EnumVariant {
    Simple,
    Tuple(Vec<Type>),
    Struct(AnonymousStruct),
}

impl Debug for EnumVariant {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Simple => write!(f, ""),
            Self::Tuple(types) => write!(f, "{types:?}"),
            Self::Struct(anon_struct) => write!(f, "{anon_struct:?}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    // Primitives
    Int,
    Float,
    String,
    Bool,
    Unit,
    Any,

    //
    Struct(QualifiedTypeIdentifier), // TODO: Module support for name
    Array(Box<Type>),
    Map(Box<Type>, Box<Type>), // TODO: not implemented yet
    Tuple(Vec<Type>),
    Enum(QualifiedTypeIdentifier), // TODO: Module support

    //
    TypeReference(QualifiedTypeIdentifier), // Some Unknown Type Reference  // TODO: Module support
}

// Takes a left and right side expression
#[derive(Debug, Clone)]
pub enum BinaryOperator {
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
    LessThanOrEqual,
    GreaterThanOrEqual,
    RangeExclusive,
}

// Only takes one expression argument
#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Not,
    Negate,
}

// Only takes one expression argument
#[derive(Debug, Clone)]
pub enum PostfixOperator {
    Unwrap, // option_operator
}

// Patterns are used in matching and destructuring
#[derive(Debug, Clone)]
pub enum Pattern {
    // Just the normal identifier
    VariableAssignment(Variable),

    // Containers
    Tuple(Vec<LocalTypeIdentifier>),  // Change to SetVec
    Struct(Vec<LocalTypeIdentifier>), // Change to SetVec

    Literal(Literal),

    // Enum variants
    EnumTuple(LocalTypeIdentifier, Vec<LocalIdentifier>), // Change to SetVec
    EnumStruct(LocalTypeIdentifier, Vec<LocalIdentifier>), // Change to SetVec
    EnumSimple(LocalTypeIdentifier),

    // Other
    Wildcard, // underscore _
}

#[derive(Debug, Clone)]
pub enum StringPart {
    Literal(String),
    Interpolation(Box<Expression>, Option<FormatSpecifier>),
}

#[derive(Debug, Clone)]
pub enum FormatSpecifier {
    Debug,                         // :?
    LowerHex,                      // :x
    UpperHex,                      // :X
    Binary,                        // :b
    Float,                         // :f
    Precision(u32, PrecisionType), // :..2f or :..5s
}

impl Display for FormatSpecifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Debug => write!(f, "?"),
            Self::LowerHex => write!(f, "x"),
            Self::UpperHex => write!(f, "X"),
            Self::Binary => write!(f, "b"),
            Self::Float => write!(f, "f"),
            Self::Precision(number, precision_type) => {
                write!(f, "{number}{precision_type}")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum PrecisionType {
    Float,
    String,
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

impl Display for Module {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for definition in &self.definitions {
            writeln!(f, "{definition:?}")?;
        }

        if !self.definitions.is_empty() && !self.statements().is_empty() {
            write!(f, "\n---\n")?;
        }

        for statement in &self.statements {
            writeln!(f, "{statement:?}")?;
        }
        Ok(())
    }
}
