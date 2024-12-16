/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use seq_map::SeqMap;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::rc::Rc;
use swamp_script_node::SpanWithoutFileId;

// Common metadata that can be shared across all AST nodes
#[derive(PartialEq, Eq, Hash, Default)]
pub struct Node {
    pub span: SpanWithoutFileId,
    // TODO: Add comments and attributes
}

impl Debug for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.span)
    }
}

/// Identifiers ================
#[derive(Debug, PartialEq, Eq, Hash)]
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

#[derive(Debug, PartialEq, Eq, Hash, Default)]
pub struct LocalTypeIdentifier(pub Node);

impl LocalTypeIdentifier {
    pub fn new(node: Node) -> Self {
        Self(node)
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct LocalIdentifier(pub Node);

impl LocalIdentifier {
    #[must_use]
    pub fn new(node: Node) -> Self {
        Self(node)
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct MemberFunctionIdentifier(pub Node);

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct IdentifierName(pub Node);

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct FieldName(pub Node);

// =========================

#[derive()]
pub struct StringConst(pub Node);

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct ModulePathItem {
    pub node: Node,
}

#[derive(Debug, Eq, Hash, PartialEq)]
pub struct ModulePath(pub Vec<ModulePathItem>);

impl ModulePath {
    pub fn new() -> Self {
        Self(vec![])
    }
}

#[derive(Debug)]
pub struct Import {
    pub module_path: ModulePath,
    pub items: ImportItems,
}

#[derive(Debug)]
pub enum ImportItems {
    Module, // import the whole module (use last path item as namespace)
    Specific(Vec<LocalTypeIdentifier>), // import { sin, cos } from math
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct StructType {
    pub identifier: LocalTypeIdentifier,
    pub fields: Vec<AnonymousStructTypeField>,
}

impl StructType {
    #[must_use]
    pub const fn new(
        identifier: LocalTypeIdentifier,
        fields: Vec<AnonymousStructTypeField>,
    ) -> Self {
        Self { identifier, fields }
    }
}

#[derive(Debug)]
pub struct EnumVariantWithData {
    pub identifier: LocalTypeIdentifier,
    pub variant: EnumVariant,
}

#[derive(Debug)]
pub enum Definition {
    StructDef(StructType),

    EnumDef(LocalTypeIdentifier, Vec<EnumVariantWithData>),

    FunctionDef(Function),
    ImplDef(LocalTypeIdentifier, Vec<Function>),
    TypeAlias(LocalTypeIdentifier, Type),
    Import(Import),
    // Other
    Comment(Node),
}

#[derive(Debug)]
pub struct ForVar {
    pub identifier: LocalIdentifier,
    pub is_mut: Option<Node>,
}

#[derive(Debug)]
pub enum ForPattern {
    Single(ForVar),
    Pair(ForVar, ForVar),
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

#[derive(Eq, PartialEq)]
pub struct VariableNotMut {
    pub name: LocalIdentifier,
}

#[derive(Eq, PartialEq)]
pub struct Variable {
    pub name: Node,
    pub is_mutable: Option<Node>,
}

impl Variable {
    pub fn new(name: Node, is_mutable: Option<Node>) -> Self {
        Self { name, is_mutable }
    }
}

// Since this is a helper struct, we want to implement the debug output for it
// to have it more concise
impl Debug for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(found) = &self.is_mutable {
            write!(f, "mut {found:?} {:?}", self.name)
        } else {
            write!(f, "{:?}", self.name)
        }
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

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct ImplMemberData {
    pub name: LocalIdentifier,
    pub self_param: SelfParameter,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub body: Vec<Statement>, // Will be empty for external members
}

pub type ImplMemberRef = Rc<ImplMember>;

#[derive(Debug)]
pub struct SelfParameter {
    pub is_mutable: Option<Node>,
    pub self_node: Node,
}

#[derive(Debug, PartialEq)]
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
    StructInstantiation(QualifiedTypeIdentifier, Vec<AnonymousStructField>),
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
#[derive(Debug)]
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

#[derive(Debug)]
pub struct AnonymousStructField {
    pub field_name: FieldName,
    pub expression: Expression,
}

#[derive(Debug, Eq, PartialEq)]
pub struct AnonymousStructTypeField {
    pub field_name: FieldName,
    pub field_type: Type,
}

#[derive(Debug)]
pub enum EnumLiteralData {
    Nothing,
    Tuple(Vec<Expression>),
    Struct(Vec<AnonymousStructField>),
}

#[derive(Debug, Default)]
pub struct AnonymousStructType {
    pub fields: Vec<AnonymousStructTypeField>,
}

#[derive(Debug)]
pub enum EnumVariant {
    Simple(Node),
    Tuple(Vec<Type>),
    Struct(AnonymousStructType),
}

#[derive(Debug, PartialEq, Eq, Hash)]
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

// Takes a left and right side expression
#[derive(Debug)]
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
#[derive(Debug)]
pub enum UnaryOperator {
    Not(Node),
    Negate(Node),
}

// Only takes one expression argument
#[derive(Debug)]
pub enum PostfixOperator {
    Unwrap(Node), // option_operator
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

#[derive(Debug)]
pub enum FormatSpecifier {
    Debug(Node),                         // :?
    LowerHex(Node),                      // :x
    UpperHex(Node),                      // :X
    Binary(Node),                        // :b
    Float(Node),                         // :f
    Precision(u32, Node, PrecisionType), // :..2f or :..5s
}

#[derive(Debug)]
pub enum PrecisionType {
    Float(Node),
    String(Node),
}

#[derive()]
pub struct Module {
    statements: Vec<Statement>,
    pub definitions: Vec<Definition>,
}

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for definition in &self.definitions {
            writeln!(f, "{definition:?}")?;
        }

        if !self.definitions.is_empty() {
            writeln!(f, "---")?;
        }

        for statement in &self.statements {
            writeln!(f, "{statement:?}")?;
        }

        Ok(())
    }
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
