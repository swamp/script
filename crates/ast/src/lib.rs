/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use seq_map::SeqMap;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};

#[derive(Clone)]
pub struct Variable {
    pub name: String,
    pub is_mutable: bool,
}

#[derive(Debug, Clone)]
pub struct MutVariableRef(pub Variable); // Just wraps a variable when passed with mut keyword

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

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn immutable(name: &str) -> Self {
        Self::new(name, false)
    }

    pub fn mutable(name: &str) -> Self {
        Self::new(name, true)
    }
}

#[derive(Clone)]
pub struct ScopedIdentifier(pub String);
impl ScopedIdentifier {
    pub fn new(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl Debug for ScopedIdentifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for ScopedIdentifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
pub struct LocalIdentifier(pub String); // pub is probably better for performance

impl LocalIdentifier {
    pub fn new(s: &str) -> Self {
        Self(s.to_string())
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub struct ScopedTypeIdentifier(pub String); // pub is probably better for performance

impl ScopedTypeIdentifier {
    pub fn new(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl Debug for ScopedTypeIdentifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for ScopedTypeIdentifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub struct LocalTypeIdentifier(pub String); // pub is probably better for performance

impl LocalTypeIdentifier {
    pub fn new(s: &str) -> Self {
        LocalTypeIdentifier(s.to_string())
    }
}

impl Debug for LocalTypeIdentifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for LocalTypeIdentifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone)]
pub struct StringConst(pub String); // pub is probably better for performance

impl StringConst {
    pub fn new(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl Debug for StringConst {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct Import {
    pub module_path: Vec<LocalTypeIdentifier>, // For handling paths like "geometry.shapes"
    pub items: ImportItems,
}

#[derive(Debug, Clone)]
pub enum ImportItems {
    Module, // import the whole module (use last path item as namespace)
    Specific(Vec<LocalTypeIdentifier>), // import { sin, cos } from math
}

#[derive(Debug, Clone)]
pub enum Definition {
    StructDef(LocalTypeIdentifier, SeqMap<LocalTypeIdentifier, Type>),
    EnumDef(
        LocalTypeIdentifier,
        SeqMap<LocalTypeIdentifier, EnumVariant>,
    ),
    FunctionDef(LocalTypeIdentifier, FunctionData),
    ImplDef(LocalTypeIdentifier, SeqMap<LocalTypeIdentifier, ImplItem>),
    ExternalFunctionDef(ScopedIdentifier, FunctionData),
    Import(Import),
    // Other
    Comment(String),
}

#[derive(Debug, Clone)]
pub enum Statement {
    // Standard
    Let(Pattern, Expression),
    ForLoop(Pattern, Expression, Vec<Statement>),
    WhileLoop(Expression, Vec<Statement>),
    Return(Expression),
    Break,                  // Return with void
    Continue,               //
    Expression(Expression), // Used for expressions with side effects (mutation, i/o)
    Block(Vec<Statement>),  // TODO: Feels a bit sketchy
    If(Expression, Vec<Statement>, Option<Vec<Statement>>),
}

#[derive(Debug, Clone)]
pub enum ImplItem {
    Member(ImplMember),
    Function(FunctionData),
}

#[derive(Clone)]
pub struct ImplMember {
    pub self_param: SelfParameter,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub body: Vec<Statement>,
}

impl Debug for SelfParameter {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.is_mutable {
            write!(f, "mut self")
        } else {
            write!(f, "self")
        }
    }
}

impl Debug for ImplMember {
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

#[derive(Clone)]
pub struct FunctionData {
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub body: Vec<Statement>,
}

impl Debug for FunctionData {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{:?}, {:?}, {:?}",
            self.params, self.return_type, self.body
        )
    }
}

impl Display for FunctionData {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{:?}, {:?}, {:?}",
            self.params, self.return_type, self.body
        )
    }
}

// Used for function definitions
#[derive(Clone)]
pub struct Parameter {
    pub variable: Variable,
    pub param_type: Type,
    pub is_mutable: bool,
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

/// Expressions are things that "converts" to a value when being evaluated.
#[derive(Debug, Clone)]
pub enum Expression {
    // Lookup values
    FieldAccess(Box<Expression>, LocalTypeIdentifier),
    Variable(Variable),
    MutRef(MutVariableRef), // Used when passing with mut keyword. mut are implicitly passed by reference
    ArrayAccess(Box<Expression>, Box<Expression>), // Read from an array: arr[3]
    ArrayAssignment(Box<Expression>, Box<Expression>, Box<Expression>), // target, index, source. Write to an index in an array: arr[3] = 42

    // Since it is a cool language, we can "chain" assignments together. like a = b = c = 1. Even for field assignments, like a.b = c.d = e.f = 1
    Assignment(Box<Expression>, Box<Expression>),
    FieldAssignment(Box<Expression>, LocalTypeIdentifier, Box<Expression>),

    // Operators
    BinaryOp(Box<Expression>, BinaryOperator, Box<Expression>),
    UnaryOp(UnaryOperator, Box<Expression>),

    // Calls
    FunctionCall(Box<Expression>, Vec<Expression>),
    MemberCall(Box<Expression>, LocalTypeIdentifier, Vec<Expression>),
    Block(Vec<Statement>),

    InterpolatedString(Vec<StringPart>),

    // Constructing
    StructInstantiation(
        ScopedTypeIdentifier,
        SeqMap<LocalTypeIdentifier, Expression>,
    ),
    Array(Vec<Expression>),
    Tuple(Vec<Expression>),
    Map(HashMap<Expression, Expression>), // Not implemented yet. Maybe call this a dictionary or similar, to avoid confusion with map()
    ExclusiveRange(Box<Expression>, Box<Expression>),
    Literal(Literal),

    // Comparing
    IfElse(Box<Expression>, Box<Expression>, Box<Expression>),
    Match(Box<Expression>, Vec<MatchArm>),
}

// Used for match expression
#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expression: Expression,
}

// Are constructed by themselves
#[derive(Clone)]
pub enum Literal {
    Int(i32),
    Float(f32), // TODO: Change to fixed point 32 bit
    String(StringConst),
    Bool(bool),
    EnumVariant(ScopedTypeIdentifier, LocalTypeIdentifier, EnumLiteralData), // EnumTypeName::Identifier tuple|struct
    Tuple(Vec<Expression>),
    Unit, // ()
}

pub fn seq_map_to_string<K, V>(map: &SeqMap<K, V>) -> String
where
    K: Debug + std::cmp::Eq + std::hash::Hash + std::clone::Clone,
    V: Debug,
{
    let entries = map
        .iter()
        .map(|(k, v)| format!("{:?}: {:?}", k, v)) // Format each key-value pair
        .collect::<Vec<String>>() // Collect into a Vec of Strings
        .join(", "); // Join with a comma and space

    entries.trim().to_string()
}

impl Debug for Literal {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::EnumVariant(enum_type, variant_name, data) => match data {
                EnumLiteralData::Nothing => {
                    write!(f, "EnumVariant({}::{})", enum_type, variant_name)
                }
                EnumLiteralData::Tuple(exprs) => write!(
                    f,
                    "EnumVariant({}::{}({:?}))",
                    enum_type, variant_name, exprs
                ),
                EnumLiteralData::Struct(exprs) => write!(
                    f,
                    "EnumVariant({}::{}{{ {} }})",
                    enum_type,
                    variant_name,
                    seq_map_to_string(exprs)
                ),
            },

            Literal::Int(v) => write!(f, "Int({})", v),
            Literal::Float(v) => write!(f, "Float({})", v),
            Literal::String(v) => write!(f, "String({})", v.0),
            Literal::Bool(v) => write!(f, "Bool({})", v),
            Literal::Tuple(v) => write!(f, "Tuple({v:?})"),
            Literal::Unit => write!(f, "()"),
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
            Self::Tuple(types) => write!(f, "{:?}", types),
            Self::Struct(types) => write!(f, "{:?}", types),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum EnumVariant {
    Simple,
    Tuple(Vec<Type>),
    Struct(SeqMap<LocalTypeIdentifier, Type>),
}

impl Debug for EnumVariant {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Simple => write!(f, ""),
            Self::Tuple(types) => write!(f, "{:?}", types),
            Self::Struct(types) => write!(f, "{:?}", types),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Float,
    String,
    Bool,
    Struct(ScopedTypeIdentifier), // TODO: Module support for name
    Array(Box<Type>),
    Map(Box<Type>, Box<Type>), // TODO: not implemented yet
    Void,
    Tuple(Vec<Type>),
    Enum(ScopedTypeIdentifier), // TODO: Module support
    Any,
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

// Patterns are used in matching and destructuring
#[derive(Debug, Clone)]
pub enum Pattern {
    // Just the normal identifier
    Variable(Variable),

    Tuple(Vec<LocalTypeIdentifier>),  // Change to SetVec
    Struct(Vec<LocalTypeIdentifier>), // Change to SetVec

    Literal(Literal),

    // Enum variants
    EnumTuple(LocalTypeIdentifier, Vec<LocalTypeIdentifier>), // Change to SetVec
    EnumStruct(LocalTypeIdentifier, Vec<LocalTypeIdentifier>), // Change to SetVec
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

#[derive(Debug, Clone)]
pub enum PrecisionType {
    Float,
    String,
}

#[derive(Debug)]
pub struct Program {
    statements: Vec<Statement>,
    definitions: Vec<Definition>,
}

impl Program {
    pub fn new(definitions: Vec<Definition>, statements: Vec<Statement>) -> Self {
        Self {
            statements,
            definitions,
        }
    }

    pub fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn definitions(&self) -> &Vec<Definition> {
        &self.definitions
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for definition in &self.definitions {
            write!(f, "{:?}\n", definition)?;
        }

        if !self.definitions.is_empty() && !self.statements().is_empty() {
            write!(f, "\n---\n")?;
        }

        for statement in &self.statements {
            write!(f, "{:?}\n", statement)?;
        }
        Ok(())
    }
}
