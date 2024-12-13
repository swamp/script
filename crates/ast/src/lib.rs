/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use fixed32::Fp;
use seq_fmt::{comma, fmt_nl};
use seq_map::SeqMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;

// Common metadata that can be shared across all AST nodes
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Node {
    pub span: Span,
    // TODO: Add comments and attributes
}

impl Display for Node {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.span.start, self.span.end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, Default)]
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

    pub fn from_str(str: &str) -> LocalTypeIdentifier {
        Self {
            node: Node::default(),
            text: str.to_string(),
        }
    }
}

impl Display for LocalTypeIdentifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.text)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
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

    pub fn from_str(str: &str) -> Self {
        Self {
            node: Default::default(),
            text: str.to_string(),
        }
    }
}

impl Display for LocalIdentifier {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.text)
    }
}

impl Debug for LocalIdentifier {
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

impl Display for StructType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "struct {} {}", self.identifier, self.fields)
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

impl Display for Definition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Definition::StructDef(struct_def) => write!(f, "{struct_def}"),
            Definition::EnumDef(a, n) => write!(f, "{a} {n}"),
            Definition::FunctionDef(a, n) => write!(f, "{a} {n}"),
            Definition::ImplDef(a, n) => write!(f, "{a} {n}"),
            Definition::TypeAlias(a, n) => write!(f, "{a} {n}"),
            Definition::Import(a) => write!(f, "{a}"),
            Definition::Comment(a) => write!(f, "{a}"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ForVar {
    pub identifier: LocalTypeIdentifier,
    pub is_mut: bool,
}

impl Display for ForVar {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "mut:{} {}", self.is_mut, self.identifier)
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
            ForPattern::Single(v) => write!(f, "{v}"),
            ForPattern::Pair(a, b) => write!(f, "{a}, {b}"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    ForLoop(ForPattern, Expression, bool, Vec<Statement>),
    WhileLoop(Expression, Vec<Statement>),
    Return(Expression),
    Break,                  // Return with void
    Continue,               // Continue iterating in the closest loop
    Expression(Expression), // Used for expressions with side effects (mutation, i/o)
    Block(Vec<Statement>),
    If(Expression, Vec<Statement>, Option<Vec<Statement>>),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::ForLoop(a, b, c, d) => write!(f, "for {a} {b} {c} {}", fmt_nl(d)),
            Statement::WhileLoop(a, b) => write!(f, "while {a} {}", fmt_nl(b)),
            Statement::Return(a) => write!(f, "return {a}"),
            Statement::Break => write!(f, "break"),
            Statement::Continue => write!(f, "continue"),
            Statement::Expression(a) => write!(f, "{a}"),
            Statement::Block(a) => write!(f, "block {}", fmt_nl(a)),
            Statement::If(a, b, c) => {
                let stmts_str = c
                    .as_ref()
                    .map_or_else(String::new, |stmts| comma(stmts.as_slice()));
                write!(f, "if {a} {} {}", comma(b), stmts_str)
            }
        }
    }
}

#[derive(Clone, Eq, PartialEq, Default)]
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

#[derive(Clone, Eq, PartialEq, Default)]
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

impl Display for FunctionSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.name,
            comma(&*self.params),
            self.return_type
        )
    }
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

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Function::Internal(i) => write!(f, "{i}"),
            Function::External(e) => write!(f, "{e}"),
        }
    }
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

#[derive(Debug, Clone, PartialEq)]
pub enum CompoundOperator {
    Add, // +=
    Sub, // -=
    Mul, // *=
    Div, // /=
}

/// Expressions are things that "converts" to a value when evaluated.
#[derive(Debug, Clone)]
pub enum Expression {
    // Access / Lookup values
    FieldAccess(Box<Expression>, LocalIdentifier),
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
        LocalIdentifier,
        CompoundOperator,
        Box<Expression>,
    ),

    IndexAssignment(Box<Expression>, Box<Expression>, Box<Expression>), // target, index, source. Write to an index in an array or map: arr[3] = 42
    FieldAssignment(Box<Expression>, LocalIdentifier, Box<Expression>),

    // Operators ----
    BinaryOp(Box<Expression>, BinaryOperator, Box<Expression>),
    UnaryOp(UnaryOperator, Box<Expression>),

    // Postfix operators
    PostfixOp(PostfixOperator, Box<Expression>),

    // Calls ----
    FunctionCall(Box<Expression>, Vec<Expression>),
    StaticCall(LocalTypeIdentifier, LocalIdentifier, Vec<Expression>), // Type::func(args)
    StaticCallGeneric(
        LocalTypeIdentifier,
        LocalIdentifier,
        Vec<Expression>,
        Vec<Type>, // Generic arguments
    ),
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

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::FieldAccess(expr, field) => {
                write!(f, "[field_access {} {}]", expr, field)
            }
            Expression::VariableAccess(var) => {
                write!(f, "[variable_access {}]", var)
            }
            Expression::MutRef(_mut_ref) => {
                write!(f, "[mut ref]")
            }
            Expression::IndexAccess(target, idx) => {
                write!(f, "[index_access {}[{}]]", target, idx)
            }
            Expression::VariableAssignment(var, rhs) => {
                write!(f, "[variable_assignment {} <- {}]", var, rhs)
            }
            Expression::MultiVariableAssignment(vars, rhs) => {
                write!(f, "[multi_variable_assignment {} = {}]", comma(vars), rhs)
            }
            Expression::IndexCompoundAssignment(target, idx, op, rhs) => {
                write!(
                    f,
                    "[index_compound_assignment {}[{}] {}= {}]",
                    target, idx, op, rhs
                )
            }
            Expression::VariableCompoundAssignment(var, op, rhs) => {
                write!(f, "[variable_compound_assignment {} {}= {}]", var, op, rhs)
            }
            Expression::FieldCompoundAssignment(target, field, op, rhs) => {
                write!(
                    f,
                    "[field_compound_assignment {}.{} {}= {}]",
                    target, field, op, rhs
                )
            }
            Expression::IndexAssignment(target, idx, rhs) => {
                write!(f, "[index_assignment {}[{}] = {}]", target, idx, rhs)
            }
            Expression::FieldAssignment(target, field, rhs) => {
                write!(f, "[field_assignment {} <- {} = {}]", target, field, rhs)
            }
            Expression::BinaryOp(lhs, op, rhs) => {
                write!(f, "[binary_op {} {} {}]", lhs, op, rhs)
            }
            Expression::UnaryOp(op, expr) => {
                write!(f, "[unary_op {}{}]", op, expr)
            }
            Expression::PostfixOp(op, expr) => {
                write!(f, "[postfix_op {}{}]", expr, op)
            }
            Expression::FunctionCall(func, args) => {
                write!(f, "[function_call {}({})]", func, comma(args))
            }
            Expression::StaticCall(ty, method, args) => {
                write!(f, "[static_call {}::{}({})]", ty, method, comma(args))
            }
            Expression::StaticCallGeneric(ty, method, args, generics) => {
                write!(
                    f,
                    "[static_call_generic {}::{}::<{}>({})]",
                    ty,
                    method,
                    comma(generics),
                    comma(args)
                )
            }
            Expression::MemberCall(expr, method, args) => {
                write!(f, "[member_call {}.{}({})]", expr, method, comma(args))
            }
            Expression::Block(stmts) => {
                write!(f, "{{\n")?;
                for stmt in stmts {
                    writeln!(f, "    {}", stmt)?;
                }
                write!(f, "}}")
            }
            Expression::InterpolatedString(parts) => {
                // Assuming `StringPart` implements Display or to_string reasonably.
                let mut result = String::new();
                for part in parts {
                    result.push_str(&part.to_string());
                }
                write!(f, "\"{}\"", result)
            }
            Expression::StructInstantiation(qty, fields) => {
                write!(f, "[struct_instantiation {} {{\n", qty)?;
                for (name, expr) in fields.iter() {
                    writeln!(f, "    {}: {},", name, expr)?;
                }
                write!(f, "}}]")
            }
            Expression::ExclusiveRange(start, end) => {
                write!(f, "[{}..{}]", start, end)
            }
            Expression::Literal(lit) => {
                write!(f, "{}", lit)
            }
            Expression::IfElse(cond, then_expr, else_expr) => {
                write!(
                    f,
                    "[if_else {} {{ {} }} else {{ {} }}]",
                    cond, then_expr, else_expr
                )
            }
            Expression::Match(expr, arms) => {
                write!(f, "[match {} {{\n", expr)?;
                for arm in arms {
                    writeln!(f, "    {}", arm)?;
                }
                write!(f, "}}]")
            }
        }
    }
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
    Map(Vec<(Expression, Expression)>),
    Unit, // ()
    None, // none
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "{}", i),
            Literal::Float(fp) => write!(f, "{}", fp),
            Literal::String(s) => write!(f, "{}", s),
            Literal::Bool(b) => write!(f, "{}", b),
            Literal::EnumVariant(qty, variant, data) => {
                write!(f, "{}::{}{}", qty, variant, data)
            }
            Literal::Tuple(vals) => {
                // Tuple: (a, b, c)
                write!(f, "({})", comma(vals))
            }
            Literal::Array(vals) => {
                // Array: [a, b, c]
                write!(f, "[{}]", comma(vals))
            }
            Literal::Map(pairs) => {
                // Map: {a: 1, b: 2, c: 3}
                write!(f, "{{")?;
                for (i, (k, v)) in pairs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, "}}")
            }
            Literal::Unit => write!(f, "()"),
            Literal::None => write!(f, "none"),
        }
    }
}

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

impl Display for EnumLiteralData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EnumLiteralData::Nothing => {
                // No additional data is displayed.
                Ok(())
            }
            EnumLiteralData::Tuple(vals) => {
                // Tuple variant: (val1, val2, ...)
                write!(f, "({})", comma(vals))
            }
            EnumLiteralData::Struct(fields) => {
                // Struct variant: { field: val, field: val, ... }
                write!(f, "{{")?;
                for (i, (name, val)) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", name, val)?;
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Default)]
pub struct AnonymousStruct {
    pub fields: SeqMap<IdentifierName, Type>,
}

impl Display for AnonymousStruct {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{ {} }}", self.fields)
    }
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

impl Display for EnumVariant {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Simple => write!(f, ""),
            Self::Tuple(types) => write!(f, "{}", comma(types)),
            Self::Struct(anon_struct) => write!(f, "{anon_struct}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum Type {
    // Primitives
    Int,
    Float,
    String,
    Bool,
    #[default]
    Unit,
    Any,

    Generic(Box<Type>, Vec<Type>),

    //
    Struct(QualifiedTypeIdentifier), // TODO: Module support for name
    Array(Box<Type>),
    Map(Box<Type>, Box<Type>), // TODO: not implemented yet
    Tuple(Vec<Type>),
    Enum(QualifiedTypeIdentifier), // TODO: Module support

    //
    TypeReference(QualifiedTypeIdentifier), // Some Unknown Type Reference  // TODO: Module support
    Optional(Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::String => write!(f, "String"),
            Type::Bool => write!(f, "Bool"),
            Type::Unit => write!(f, "()"),
            Type::Any => write!(f, "ANY"),
            Type::Generic(a, b) => write!(f, "{a} {}", comma(b)),
            Type::Struct(a) => write!(f, "{a}"),
            Type::Array(t) => write!(f, "[{t}]"),
            Type::Map(a, b) => write!(f, "[{a}: {b}]"),
            Type::Tuple(types) => write!(f, "( {} )", comma(types)),
            Type::Enum(a) => write!(f, "{a}"),
            Type::TypeReference(a) => write!(f, "ref {a}"),
            Type::Optional(a) => write!(f, "{a}?"),
        }
    }
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

impl Display for CompoundOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            CompoundOperator::Add => "+=",
            CompoundOperator::Sub => "-=",
            CompoundOperator::Mul => "*=",
            CompoundOperator::Div => "/=",
        };
        write!(f, "{}", symbol)
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            BinaryOperator::Add => "+",
            BinaryOperator::Subtract => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::Modulo => "%",
            BinaryOperator::LogicalOr => "||",
            BinaryOperator::LogicalAnd => "&&",
            BinaryOperator::Equal => "==",
            BinaryOperator::NotEqual => "!=",
            BinaryOperator::LessThan => "<",
            BinaryOperator::LessEqual => "<=",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::GreaterEqual => ">=",
            BinaryOperator::RangeExclusive => "..",
        };
        write!(f, "{}", symbol)
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            UnaryOperator::Not => "!",
            UnaryOperator::Negate => "-",
        };
        write!(f, "{}", symbol)
    }
}

impl Display for PostfixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            PostfixOperator::Unwrap => "?", // or "!" depending on your language design
        };
        write!(f, "{}", symbol)
    }
}

// Patterns are used in matching and destructuring
#[derive(Debug, Clone)]
pub enum Pattern {
    PatternList(Vec<PatternElement>), // TODO: Change to SetVec
    EnumPattern(LocalTypeIdentifier, Option<Vec<PatternElement>>), // TODO: Change to SetVec
    Literal(Literal),
}

#[derive(Debug, Clone)]
pub enum PatternElement {
    Variable(LocalIdentifier),
    Expression(Expression),
    Wildcard,
}

impl Display for StringPart {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StringPart::Literal(s) => write!(f, "{}", s),
            StringPart::Interpolation(expr, Some(format_spec)) => {
                write!(f, "{{{}:{}}}", expr, format_spec)
            }
            StringPart::Interpolation(expr, None) => write!(f, "{{{}}}", expr),
        }
    }
}

impl Display for MatchArm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Typically match arms are displayed as `pattern => expression`
        write!(f, "{} => {}", self.pattern, self.expression)
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::PatternList(elements) => {
                // (a, b, c) style tuple pattern
                write!(f, "({})", comma(elements))
            }
            Pattern::EnumPattern(ty, maybe_elements) => {
                // EnumName(...) if elements present, else just EnumName
                if let Some(elements) = maybe_elements {
                    write!(f, "{}({})", ty, comma(elements))
                } else {
                    write!(f, "{}", ty)
                }
            }
            Pattern::Literal(lit) => {
                write!(f, "{}", lit)
            }
        }
    }
}

impl Display for PatternElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PatternElement::Variable(var) => write!(f, "{}", var),
            PatternElement::Expression(expr) => write!(f, "{}", expr),
            PatternElement::Wildcard => write!(f, "_"),
        }
    }
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
            writeln!(f, "{definition}")?;
        }

        if !self.definitions.is_empty() && !self.statements().is_empty() {
            write!(f, "\n---\n")?;
        }

        for statement in &self.statements {
            writeln!(f, "{statement}")?;
        }
        Ok(())
    }
}
