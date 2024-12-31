/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use std::fmt;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::rc::Rc;

#[derive(PartialEq, Eq, Hash, Default, Clone)]
pub struct SpanWithoutFileId {
    pub offset: u32,
    pub length: u16,
}

impl Debug for SpanWithoutFileId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}:{}>", self.offset, self.length)
    }
}

// Common metadata that can be shared across all AST nodes
#[derive(PartialEq, Eq, Hash, Default, Clone)]
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
    pub generic_params: Vec<Type>,
}

impl QualifiedTypeIdentifier {
    pub fn new(name: LocalTypeIdentifier, module_path: Vec<Node>) -> Self {
        let module_path = if module_path.is_empty() {
            None
        } else {
            Some(ModulePath(module_path))
        };

        Self {
            name,
            module_path,
            generic_params: Vec::new(),
        }
    }

    pub fn new_with_generics(
        name: LocalTypeIdentifier,
        module_path: Vec<Node>,
        generic_params: Vec<Type>,
    ) -> Self {
        let module_path = if module_path.is_empty() {
            None
        } else {
            Some(ModulePath(module_path))
        };

        Self {
            name,
            module_path,
            generic_params,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct QualifiedIdentifier {
    pub name: Node,
    pub module_path: Option<ModulePath>,
}

impl QualifiedIdentifier {
    pub fn new(name: Node, module_path: Vec<Node>) -> Self {
        let module_path = if module_path.is_empty() {
            None
        } else {
            Some(ModulePath(module_path))
        };

        Self { name, module_path }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Default, Clone)]
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

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
pub struct ModulePath(pub Vec<Node>);

impl ModulePath {
    pub fn new() -> Self {
        Self(vec![])
    }
}

#[derive(Debug)]
pub enum UseItem {
    Identifier(LocalIdentifier),
    Type(LocalTypeIdentifier),
}

#[derive(Debug)]
pub struct Use {
    pub module_path: ModulePath,
    pub assigned_path: Vec<String>,
    pub items: Vec<UseItem>,
}

#[derive(Debug, Eq, PartialEq, Default)]
pub struct StructType {
    pub identifier: LocalTypeIdentifier,
    pub fields: Vec<FieldType>,
}

impl StructType {
    #[must_use]
    pub const fn new(identifier: LocalTypeIdentifier, fields: Vec<FieldType>) -> Self {
        Self { identifier, fields }
    }
}

#[derive(Debug)]
pub enum Definition {
    StructDef(StructType),
    EnumDef(Node, Vec<EnumVariantType>),
    FunctionDef(Function),
    ImplDef(Node, Vec<Function>),
    Use(Use),

    // Other
    Comment(Node),
}

#[derive(Debug)]
pub struct ForVar {
    pub identifier: Node,
    pub is_mut: Option<Node>,
}

#[derive(Debug)]
pub enum ForPattern {
    Single(ForVar),
    Pair(ForVar, ForVar),
}

#[derive(Debug)]
pub struct IteratableExpression {
    pub expression: Box<Expression>,
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

#[derive(Debug, Eq, PartialEq)]
pub struct Parameter {
    pub variable: Variable,
    pub param_type: Type,
    //    pub is_mutable: Option<Node>,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: Node,
    pub params: Vec<Parameter>,
    pub self_parameter: Option<SelfParameter>,
    pub return_type: Option<Type>,
}

#[derive(Debug)]
pub struct FunctionWithBody {
    pub declaration: FunctionDeclaration,
    pub body: Expression,
}

#[derive(Debug)]
pub enum Function {
    Internal(FunctionWithBody),
    External(FunctionDeclaration),
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
    Internal(FunctionWithBody),
    External(FunctionDeclaration),
}

#[derive(Debug)]
pub struct ImplMemberData {
    pub name: LocalIdentifier,
    pub self_param: SelfParameter,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub body: Vec<Expression>, // Will be empty for external members
}

pub type ImplMemberRef = Rc<ImplMember>;

#[derive(Debug)]
pub struct SelfParameter {
    pub is_mutable: Option<Node>,
    pub self_node: Node,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CompoundOperatorKind {
    Add, // +=
    Sub, // -=
    Mul, // *=
    Div, // /=
}

#[derive(Debug)]
pub struct CompoundOperator {
    pub node: Node,
    pub kind: CompoundOperatorKind,
}

#[derive(Debug)]
pub enum LocationExpression {
    Variable(Variable),
    IndexAccess(Box<Expression>, Box<Expression>), // TODO: Not supported yet
    FieldAccess(Box<Expression>, Node),            // TODO: Not supported yet
}

/// Expressions are things that "converts" to a value when evaluated.
#[derive(Debug)]
pub enum Expression {
    // Access
    FieldAccess(Box<Expression>, Node),
    VariableAccess(Variable),
    FunctionAccess(QualifiedIdentifier),

    MutRef(LocationExpression),
    IndexAccess(Box<Expression>, Box<Expression>),

    // Assignments
    VariableAssignment(Variable, Box<Expression>),
    VariableCompoundAssignment(Node, CompoundOperator, Box<Expression>),
    MultiVariableAssignment(Vec<Variable>, Box<Expression>),
    IndexAssignment(Box<Expression>, Box<Expression>, Box<Expression>),
    IndexCompoundAssignment(
        Box<Expression>,
        Box<Expression>,
        CompoundOperator,
        Box<Expression>,
    ),
    FieldAssignment(Box<Expression>, Node, Box<Expression>),
    FieldCompoundAssignment(Box<Expression>, Node, CompoundOperator, Box<Expression>),

    // Operators
    BinaryOp(Box<Expression>, BinaryOperator, Box<Expression>),
    UnaryOp(UnaryOperator, Box<Expression>),

    // Calls
    FunctionCall(Box<Expression>, Vec<Expression>),
    StaticCall(QualifiedTypeIdentifier, Node, Vec<Expression>),
    StaticCallGeneric(QualifiedTypeIdentifier, Node, Vec<Expression>),
    MemberCall(Box<Expression>, Node, Vec<Expression>),

    Block(Vec<Expression>),

    // Control flow
    ForLoop(ForPattern, IteratableExpression, Box<Expression>),
    WhileLoop(Box<Expression>, Box<Expression>),
    Return(Option<Box<Expression>>),
    Break(Node),
    Continue(Node),

    // Compare and Matching
    If(Box<Expression>, Box<Expression>, Option<Box<Expression>>),
    Match(Box<Expression>, Vec<MatchArm>),

    InterpolatedString(Vec<StringPart>),

    // Instantiation
    StructInstantiation(QualifiedTypeIdentifier, Vec<FieldExpression>),
    ExclusiveRange(Box<Expression>, Box<Expression>),
    Literal(Literal),

    PostfixOp(PostfixOperator, Box<Expression>),
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
    EnumVariant(EnumVariantLiteral),
    Tuple(Vec<Expression>),
    Array(Vec<Expression>),
    Map(Vec<(Expression, Expression)>),
    Unit,       // ()
    None(Node), // none
}

#[derive(Debug)]
pub struct FieldExpression {
    pub field_name: FieldName,
    pub expression: Expression,
}

#[derive(Debug, Eq, PartialEq)]
pub struct FieldType {
    pub field_name: FieldName,
    pub field_type: Type,
}

#[derive(Debug)]
pub enum EnumVariantLiteral {
    Simple(QualifiedTypeIdentifier, LocalTypeIdentifier),
    Tuple(
        QualifiedTypeIdentifier,
        LocalTypeIdentifier,
        Vec<Expression>,
    ),
    Struct(
        QualifiedTypeIdentifier,
        LocalTypeIdentifier,
        Vec<FieldExpression>,
    ),
}

#[derive(Debug, Default)]
pub struct AnonymousStructType {
    pub fields: Vec<FieldType>,
}

#[derive(Debug)]
pub enum EnumVariantType {
    Simple(Node),
    Tuple(Node, Vec<Type>),
    Struct(Node, AnonymousStructType),
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
    Optional(Box<Type>, Node),
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
    PatternList(Vec<PatternElement>),
    EnumPattern(Node, Option<Vec<PatternElement>>),
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
    pub expression: Option<Expression>,
    pub definitions: Vec<Definition>,
}

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for definition in &self.definitions {
            writeln!(f, "{definition:?}")?;
        }

        if !self.definitions.is_empty() && !self.expression.is_none() {
            writeln!(f, "---")?;
        }

        if let Some(found_expression) = &self.expression {
            match found_expression {
                Expression::Block(expressions) => {
                    for expression in expressions {
                        writeln!(f, "{expression:?}")?;
                    }
                }
                _ => writeln!(f, "{found_expression:?}")?,
            }
        }

        Ok(())
    }
}

impl Module {
    #[must_use]
    pub fn new(definitions: Vec<Definition>, expression: Option<Expression>) -> Self {
        Self {
            expression,
            definitions,
        }
    }

    #[must_use]
    pub const fn expression(&self) -> &Option<Expression> {
        &self.expression
    }

    #[must_use]
    pub const fn definitions(&self) -> &Vec<Definition> {
        &self.definitions
    }

    #[must_use]
    pub fn imports(&self) -> Vec<&Use> {
        let mut use_items = Vec::new();

        for def in &self.definitions {
            if let Definition::Use(use_info) = def {
                use_items.push(use_info);
            }
        }

        use_items
    }
}
