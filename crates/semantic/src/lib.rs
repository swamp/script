/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod ns;
pub mod prelude;

use crate::ns::{LocalTypeName, ResolvedModuleNamespace, SemanticError};
use fixed32::Fp;
use seq_fmt::{comma, comma_tuple, fmt_nl};
use seq_map::SeqMap;
use std::cell::RefCell;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;
pub use swamp_script_ast::{
    AnonymousStruct, BinaryOperator, Expression, FormatSpecifier, IdentifierName, ImplMember,
    LocalIdentifier, LocalTypeIdentifier, MatchArm, ModulePath, Parameter, PrecisionType,
    StringConst, StructType, UnaryOperator, Variable,
};

#[derive(Debug, Clone)]
pub struct ResolvedParameter {
    pub name: String,
    pub resolved_type: ResolvedType,
    pub ast_parameter: Parameter,
    pub is_mutable: bool,
}

impl Display for ResolvedParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.resolved_type)
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedFunctionSignature {
    pub parameters: Vec<ResolvedParameter>,
    pub return_type: ResolvedType,
}

#[derive(Debug, Clone)]
pub enum ResolvedType {
    // Primitives
    Int(ResolvedIntTypeRef),
    Float(ResolvedFloatTypeRef),
    String(ResolvedStringTypeRef),
    Bool(ResolvedBoolTypeRef),
    Unit(ResolvedUnitTypeRef),

    Array(ResolvedArrayTypeRef),
    Tuple(ResolvedTupleTypeRef),
    Struct(ResolvedStructTypeRef),

    Enum(ResolvedEnumTypeRef),
    EnumVariant(ResolvedEnumVariantTypeRef),

    FunctionInternal(ResolvedInternalFunctionDefinitionRef),
    FunctionExternal(ResolvedExternalFunctionDefinitionRef),

    ExclusiveRange(ResolvedExclusiveRangeTypeRef),

    Alias(LocalTypeName, Rc<ResolvedType>), // The alias name and the actual type

    Any,
}

impl ResolvedType {
    pub fn display_name(&self) -> String {
        match self {
            Self::Alias(name, _) => name.0.clone(),
            Self::Struct(struct_ref) => struct_ref.borrow().name.text.clone(),
            Self::Int(_) => "Int".to_string(),
            Self::Float(_) => "Float".to_string(),
            Self::String(_) => "String".to_string(),
            Self::Bool(_) => "Bool".to_string(),
            Self::Unit(_) => "Unit".to_string(),
            Self::Array(array_type) => {
                format!("Array<{}>", array_type.item_type.display_name())
            }
            Self::Tuple(tuple_type) => format!(
                "({})",
                tuple_type
                    .0
                    .iter()
                    .map(|t| t.display_name())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Enum(enum_type) => enum_type.name.text.clone(),
            _ => "unsure".to_string(),
        }
    }
}

impl PartialEq for ResolvedType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ResolvedType::Int(_), ResolvedType::Int(_)) => true,
            (ResolvedType::Float(_), ResolvedType::Float(_)) => true,
            (ResolvedType::String(_), ResolvedType::String(_)) => true,
            (ResolvedType::Bool(_), ResolvedType::Bool(_)) => true,
            (ResolvedType::Unit(_), ResolvedType::Unit(_)) => true,
            (ResolvedType::Array(array_ref), ResolvedType::Array(array_ref2)) => {
                array_ref == array_ref2
            }
            _ => false,
        }
    }
}

pub fn same_type(p0: &ResolvedType, p1: &ResolvedType) -> bool {
    match (p0, p1) {
        (ResolvedType::Any, _) => true,
        (_, ResolvedType::Any) => true,
        (ResolvedType::Int(_), ResolvedType::Int(_)) => true,
        (ResolvedType::Float(_), ResolvedType::Float(_)) => true,
        (ResolvedType::String(_), ResolvedType::String(_)) => true,
        (ResolvedType::Bool(_), ResolvedType::Bool(_)) => true,
        (ResolvedType::Unit(_), ResolvedType::Unit(_)) => true,
        (ResolvedType::Array(_), ResolvedType::Array(_)) => true,
        (ResolvedType::Struct(a), ResolvedType::Struct(b)) => compare_struct_types(a, b),
        (ResolvedType::Enum(_), ResolvedType::Enum(_)) => true,
        (ResolvedType::FunctionInternal(_), ResolvedType::FunctionInternal(_)) => true,
        (ResolvedType::ExclusiveRange(_), ResolvedType::ExclusiveRange(_)) => true,
        (ResolvedType::EnumVariant(a), ResolvedType::EnumVariant(b)) => {
            a.owner.number == b.owner.number
        }
        _ => false,
    }
}

fn compare_struct_types(a: &ResolvedStructTypeRef, b: &ResolvedStructTypeRef) -> bool {
    let struct_a = a.borrow();
    let struct_b = b.borrow();

    if struct_a.name != struct_b.name {
        return false;
    }

    if struct_a.fields.len() != struct_b.fields.len() {
        return false;
    }

    for (a_field, b_field) in struct_a.fields.values().zip(struct_b.fields.values()) {
        if !same_type(a_field, b_field) {
            return false;
        }
    }

    true
}

#[derive(Debug)]
pub struct ResolvedInternalFunctionDefinition {
    pub statements: Vec<ResolvedStatement>,
    pub name: LocalIdentifier,
    pub signature: ResolvedFunctionSignature,
}

impl Display for ResolvedInternalFunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "(fn_def {}({}) -> {})",
            self.name,
            comma(&self.signature.parameters),
            self.signature.return_type
        )
    }
}

pub type ResolvedInternalFunctionDefinitionRef = Rc<ResolvedInternalFunctionDefinition>;

pub type ExternalFunctionId = u32;

#[derive(Debug)]
pub struct ResolvedExternalFunctionDefinition {
    pub name: LocalIdentifier,
    pub signature: ResolvedFunctionSignature,
    pub id: ExternalFunctionId,
}

impl Display for crate::ResolvedExternalFunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "(ext_fn_def {}({}) -> {})",
            self.name,
            comma(&self.signature.parameters),
            self.signature.return_type
        )
    }
}

pub type ResolvedExternalFunctionDefinitionRef = Rc<crate::ResolvedExternalFunctionDefinition>;

impl Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(_int_type) => write!(f, "Int"),
            Self::Float(_) => write!(f, "Float"),
            Self::String(_string_type_ref) => write!(f, "String"),
            Self::Bool(_) => write!(f, "Bool"),
            Self::Unit(_) => write!(f, "Unit"),
            Self::Array(array_type_ref) => write!(f, "{array_type_ref}"),
            Self::Tuple(_) => write!(f, "Tuple"),
            Self::Struct(struct_type) => {
                write!(f, "{}", struct_type.borrow())
            }
            Self::Enum(enum_ref) => write!(f, "{enum_ref}"),
            Self::EnumVariant(_) => todo!(),
            Self::ExclusiveRange(range_type) => write!(f, "{range_type:?}"),
            Self::Any => write!(f, "Any"),
            Self::FunctionInternal(function_def_ref) => {
                write!(f, "{function_def_ref}")
            }
            Self::FunctionExternal(function_def_ref) => {
                write!(f, "{function_def_ref}")
            }
            Self::Alias(name, actual_type) => {
                write!(f, "alias {name} {actual_type}")
            }
        }
    }
}

#[derive(Debug)]
pub struct ResolvedVariable {
    pub resolved_type: ResolvedType,
    pub ast_variable: Variable,
    pub scope_index: usize,
    pub variable_index: usize,
}

impl ResolvedVariable {
    #[must_use]
    pub const fn is_mutable(&self) -> bool {
        self.ast_variable.is_mutable
    }
}

impl Display for ResolvedVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix = if self.ast_variable.is_mutable {
            "mut "
        } else {
            ""
        };
        write!(
            f,
            "{}{}<{}:{}>: {}",
            prefix,
            self.ast_variable.name,
            self.scope_index,
            self.variable_index,
            self.resolved_type
        )
    }
}

pub type ResolvedVariableRef = Rc<ResolvedVariable>;

#[derive(Debug)]
pub struct ResolvedMutVariable {
    pub variable_ref: ResolvedVariableRef,
}
type ResolvedMutVariableRef = Rc<ResolvedMutVariable>;

impl Display for ResolvedMutVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "mut {}", self.variable_ref)
    }
}

#[derive(Debug)]

pub struct ResolvedBinaryOperator {
    pub left: Box<ResolvedExpression>,
    pub right: Box<ResolvedExpression>,
    pub ast_operator_type: BinaryOperator,
    pub resolved_type: ResolvedType,
}

#[derive(Debug)]

pub struct ResolvedUnaryOperator {
    pub left: Box<ResolvedExpression>,
    pub ast_operator_type: UnaryOperator,
    pub resolved_type: ResolvedType,
}

#[derive(Debug)]
pub struct ResolvedInternalFunctionCall {
    //pub resolved_type: ResolvedType,
    pub arguments: Vec<ResolvedExpression>,
    pub function_definition: ResolvedInternalFunctionDefinitionRef,
    pub function_expression: Box<ResolvedExpression>,
}

#[derive(Debug)]
pub struct ResolvedStaticCall {
    pub function: Rc<ResolvedFunction>,
    pub arguments: Vec<ResolvedExpression>,
}

#[derive(Debug)]
pub struct ResolvedExternalFunctionCall {
    //pub resolved_type: ResolvedType,
    pub arguments: Vec<ResolvedExpression>,
    pub function_definition: ResolvedExternalFunctionDefinitionRef,
    pub function_expression: Box<ResolvedExpression>,
}

impl Display for ResolvedExternalFunctionCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "external function {}", self.function_definition)
    }
}

pub fn comma_seq<K: Clone + Hash + Eq + Display, V: Display>(values: &SeqMap<K, V>) -> String {
    let mut result = String::new();
    for (i, (key, value)) in values.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        result.push_str(format!("{}: {}", key, value).as_str());
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

impl Display for ResolvedInternalFunctionCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}({}))",
            self.function_definition,
            comma(&self.arguments)
        )
    }
}

#[derive(Debug)]
pub struct ResolvedMemberCall {
    pub function: ResolvedFunctionRef,
    pub arguments: Vec<ResolvedExpression>,
    pub self_expression: Box<ResolvedExpression>,
    pub struct_type_ref: ResolvedStructTypeRef,
    pub self_is_mutable: bool,
}
impl Display for ResolvedMemberCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "(< {} >.{:?}",
            self.self_expression, self.struct_type_ref
        )?;

        if !self.arguments.is_empty() {
            write!(f, " <- {}", comma(&self.arguments))?;
        }

        write!(f, ")")
    }
}

pub type ResolvedMutStructTypeFieldRef = Rc<ResolvedMutStructTypeField>;
#[derive(Debug)]
pub struct ResolvedMutStructTypeField {
    pub inner: ResolvedStructTypeFieldRef,
}

pub type ResolvedMutTupleFieldRef = Rc<ResolvedMutTupleField>;

#[derive(Debug)]
pub struct ResolvedStructTypeField {
    pub struct_type_ref: ResolvedStructTypeRef,
    pub index: usize,
    pub field_name: LocalIdentifier,
    pub resolved_type: ResolvedType,
    pub struct_expression: Box<ResolvedExpression>,
}

impl Display for ResolvedStructTypeField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}.{}<{}>",
            self.struct_type_ref.borrow(),
            self.field_name,
            self.index
        )
    }
}

pub type ResolvedStructTypeFieldRef = Rc<ResolvedStructTypeField>;

pub type ResolvedArrayRef = Rc<ResolvedArray>;
pub type ResolvedMutArrayRef = Rc<ResolvedMutArray>;

#[derive(Debug)]
pub struct ResolvedArray {}
#[derive(Debug)]
pub struct ResolvedMutArray {
    pub expression: ResolvedExpression,
    pub array_type_ref: ResolvedArrayTypeRef,
}

#[derive(Debug)]
pub struct ResolvedArrayItem {
    pub item_type: ResolvedType,
    pub int_expression: ResolvedExpression,
    pub array_expression: ResolvedExpression,
    pub array_type: ResolvedType,
}

impl Display for ResolvedArrayItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "ArrayItem( {} [{}] = {})",
            self.array_type, self.int_expression, self.item_type
        )
    }
}

pub type ResolvedArrayItemRef = Rc<ResolvedArrayItem>;

#[derive(Debug)]
pub struct ResolvedIndexType {
    pub expression: ResolvedExpression,
    pub resolved_type: ResolvedType,
}
pub type ResolvedIndexTypeRef = Rc<ResolvedIndexType>;

#[derive(Debug)]
pub enum ResolvedStringPart {
    Literal(String),
    Interpolation(ResolvedExpression, Option<FormatSpecifier>),
}

impl Display for ResolvedStringPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedStringPart::Literal(literal_string) => write!(f, "\"{literal_string}\""),
            ResolvedStringPart::Interpolation(expression, optional_format) => {
                write!(f, "{expression}")?;

                if let Some(format) = optional_format {
                    write!(f, ":{}", format)?;
                }

                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub struct ResolvedMutTupleField {
    #[allow(unused)]
    ast: Expression,
}

pub type ResolvedFunctionRef = Rc<ResolvedFunction>;

#[derive(Debug)]
pub enum ResolvedFunction {
    Internal(ResolvedInternalFunctionDefinitionRef),
    External(ResolvedExternalFunctionDefinitionRef),
}

impl Display for ResolvedFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "resolved func")
    }
}

pub type MutMemberRef = Rc<MutMember>;

#[derive(Debug)]
pub struct MutMember {
    #[allow(unused)]
    ast: Expression,
}

pub type MemberRef = Rc<Member>;

#[derive(Debug)]
pub struct Member {
    #[allow(unused)]
    ast: Expression,
}

#[derive(Debug)]

pub struct ResolvedBooleanExpression {
    #[allow(unused)]
    pub ast: Expression,

    pub expression: ResolvedExpression,
}

#[derive(Debug)]
pub struct ResolvedMatch {
    pub arms: Vec<ResolvedMatchArm>,
    pub expression: Box<ResolvedExpression>,
}

impl Display for ResolvedMatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Match {} ", self.expression)?;
        for arm in self.arms.iter() {
            writeln!(f, "..{} ", arm)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct ResolvedMatchArm {
    #[allow(unused)]
    pub ast_match_arm: MatchArm,
    pub pattern: ResolvedPattern,
    pub expression: Box<ResolvedExpression>,
    pub expression_type: ResolvedType,
}

impl Display for ResolvedMatchArm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "pattern({}) => {}", self.pattern, self.expression)
    }
}

#[derive(Debug)]
pub enum ResolvedPattern {
    VariableAssignment(ResolvedVariableRef),
    Literal(ResolvedLiteral),
    Tuple(ResolvedTupleTypeRef),
    EnumTuple(
        ResolvedEnumVariantTupleTypeRef,
        Vec<ResolvedEnumVariantTupleFieldType>,
    ),
    EnumStruct(
        ResolvedEnumVariantTypeRef,
        Vec<ResolvedEnumVariantStructFieldType>,
    ),
    Wildcard,
    Struct(ResolvedStructTypeRef),
    EnumSimple(ResolvedEnumVariantTypeRef),
}

impl Display for ResolvedPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedPattern::VariableAssignment(variable_ref) => write!(f, "{}", variable_ref),
            ResolvedPattern::Literal(literal) => write!(f, "{literal}"),
            ResolvedPattern::Wildcard => write!(f, "_"),
            ResolvedPattern::Tuple(_) => write!(f, "_"),
            ResolvedPattern::EnumTuple(_, _) => write!(f, "_"),
            ResolvedPattern::EnumStruct(_, _) => write!(f, "_"),
            ResolvedPattern::Struct(_) => write!(f, "_"),
            ResolvedPattern::EnumSimple(_) => write!(f, "_"),
        }
    }
}

#[derive(Debug)]
pub struct ResolvedIterator {
    pub item_type: ResolvedType,
    pub resolved_expression: ResolvedExpression,
}

//#[derive(Debug)]
//pub struct ResolvedBoolExpression(pub ResolvedExpression);

#[derive(Debug)]
pub struct ResolvedStructInstantiation {
    pub expressions_in_order: Vec<ResolvedExpression>,
    pub struct_type_ref: ResolvedStructTypeRef,
    pub display_type_ref: ResolvedType,
}

#[derive(Debug)]
pub struct ResolvedVariableAssignment {
    pub variable_ref: ResolvedVariableRef,
    pub expression: Box<ResolvedExpression>,
}

#[derive(Debug)]
pub enum ResolvedExpression {
    // Access Lookup values
    FieldAccess(ResolvedStructTypeFieldRef),
    VariableAccess(ResolvedVariableRef),
    InternalFunctionAccess(ResolvedInternalFunctionDefinitionRef),
    ExternalFunctionAccess(ResolvedExternalFunctionDefinitionRef),

    MutRef(ResolvedMutVariableRef), // Used when passing with mut keyword. mut are implicitly passed by reference
    ArrayAccess(ResolvedArrayItemRef), // Read from an array: arr[3]

    // Assignment
    // Since it is a cool language, we can "chain" assignments together. like a = b = c = 1. Even for field assignments, like a.b = c.d = e.f = 1
    VariableAssignment(ResolvedVariableAssignment),
    ArrayAssignment(
        ResolvedMutArrayRef,
        ResolvedIndexTypeRef,
        Box<ResolvedExpression>,
    ), // target, index, source. Write to an index in an array: arr[3] = 42
    StructFieldAssignment(ResolvedMutStructTypeFieldRef, Box<ResolvedExpression>),
    TupleFieldAssignment(ResolvedMutTupleFieldRef, Box<ResolvedExpression>),

    // Operators
    BinaryOp(ResolvedBinaryOperator),
    UnaryOp(ResolvedUnaryOperator),

    // Calls
    FunctionInternalCall(ResolvedInternalFunctionCall), // ResolvedFunctionReference, Vec<ResolvedExpression>
    FunctionExternalCall(ResolvedExternalFunctionCall),
    StaticCall(ResolvedStaticCall),
    MutMemberCall(MutMemberRef, Vec<ResolvedExpression>),
    MemberCall(ResolvedMemberCall),

    Block(Vec<ResolvedStatement>),

    InterpolatedString(ResolvedStringTypeRef, Vec<ResolvedStringPart>),

    // Constructing
    StructInstantiation(ResolvedStructInstantiation),
    Array(ResolvedArrayInstantiation),
    Tuple(Vec<ResolvedExpression>),
    Literal(ResolvedLiteral),
    //Map(HashMap<ResolvedExpression, ResolvedExpression>), // Not implemented yet. Maybe call this a dictionary or similar, to avoid confusion with map()
    ExclusiveRange(
        ResolvedExclusiveRangeTypeRef,
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
    ),

    // Comparing
    IfElse(
        Box<ResolvedBooleanExpression>,
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
    ),
    Match(ResolvedMatch),
    LetVar(ResolvedVariableRef, Box<ResolvedExpression>),
}

#[derive(Debug)]
pub enum ResolvedLiteral {
    FloatLiteral(Fp, ResolvedFloatTypeRef),
    UnitLiteral(ResolvedUnitTypeRef),
    IntLiteral(i32, ResolvedIntTypeRef),
    StringLiteral(StringConst, ResolvedStringTypeRef),
    BoolLiteral(bool, ResolvedBoolTypeRef),
    EnumVariantLiteral(ResolvedEnumVariantTypeRef, ResolvedEnumLiteralData),
    TupleLiteral(ResolvedTupleTypeRef, Vec<ResolvedExpression>),
    Array(ResolvedArrayTypeRef, Vec<ResolvedExpression>),
}

impl Display for ResolvedLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedLiteral::FloatLiteral(value, _) => write!(f, "FloatLit({value})"),
            ResolvedLiteral::UnitLiteral(_) => write!(f, "UnitLit"),
            ResolvedLiteral::IntLiteral(value, _) => write!(f, "IntLit({value})"),
            ResolvedLiteral::StringLiteral(value, _) => write!(f, "StringLit({value})"),
            ResolvedLiteral::BoolLiteral(value, _) => write!(f, "BoolLit({value})"),
            ResolvedLiteral::EnumVariantLiteral(_, _) => write!(f, "EnumVariantLiteral"),
            ResolvedLiteral::TupleLiteral(_, _) => write!(f, "TupleLiteral"),
            ResolvedLiteral::Array(_, _) => write!(f, "ArrayLiteral"),
        }
    }
}

//pub type ResolvedExpressionRef = Rc<ResolvedExpression>;

impl Display for ResolvedExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FieldAccess(field_lookup) => write!(f, "{}", field_lookup),
            Self::VariableAccess(variable) => write!(f, "VarRead({})", variable),
            Self::InternalFunctionAccess(internal_function_ref) => {
                write!(f, "{:?}", internal_function_ref)
            }
            Self::ExternalFunctionAccess(external_function_ref) => {
                write!(f, "{:?}", external_function_ref)
            }
            Self::MutRef(mut_var_ref) => write!(f, "MutRef({})", mut_var_ref),
            Self::ArrayAccess(array_item_ref) => {
                write!(f, "[{}]", array_item_ref.item_type)
            }
            Self::VariableAssignment(var_assignment) => write!(
                f,
                "< {}={} >",
                var_assignment.variable_ref, var_assignment.expression
            ),
            Self::ArrayAssignment(_, _, _) => todo!(),
            Self::StructFieldAssignment(_, _) => todo!(),
            Self::TupleFieldAssignment(_, _) => todo!(),
            Self::BinaryOp(binary_op) => write!(f, "{binary_op:?}"),
            Self::UnaryOp(unary_op) => {
                write!(f, "{:?}", unary_op)
            }
            Self::FunctionInternalCall(resolved_call) => {
                write!(f, "{resolved_call}")
            }
            Self::FunctionExternalCall(resolved_call) => write!(f, "{resolved_call}"),
            Self::MutMemberCall(_, _) => todo!(),
            Self::MemberCall(member_call) => write!(f, "{member_call}"),
            Self::Block(statements) => write!(f, "{}", fmt_nl(statements)),
            Self::InterpolatedString(_string_type, parts) => {
                write!(f, "'{}'", comma(parts))
            }
            Self::StructInstantiation(struct_instantiation) => {
                let borrowed = struct_instantiation.struct_type_ref.borrow();
                let zipped: Vec<_> = borrowed
                    .fields
                    .keys()
                    .zip(struct_instantiation.expressions_in_order.iter())
                    .collect();

                write!(f, "{{ {} }}", comma_tuple(&zipped))?;
                Ok(())
            }
            Self::Array(array_instantiation) => {
                write!(f, "[{:?}]", array_instantiation.expressions)
            }
            Self::Tuple(_) => todo!(),
            Self::ExclusiveRange(range_type, _, _) => {
                write!(f, "[{range_type:?}]")
            }
            Self::IfElse(_, _, _) => todo!(),
            Self::Match(resolved_match) => write!(f, "{resolved_match}"),
            Self::LetVar(_, _) => todo!(),
            Self::Literal(resolved_literal) => match resolved_literal {
                ResolvedLiteral::FloatLiteral(value, _float_type) => {
                    write!(f, "FloatLit({value:?})")
                }
                ResolvedLiteral::IntLiteral(value, _int_type) => write!(f, "IntLit({value:?})"),
                ResolvedLiteral::StringLiteral(value, _string_type) => {
                    write!(f, "StringLit({value:?})")
                }
                ResolvedLiteral::UnitLiteral(_unit_lit) => write!(f, "UnitLit"),
                ResolvedLiteral::BoolLiteral(value, _bool_type_ref) => {
                    write!(f, "BoolLit({value:?})")
                }
                ResolvedLiteral::EnumVariantLiteral(variant_ref, data) => {
                    write!(f, "EnumVariantLit({variant_ref:?}, {data})")
                }

                ResolvedLiteral::TupleLiteral(_tuple_type, data) => {
                    write!(f, "TupleLiteral({data:?})")
                }
                ResolvedLiteral::Array(_array_type, data) => write!(f, "Array({data:?})"),
            },
            Self::StaticCall(_) => write!(f, "static call"),
        }
    }
}

#[derive(Debug)]
pub struct ResolvedArrayInstantiation {
    pub expressions: Vec<ResolvedExpression>,
    pub item_type: ResolvedType,
    pub array_type: ResolvedType,
    pub array_type_ref: ResolvedArrayTypeRef,
}

#[derive(Debug)]
pub enum ResolvedStatement {
    // Standard
    Let(ResolvedPattern, ResolvedExpression), // Should be expression only? and put in Expression()
    ForLoop(ResolvedPattern, ResolvedIterator, Vec<ResolvedStatement>),
    WhileLoop(ResolvedBooleanExpression, Vec<ResolvedStatement>),
    Return(ResolvedExpression),
    Break,                          // Return with void
    Continue,                       //
    Expression(ResolvedExpression), // Used for expressions with side effects (mutation, i/o)
    Block(Vec<ResolvedStatement>),
    If(
        ResolvedBooleanExpression,
        Vec<ResolvedStatement>,
        Option<Vec<ResolvedStatement>>,
    ),
    LetVar(ResolvedVariableRef, ResolvedExpression), // Should be expression only? and put in Expression()
    SetVar(ResolvedVariableRef, ResolvedExpression), // Should be expression only? and put in Expression()
}

impl Display for ResolvedStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedStatement::Let(_, _) => todo!(),
            ResolvedStatement::ForLoop(target, iterator, statements) => {
                write!(f, "{target} {iterator:?} {statements:?}")
            }
            ResolvedStatement::WhileLoop(_, _) => write!(f, "WhileLoop"),
            ResolvedStatement::Return(_) => write!(f, "Return"),
            ResolvedStatement::Break => write!(f, "Break"),
            ResolvedStatement::Continue => write!(f, "Continue"),
            ResolvedStatement::Expression(expression) => write!(f, "{}", expression),
            ResolvedStatement::Block(_) => write!(f, "Block"),
            ResolvedStatement::If(_, _, _) => write!(f, "If"),
            ResolvedStatement::LetVar(variable_ref, expr) => {
                write!(f, "let {variable_ref} = {expr}")
            }
            ResolvedStatement::SetVar(variable_ref, expr) => {
                write!(f, "set {variable_ref} = {expr}")
            }
        }
    }
}

pub type ResolvedStructTypeRef = Rc<RefCell<ResolvedStructType>>;

pub type TypeNumber = u32;

#[derive()]
pub struct ResolvedStructType {
    // TODO:  pub defined_in_module: ResolvedModuleRef,
    pub number: TypeNumber,
    pub module_path: ModulePath,
    pub fields: SeqMap<IdentifierName, ResolvedType>,
    pub name: LocalTypeIdentifier,
    pub ast_struct: StructType,
    pub functions: SeqMap<IdentifierName, ResolvedFunctionRef>,
}

impl Debug for ResolvedStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct type {:?}", self.number)
    }
}

impl Display for ResolvedStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{", self.name)?;
        write!(f, " {} ", comma_seq(&self.fields))?;
        write!(f, "}}")
    }
}

impl ResolvedStructType {
    pub fn new(
        // TODO: defined_in_module: ResolvedModuleRef,
        module_path: ModulePath,
        name: LocalTypeIdentifier,
        fields: SeqMap<IdentifierName, ResolvedType>,
        ast_struct: StructType,
        number: TypeNumber,
    ) -> Self {
        Self {
            number,
            //defined_in_module,
            module_path,
            ast_struct,
            fields,
            name,
            functions: SeqMap::default(),
        }
    }

    pub fn field_index(&self, field_name: &LocalIdentifier) -> Option<usize> {
        self.fields
            .get_index(&IdentifierName(field_name.text.to_string()))
    }

    pub fn get_internal_member(
        &self,
        name: IdentifierName,
    ) -> Option<ResolvedInternalFunctionDefinitionRef> {
        let found = self.functions.get(&name)?;

        match &**found {
            ResolvedFunction::Internal(ref function_ref) => Some(function_ref.clone()),
            _ => None,
        }
    }

    pub fn name(&self) -> &LocalTypeIdentifier {
        &self.name
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ResolvedUnitType;
pub type ResolvedUnitTypeRef = Rc<ResolvedUnitType>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ResolvedIntType;
pub type ResolvedIntTypeRef = Rc<ResolvedIntType>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ResolvedFloatType;

pub type ResolvedFloatTypeRef = Rc<ResolvedFloatType>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ResolvedBoolType;
pub type ResolvedBoolTypeRef = Rc<ResolvedBoolType>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ResolvedStringType;
pub type ResolvedStringTypeRef = Rc<ResolvedStringType>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ResolvedExclusiveRangeType;
pub type ResolvedExclusiveRangeTypeRef = Rc<ResolvedExclusiveRangeType>;

pub type ResolvedArrayTypeRef = Rc<ResolvedArrayType>;

#[derive(Debug)]
pub struct ResolvedArrayType {
    pub item_type: ResolvedType,
    //pub ast_type: Type,
}

impl PartialEq for ResolvedArrayType {
    fn eq(&self, other: &Self) -> bool {
        self.item_type == other.item_type
    }
}

impl Display for ResolvedArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.item_type)
    }
}

pub type ResolvedEnumVariantStructTypeRef = Rc<ResolvedEnumVariantStructType>;

#[derive(Debug)]
pub struct CommonEnumVariantType {
    pub number: TypeNumber,
    pub module_path: ModulePath,
    pub variant_name: LocalTypeIdentifier,
    pub enum_ref: ResolvedEnumTypeRef,
}

impl Display for CommonEnumVariantType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.enum_ref.name, self.variant_name)
    }
}

#[derive(Debug)]
pub struct ResolvedEnumVariantStructType {
    pub common: CommonEnumVariantType,

    pub fields: SeqMap<IdentifierName, ResolvedType>, // Anonymous Struct
    pub ast_struct: AnonymousStruct,
}

impl Display for ResolvedEnumVariantStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{ {} }}", self.common, comma_seq(&self.fields))
    }
}

pub type ResolvedEnumVariantTupleTypeRef = Rc<ResolvedEnumVariantTupleType>;

#[derive(Debug)]
pub struct ResolvedEnumVariantTupleType {
    pub common: CommonEnumVariantType,

    pub fields_in_order: Vec<ResolvedType>,
}

impl Display for crate::ResolvedEnumVariantTupleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ {} }}", comma(&self.fields_in_order))
    }
}

pub type ResolvedTupleTypeRef = Rc<ResolvedTupleType>;

#[derive(Debug)]
pub struct ResolvedTupleType(pub Vec<ResolvedType>);

impl Display for ResolvedTupleType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({})", comma(&self.0))
    }
}

impl ResolvedTupleType {
    pub fn new(types: Vec<ResolvedType>) -> Self {
        Self(types)
    }
}

pub type ResolvedEnumTypeRef = Rc<ResolvedEnumType>;

#[derive(Debug)]
pub struct ResolvedEnumType {
    pub name: LocalTypeIdentifier,
    pub number: TypeNumber,
}

impl Display for ResolvedEnumType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl ResolvedEnumType {
    pub fn new(name: LocalTypeIdentifier, number: TypeNumber) -> Self {
        Self { name, number }
    }

    pub fn name(&self) -> &LocalTypeIdentifier {
        &self.name
    }
}

pub type ResolvedEnumVariantTypeRef = Rc<ResolvedEnumVariantType>;

pub struct ResolvedEnumVariantType {
    pub owner: ResolvedEnumTypeRef,
    pub data: ResolvedEnumVariantContainerType,
    pub name: LocalTypeIdentifier,
    pub number: TypeNumber,
}

impl Display for ResolvedEnumVariantType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}{}", self.owner, self.name, self.data)
    }
}

pub type ResolvedEnumVariantStructFieldTypeRef = Rc<ResolvedEnumVariantStructFieldType>;

#[derive(Debug)]
pub struct ResolvedEnumVariantStructFieldType {
    pub name: LocalIdentifier,
    pub field_index: usize,
    pub enum_variant: ResolvedEnumVariantTypeRef,
    pub resolved_type: ResolvedType,
}

impl Display for crate::ResolvedEnumVariantStructFieldType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} <{}>", self.name, self.field_index)
    }
}

pub type ResolvedEnumVariantTupleFieldTypeRef = Rc<ResolvedEnumVariantTupleFieldType>;

#[derive(Debug)]
pub struct ResolvedEnumVariantTupleFieldType {
    pub name: LocalIdentifier,
    pub field_index: usize,
    pub enum_variant: ResolvedEnumVariantTypeRef,
    pub resolved_type: ResolvedType,
}

impl Display for crate::ResolvedEnumVariantTupleFieldType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} <{}>", self.name, self.field_index)
    }
}

/*
impl Display for ResolvedEnumVariantType {

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.owner.name(), self.name)?;

        match &self.data {
            ResolvedEnumVariantContainerType::Struct(_) => {
                write!(f, "{}::{}", self.name, self.complete_name())
            }
            ResolvedEnumVariantContainerType::Tuple(tuple_ref) => write!(f, "{:?}", tuple_ref),
            ResolvedEnumVariantContainerType::Nothing => write!(f, ""),
        }
    }
}
*/

impl ResolvedEnumVariantType {
    pub fn new(
        owner: ResolvedEnumTypeRef,
        name: LocalTypeIdentifier,
        data: ResolvedEnumVariantContainerType,
        number: TypeNumber,
    ) -> Self {
        Self {
            owner,
            data,
            name,
            number,
        }
    }

    pub fn container(&self) -> &ResolvedEnumVariantContainerType {
        &self.data
    }

    pub fn name(&self) -> &LocalTypeIdentifier {
        &self.name
    }

    pub fn complete_name(&self) -> String {
        self.owner.name.text.to_string() + "::" + &*self.name.text.to_string()
    }
}

impl Debug for ResolvedEnumVariantType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.owner.name(), self.name)
    }
}

#[derive(Debug, Clone)]
pub enum ResolvedEnumVariantContainerType {
    Struct(ResolvedEnumVariantStructTypeRef),
    Tuple(ResolvedEnumVariantTupleTypeRef),
    Nothing,
}

impl Display for ResolvedEnumVariantContainerType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedEnumVariantContainerType::Struct(struct_ref) => write!(f, " {} ", struct_ref),
            ResolvedEnumVariantContainerType::Tuple(tuple_ref) => write!(f, "{}", tuple_ref),
            ResolvedEnumVariantContainerType::Nothing => Ok(()),
        }
    }
}

#[derive(Debug)]
pub struct ImplType {
    pub members: SeqMap<String, ImplMember>,
    pub associated_with_struct_type: ResolvedStructTypeRef,
}

impl ImplType {
    pub fn new(
        members: SeqMap<String, ImplMember>,
        associated_with_struct_type: ResolvedStructTypeRef,
    ) -> Self {
        Self {
            members,
            associated_with_struct_type,
        }
    }
}

pub type ResolvedModuleRef = Rc<ResolvedModule>;

#[derive(Debug)]
pub struct ResolvedModule {
    pub definitions: Vec<ResolvedDefinition>,
    pub statements: Vec<ResolvedStatement>,
    pub namespace: ResolvedModuleNamespace,
    pub module_path: ModulePath,
}

impl Display for ResolvedModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "namespace:\n{}", self.namespace)?;
        if !self.statements.is_empty() {
            writeln!(f, "statements:")?;

            for statement in &self.statements {
                writeln!(f, "{}", statement)?;
            }
        };

        Ok(())
    }
}

impl ResolvedModule {
    pub fn new(module_path: ModulePath) -> Self {
        Self {
            module_path,
            definitions: Vec::new(),
            namespace: ResolvedModuleNamespace::new(),
            statements: Vec::new(),
        }
    }
}

/*
#[derive(Debug)]
pub struct ResolvedFunctionData {
    pub signature: ResolvedFunctionSignature,
    pub statements: Vec<ResolvedStatement>,
}

pub type ResolvedFunctionDataRef = Rc<ResolvedFunctionData>;

 */
/*
pub struct ResolvedImplType {
    pub items: Vec<ResolvedImplItem>,
}

pub enum ResolvedImplItem {
    Member(ResolvedImplMember),
    Function(ResolvedFunctionData),
}

 */

impl Default for ResolvedProgram {
    fn default() -> Self {
        Self::new()
    }
}

impl ResolvedProgram {
    pub fn new() -> Self {
        Self {
            modules: ResolvedModules::new(),
            int_type: Rc::new(ResolvedIntType {}),
            float_type: Rc::new(ResolvedFloatType),
            string_type: Rc::new(ResolvedStringType),
            bool_type: Rc::new(ResolvedBoolType),
            unit_type: Rc::new(ResolvedUnitType),
            exclusive_range_type: Rc::new(ResolvedExclusiveRangeType),
            array_types: Vec::new(),
            number: 0,
            external_function_number: 0,
        }
    }
}

#[derive(Debug)]
pub enum ResolvedDefinition {
    StructType(),
    EnumType(),
    Function(),
    ExternalFunction(),
    ImplType(),
    FunctionDef(LocalIdentifier, ResolvedFunction),
    ImplDef(
        LocalTypeIdentifier,
        SeqMap<IdentifierName, ResolvedFunction>,
    ),
}

#[derive(Debug)]
pub struct ResolvedModules {
    pub modules: SeqMap<ModulePath, ResolvedModuleRef>,
}

impl Default for ResolvedModules {
    fn default() -> Self {
        Self::new()
    }
}

impl ResolvedModules {
    pub fn new() -> Self {
        Self {
            modules: SeqMap::new(),
        }
    }

    pub fn add_module(
        &mut self,
        module_path: ModulePath,
        module: ResolvedModuleRef,
    ) -> Result<(), SemanticError> {
        Ok(self.modules.insert(module_path, module)?)
    }

    pub fn get(&self, module_path: &ModulePath) -> Option<&ResolvedModuleRef> {
        self.modules.get(module_path)
    }
}

impl Display for ResolvedModules {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (module_path, module) in &self.modules {
            writeln!(f, "{}\n  {}", module_path, module)?
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct ResolvedProgram {
    pub modules: ResolvedModules,
    pub int_type: ResolvedIntTypeRef,
    pub float_type: ResolvedFloatTypeRef,
    pub string_type: ResolvedStringTypeRef,
    pub bool_type: ResolvedBoolTypeRef,
    pub unit_type: ResolvedUnitTypeRef,

    pub exclusive_range_type: ResolvedExclusiveRangeTypeRef,

    pub array_types: Vec<ResolvedArrayTypeRef>,

    pub number: TypeNumber,
    pub external_function_number: ExternalFunctionId,
}

impl ResolvedProgram {
    pub fn allocate_number(&mut self) -> TypeNumber {
        self.number += 1;
        self.number
    }
}

impl fmt::Display for ResolvedProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "modules:\n{}", self.modules)
    }
}

#[derive(Debug)]
pub enum ResolvedEnumLiteralData {
    Nothing,
    Tuple(Vec<ResolvedExpression>),
    Struct(Vec<ResolvedExpression>),
}

impl Display for ResolvedEnumLiteralData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nothing => Ok(()),
            Self::Tuple(tuple_data) => write!(f, "{:?}", tuple_data),
            Self::Struct(struct_data) => write!(f, "{:?}", struct_data),
        }
    }
}
