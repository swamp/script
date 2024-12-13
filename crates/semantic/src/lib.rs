/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod ns;
pub mod prelude;

use crate::ns::{LocalTypeName, ResolvedModuleNamespace, SemanticError};
pub use fixed32::Fp;
use seq_fmt::{comma, comma_tuple, fmt_nl};
use seq_map::SeqMap;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;
pub use swamp_script_ast::{
    AnonymousStruct, BinaryOperator, CompoundOperator, Expression, FormatSpecifier, IdentifierName,
    ImplMember, LocalIdentifier, LocalTypeIdentifier, MatchArm, ModulePath, Node, Parameter,
    Position, PostfixOperator, PrecisionType, StringConst, StructType, UnaryOperator, Variable,
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
pub struct ResolvedRustType {
    pub type_name: String, // To identify the specific Rust type
    pub number: u32,       // For type comparison
}
pub type ResolvedRustTypeRef = Rc<ResolvedRustType>;

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
    Map(ResolvedMapTypeRef),

    Generic(Box<ResolvedType>, Vec<ResolvedType>),

    Enum(ResolvedEnumTypeRef),
    EnumVariant(ResolvedEnumVariantTypeRef),

    FunctionInternal(ResolvedInternalFunctionDefinitionRef),
    FunctionExternal(ResolvedExternalFunctionDefinitionRef),

    ExclusiveRange(ResolvedExclusiveRangeTypeRef),

    Alias(LocalTypeName, Box<ResolvedType>), // The alias name and the actual type
    Optional(Box<ResolvedType>),

    RustType(ResolvedRustTypeRef),

    Any,
}

impl ResolvedType {
    pub fn expect_struct_type(&self) -> Result<ResolvedStructTypeRef, SemanticError> {
        match self {
            ResolvedType::Struct(struct_type_ref) => Ok(struct_type_ref.clone()),
            _ => Err(SemanticError::ResolveNotStruct),
        }
    }

    pub fn same_type(&self, other: &ResolvedType) -> bool {
        match (self, other) {
            (Self::Any, _) => true,
            (_, Self::Any) => true,
            (Self::Int(_), Self::Int(_)) => true,
            (Self::Float(_), Self::Float(_)) => true,
            (Self::String(_), Self::String(_)) => true,
            (Self::Bool(_), Self::Bool(_)) => true,
            (Self::Unit(_), Self::Unit(_)) => true,
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
            (Self::FunctionInternal(_), Self::FunctionInternal(_)) => true,
            (Self::ExclusiveRange(_), Self::ExclusiveRange(_)) => true,
            (Self::EnumVariant(a), Self::EnumVariant(b)) => a.owner.number == b.owner.number,
            (Self::Optional(inner_type_a), Self::Optional(inner_type_b)) => {
                inner_type_a.same_type(inner_type_b)
            }
            (Self::RustType(type_ref_a), Self::RustType(type_ref_b)) => {
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
            Self::Map(map_type_ref) => {
                format!(
                    "[{}:{}]",
                    map_type_ref.key_type.display_name(),
                    map_type_ref.value_type.display_name()
                )
            }
            Self::Tuple(tuple_type) => format!(
                "({})",
                tuple_type
                    .0
                    .iter()
                    .map(Self::display_name)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Enum(enum_type) => enum_type.name.text.clone(),
            Self::EnumVariant(variant_type_ref) => format!("{variant_type_ref}"),
            Self::FunctionInternal(internal_function) => format!(
                "fn {} {:?}",
                internal_function.name, internal_function.signature
            ),
            Self::FunctionExternal(external_function) => {
                format!("ext_fn {} {}", external_function.name, external_function.id)
            }
            Self::ExclusiveRange(_exclusive_range) => "exclusive_range".to_string(),
            Self::Any => "Any".to_string(),
            Self::Generic(resolved_type, vec) => format!("{resolved_type}<{}>", comma(&vec)),
            Self::Optional(inner_type) => format!("{inner_type}?"),
            Self::RustType(rust_type_ref) => rust_type_ref.type_name.clone(),
        }
    }
}

// impl PartialEq for ResolvedType {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (Self::Int(_), Self::Int(_)) => true,
//             (Self::Float(_), Self::Float(_)) => true,
//             (Self::String(_), Self::String(_)) => true,
//             (Self::Bool(_), Self::Bool(_)) => true,
//             (Self::Unit(_), Self::Unit(_)) => true,
//             (Self::Array(array_ref), Self::Array(array_ref2)) => array_ref == array_ref2,
//             _ => false,
//         }
//     }
//}

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
        if !a_field.same_type(b_field) {
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
            Self::Map(map_type_ref) => write!(f, "{map_type_ref}"),
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
                write!(f, "type {name} = {actual_type}")
            }
            Self::Generic(resolved_type, vec) => {
                write!(f, "{resolved_type}<{}>", comma(&vec))
            }
            Self::RustType(rust_type_ref) => write!(f, "rust_type {rust_type_ref:?}"),
            Self::Optional(inner_type) => write!(f, "{inner_type}?"),
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
pub struct ResolvedPostfixOperator {
    pub left: Box<ResolvedExpression>,
    pub ast_operator_type: PostfixOperator,
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
    //pub struct_type_ref: ResolvedStructTypeRef,
    pub self_is_mutable: bool,
}
impl Display for ResolvedMemberCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(< {} >.{:?}", self.self_expression, self.function)?;

        if !self.arguments.is_empty() {
            write!(f, " <- {}", comma(&self.arguments))?;
        }

        write!(f, ")")
    }
}

#[derive(Debug)]
pub enum ResolvedAccess {
    FieldIndex(usize),
    CollectionIndex(usize),
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

#[derive(Debug)]
pub struct ResolvedArray {}
#[derive(Debug)]
pub struct ResolvedMutArray {
    pub expression: Box<ResolvedExpression>,
    pub array_type_ref: ResolvedArrayTypeRef,
}

#[derive(Debug)]
pub struct ResolvedMutMap {
    pub expression: Box<ResolvedExpression>,
    pub map_type_ref: ResolvedMapTypeRef,
}

#[derive(Debug)]
pub struct ResolvedMapIndexLookup {
    pub map_type: ResolvedType,
    pub item_type: ResolvedType,
    pub map_type_ref: ResolvedMapTypeRef,
    pub index_expression: Box<ResolvedExpression>,
    pub map_expression: Box<ResolvedExpression>,
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
    pub expression: Box<ResolvedExpression>,
    pub resolved_type: ResolvedType,
}

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
        match self {
            Self::Internal(internal_func_def_ref) => write!(
                f,
                "fn {} {:?}",
                internal_func_def_ref.name, internal_func_def_ref.signature
            ),
            Self::External(external_func_def_ref) => write!(
                f,
                "ext_fn {} {:?}",
                external_func_def_ref.name, external_func_def_ref.signature
            ),
        }
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
        for arm in &self.arms {
            writeln!(f, "..{arm} ")?;
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
    PatternList(Vec<ResolvedPatternElement>),
    EnumPattern(
        ResolvedEnumVariantTypeRef,
        Option<Vec<ResolvedPatternElement>>,
    ),
    Literal(ResolvedLiteral),
}

#[derive(Debug)]
pub enum ResolvedPatternElement {
    Variable(ResolvedVariableRef),
    VariableWithFieldIndex(ResolvedVariableRef, usize),
    Wildcard,
}

impl Display for ResolvedPatternElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Variable(variable_ref) => write!(f, "{variable_ref}"),
            Self::Wildcard => write!(f, "_"),
            Self::VariableWithFieldIndex(_rc, _) => write!(f, "variable with field index"),
        }
    }
}

impl Display for ResolvedPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedPattern::Literal(literal) => write!(f, "{literal}"),
            ResolvedPattern::PatternList(elements) => write!(f, "{}", comma(elements)),
            ResolvedPattern::EnumPattern(enum_variant_type_ref, elements) => {
                write!(f, "{}", enum_variant_type_ref.name)?;
                if let Some(found_elements) = elements {
                    write!(f, "{}", comma(found_elements))?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug)]
pub struct ResolvedIterator {
    pub item_type: ResolvedType,
    pub resolved_expression: ResolvedExpression,
    pub is_mutable: bool,
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
    pub variable_refs: Vec<ResolvedVariableRef>, // Support single or multiple variables
    pub expression: Box<ResolvedExpression>,
}

#[derive(Debug)]
pub struct ResolvedVariableCompoundAssignment {
    pub variable_ref: ResolvedVariableRef, // compound only support single variable
    pub expression: Box<ResolvedExpression>,
    pub ast_operator: CompoundOperator,
}

#[derive(Debug)]
pub struct ResolvedFieldCompoundAssignment {
    pub struct_field_ref: ResolvedStructTypeFieldRef,
    pub expression: Box<ResolvedExpression>,
    pub ast_operator: CompoundOperator,
}

pub fn create_rust_type(name: &str, type_number: TypeNumber) -> ResolvedRustTypeRef {
    let rust_type = ResolvedRustType {
        type_name: name.to_string(),
        number: type_number,
    };
    Rc::new(rust_type)
}

#[derive(Debug)]
pub enum ResolvedExpression {
    // Access Lookup values
    VariableAccess(ResolvedVariableRef),
    FieldAccess(
        Box<ResolvedExpression>,
        ResolvedStructTypeFieldRef,
        Vec<ResolvedAccess>,
    ),

    InternalFunctionAccess(ResolvedInternalFunctionDefinitionRef),
    ExternalFunctionAccess(ResolvedExternalFunctionDefinitionRef),

    MutRef(ResolvedMutVariableRef), // Used when passing with mut keyword. mut are implicitly passed by reference
    Option(Option<Box<ResolvedExpression>>),
    ArrayAccess(ResolvedArrayItemRef), // Read from an array: arr[3]
    MapIndexAccess(ResolvedMapIndexLookup),

    // Assignment
    // Since it is a cool language, we can "chain" assignments together. like a = b = c = 1. Even for field assignments, like a.b = c.d = e.f = 1
    InitializeVariable(ResolvedVariableAssignment), // First time assignment
    ReassignVariable(ResolvedVariableAssignment),   // Subsequent assignments

    VariableCompoundAssignment(ResolvedVariableCompoundAssignment),
    FieldCompoundAssignment(ResolvedFieldCompoundAssignment),

    ArrayExtend(ResolvedVariableRef, Box<ResolvedExpression>), // Extends an array with another array
    ArrayPush(ResolvedVariableRef, Box<ResolvedExpression>),   // Adds an item to an array

    //    CompoundAssignmentIndex(ResolvedIndexCompoundAssignment),
    ArrayAssignment(ResolvedMutArray, ResolvedIndexType, Box<ResolvedExpression>), // target, index, source. Write to an index in an array: arr[3] = 42

    MapAssignment(ResolvedMutMap, ResolvedIndexType, Box<ResolvedExpression>),
    StructFieldAssignment(
        Box<ResolvedExpression>,
        Vec<ResolvedAccess>,
        Box<ResolvedExpression>,
    ),

    // Operators
    BinaryOp(ResolvedBinaryOperator),
    UnaryOp(ResolvedUnaryOperator),
    PostfixOp(ResolvedPostfixOperator),

    CoerceOptionToBool(Box<ResolvedExpression>),

    // Calls
    FunctionInternalCall(ResolvedInternalFunctionCall), // ResolvedFunctionReference, Vec<ResolvedExpression>
    FunctionExternalCall(ResolvedExternalFunctionCall),
    StaticCall(ResolvedStaticCall),
    StaticCallGeneric(ResolvedStaticCallGeneric),
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

    // Special if-else variants for optional unwrapping
    IfElseOnlyVariable {
        variable: ResolvedVariableRef,
        optional_expr: Box<ResolvedExpression>,
        true_block: Box<ResolvedExpression>,
        false_block: Box<ResolvedExpression>,
    },

    IfElseAssignExpression {
        variable: ResolvedVariableRef,
        optional_expr: Box<ResolvedExpression>,
        true_block: Box<ResolvedExpression>,
        false_block: Box<ResolvedExpression>,
    },

    Match(ResolvedMatch),
    LetVar(ResolvedVariableRef, Box<ResolvedExpression>),
    ArrayRemoveIndex(ResolvedVariableRef, Box<ResolvedExpression>),
    ArrayClear(ResolvedVariableRef),

    FloatRound(Box<ResolvedExpression>),
    FloatFloor(Box<ResolvedExpression>),
    FloatSign(Box<ResolvedExpression>),
    FloatAbs(Box<ResolvedExpression>),

    // --- Special methods
    // TODO: Have a better interface for these "engine" member calls
    SparseAdd(Box<ResolvedExpression>, Box<ResolvedExpression>),
    SparseRemove(Box<ResolvedExpression>, Box<ResolvedExpression>),
    SparseNew(ResolvedRustTypeRef, ResolvedType),
}

#[derive(Debug)]
pub enum ResolvedLiteral {
    FloatLiteral(Fp, ResolvedFloatTypeRef),
    UnitLiteral(ResolvedUnitTypeRef),
    NoneLiteral,
    IntLiteral(i32, ResolvedIntTypeRef),
    StringLiteral(StringConst, ResolvedStringTypeRef),
    BoolLiteral(bool, ResolvedBoolTypeRef),
    EnumVariantLiteral(ResolvedEnumVariantTypeRef, ResolvedEnumLiteralData),
    TupleLiteral(ResolvedTupleTypeRef, Vec<ResolvedExpression>),
    Array(ResolvedArrayTypeRef, Vec<ResolvedExpression>),
    Map(
        ResolvedMapTypeRef,
        Vec<(ResolvedExpression, ResolvedExpression)>,
    ),
}

impl Display for ResolvedLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FloatLiteral(value, _) => write!(f, "FloatLit({value})"),
            Self::UnitLiteral(_) => write!(f, "UnitLit"),
            Self::IntLiteral(value, _) => write!(f, "IntLit({value})"),
            Self::StringLiteral(value, _) => write!(f, "StringLit({value})"),
            Self::BoolLiteral(value, _) => write!(f, "BoolLit({value})"),
            Self::EnumVariantLiteral(_, _) => write!(f, "EnumVariantLiteral"),
            Self::TupleLiteral(_, _) => write!(f, "TupleLiteral"),
            Self::Array(_, _) => write!(f, "ArrayLiteral"),
            Self::Map(_, _) => write!(f, "MapLiteral"),
            Self::NoneLiteral => write!(f, "NoneLiteral"),
        }
    }
}

//pub type ResolvedExpressionRef = Rc<ResolvedExpression>;

impl Display for ResolvedExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::VariableAccess(variable) => write!(f, "VarRead({})", variable),
            Self::FieldAccess(_expr, _struct_type_field, lookups) => {
                write!(f, "field access {lookups:?}")
            }
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
            Self::MapIndexAccess(map_lookup) => {
                write!(
                    f,
                    "[{}[{}]]",
                    map_lookup.map_expression, map_lookup.index_expression
                )
            }

            Self::ArrayAssignment(_, _, _) => write!(f, "array assignment"),
            Self::MapAssignment(_, _, _) => write!(f, "map assignment"),
            Self::StructFieldAssignment(_, _, _) => write!(f, "field assignment"),
            Self::BinaryOp(binary_op) => write!(f, "{binary_op:?}"),
            Self::UnaryOp(unary_op) => {
                write!(f, "{:?}", unary_op)
            }
            Self::PostfixOp(postfix_op) => write!(f, "{:?}", postfix_op),

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
            Self::IfElse(condition, consequence, alternative) => {
                write!(f, "if({condition:?}, {consequence}, {alternative})")
            }
            Self::IfElseOnlyVariable { .. } => write!(f, "if_else_only_variable"),
            Self::IfElseAssignExpression {
                variable,
                true_block,
                false_block,
                ..
            } => write!(
                f,
                "ifelse_assign_expression({variable}, {true_block}, {false_block})"
            ),
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
                ResolvedLiteral::Map(_, data) => write!(f, "Map({data:?})"),
                ResolvedLiteral::NoneLiteral => write!(f, "NoneLiteral()"),
            },
            Self::StaticCall(static_call) => write!(
                f,
                "static call {}({:?})",
                static_call.function, static_call.arguments
            ),
            Self::StaticCallGeneric(static_call_generic) => {
                write!(
                    f,
                    "static call generic {}({:?})",
                    static_call_generic.function, static_call_generic.arguments
                )
            }
            Self::Option(inner) => write!(f, "OptionExpr({inner:?})"),
            ResolvedExpression::ArrayExtend(_, _) => write!(f, "array.extend"),
            ResolvedExpression::ArrayPush(_, _) => write!(f, "array.push"),
            ResolvedExpression::ArrayRemoveIndex(_, _) => write!(f, "array.remove"),
            ResolvedExpression::ArrayClear(_) => write!(f, "array.clear"),

            ResolvedExpression::SparseAdd(_, _) => write!(f, "sparse.add"),
            ResolvedExpression::SparseRemove(_, _) => write!(f, "sparse.remove()"),
            ResolvedExpression::SparseNew(_, _) => write!(f, "Sparse::new()"),
            ResolvedExpression::InitializeVariable(variable) => {
                write!(f, "initialize variable {variable:?}")
            }
            ResolvedExpression::ReassignVariable(variable) => {
                write!(f, "reassign variable {variable:?}")
            }
            ResolvedExpression::CoerceOptionToBool(_) => write!(f, "coerce option to bool"),
            ResolvedExpression::VariableCompoundAssignment(_) => {
                write!(f, "variable compount assignment")
            }
            ResolvedExpression::FieldCompoundAssignment(_) => {
                write!(f, "field compound assignment")
            }
            &ResolvedExpression::FloatRound(_) | &ResolvedExpression::FloatFloor(_) => {
                write!(f, "float members")
            }
            &ResolvedExpression::FloatSign(_) | &ResolvedExpression::FloatAbs(_) => {
                write!(f, "float members")
            }
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
pub enum ResolvedForPattern {
    Single(ResolvedVariableRef),
    Pair(ResolvedVariableRef, ResolvedVariableRef),
}

impl Display for ResolvedForPattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "resolved_for_pattern")
    }
}

#[derive(Debug)]
pub enum ResolvedStatement {
    // Standard
    ForLoop(ResolvedForPattern, ResolvedIterator, Vec<ResolvedStatement>),
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

    IfOnlyVariable {
        variable: ResolvedVariableRef,
        optional_expr: Box<ResolvedExpression>,
        true_block: Vec<ResolvedStatement>,
        false_block: Option<Vec<ResolvedStatement>>,
    },

    IfAssignExpression {
        variable: ResolvedVariableRef,
        optional_expr: Box<ResolvedExpression>,
        true_block: Vec<ResolvedStatement>,
        false_block: Option<Vec<ResolvedStatement>>,
    },
}

impl Display for ResolvedStatement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedStatement::ForLoop(target, iterator, statements) => {
                write!(f, "{target} {iterator:?} {statements:?}")
            }
            Self::WhileLoop(_, _) => write!(f, "WhileLoop"),
            Self::Return(_) => write!(f, "Return"),
            Self::Break => write!(f, "Break"),
            Self::Continue => write!(f, "Continue"),
            Self::Expression(expression) => write!(f, "{}", expression),
            Self::Block(_) => write!(f, "Block"),
            Self::If(_, _, _) => write!(f, "If"),
            ResolvedStatement::IfOnlyVariable { .. } => todo!(),
            ResolvedStatement::IfAssignExpression { .. } => todo!(),
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
        write!(f, "{} {{", self.name)?;
        write!(f, " {} ", comma_seq(&self.fields))?;
        write!(f, "}}")
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

pub type ResolvedOptionTypeRef = Rc<crate::ResolvedOptionType>;

#[derive(Debug)]
pub struct ResolvedOptionType {
    pub item_type: ResolvedType,
    //pub ast_type: Type,
}

// impl PartialEq for crate::ResolvedOptionType {
//     fn eq(&self, other: &Self) -> bool {
//         self.item_type == other.item_type
//     }
// }

impl Display for crate::ResolvedOptionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.item_type)
    }
}

pub type ResolvedArrayTypeRef = Rc<ResolvedArrayType>;

#[derive(Debug)]
pub struct ResolvedArrayType {
    pub item_type: ResolvedType,
    //pub ast_type: Type,
}

pub type ResolvedMapTypeRef = Rc<ResolvedMapType>;

#[derive(Debug)]
pub struct ResolvedMapType {
    pub key_type: ResolvedType,
    pub value_type: ResolvedType,
}

impl Display for ResolvedMapType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}:{}]", self.key_type, self.value_type)
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
    pub module_path: ModulePath,
    pub number: TypeNumber,
}

impl Display for ResolvedEnumType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl ResolvedEnumType {
    pub fn new(name: LocalTypeIdentifier, module_path: ModulePath, number: TypeNumber) -> Self {
        Self {
            name,
            module_path,
            number,
        }
    }

    pub fn name(&self) -> &LocalTypeIdentifier {
        &self.name
    }
}

pub type ResolvedEnumVariantTypeRef = Rc<ResolvedEnumVariantType>;

#[derive()]
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
            Self::Struct(struct_ref) => write!(f, " {} ", struct_ref),
            Self::Tuple(tuple_ref) => write!(f, "{}", tuple_ref),
            Self::Nothing => Ok(()),
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

#[derive(Debug)]
pub struct ResolvedModule {
    pub definitions: Vec<ResolvedDefinition>,
    pub statements: Vec<ResolvedStatement>,
    pub namespace: ResolvedModuleNamespace,
    pub module_path: ModulePath,
}

pub type ResolvedModuleRef = Rc<RefCell<ResolvedModule>>;

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
            module_path: module_path.clone(),
            definitions: Vec::new(),
            namespace: ResolvedModuleNamespace::new(module_path),
            statements: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub enum ResolvedDefinition {
    StructType(ResolvedStructType),
    EnumType(ResolvedEnumTypeRef, Vec<ResolvedEnumVariantType>),
    Function(),
    ExternalFunction(),
    ImplType(ResolvedType, SeqMap<IdentifierName, ResolvedFunctionRef>),
    FunctionDef(ResolvedFunction),
    Alias(ResolvedType),
    Comment(String),
}

#[derive(Debug)]
pub struct ResolvedModules {
    pub modules: HashMap<ModulePath, ResolvedModuleRef>,
}

impl Default for ResolvedModules {
    fn default() -> Self {
        Self::new()
    }
}

impl ResolvedModules {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }

    pub fn add_module(&mut self, module_ref: ResolvedModuleRef) -> Result<(), SemanticError> {
        let name = module_ref.borrow().module_path.clone();
        self.modules.insert(name, module_ref);
        Ok(())
    }

    pub fn add_linked_module(
        &mut self,
        module_path: ModulePath,
        module: ResolvedModuleRef,
    ) -> Result<(), SemanticError> {
        self.modules.insert(module_path.clone(), module);
        Ok(())
    }

    pub fn get(&self, module_path: &ModulePath) -> Option<&ResolvedModuleRef> {
        self.modules.get(module_path)
    }

    pub fn get_mut(&mut self, module_path: &ModulePath) -> Option<&mut ResolvedModuleRef> {
        self.modules.get_mut(module_path)
    }

    pub fn contains_key(&self, module_path: ModulePath) -> bool {
        self.modules.contains_key(&module_path)
    }
}

impl Display for ResolvedModules {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (module_path, module) in &self.modules {
            writeln!(f, "{}\n  {}", module_path, module.borrow())?
        }
        Ok(())
    }
}

// Immutable part
#[derive(Debug)]
pub struct ResolvedProgramTypes {
    pub int_type: ResolvedIntTypeRef,
    pub float_type: ResolvedFloatTypeRef,
    pub string_type: ResolvedStringTypeRef,
    pub bool_type: ResolvedBoolTypeRef,
    pub unit_type: ResolvedUnitTypeRef,
    pub exclusive_range_type: ResolvedExclusiveRangeTypeRef,
}

impl ResolvedProgramTypes {
    pub fn new() -> Self {
        Self {
            int_type: Rc::new(ResolvedIntType {}),
            float_type: Rc::new(ResolvedFloatType),
            string_type: Rc::new(ResolvedStringType),
            bool_type: Rc::new(ResolvedBoolType),
            unit_type: Rc::new(ResolvedUnitType),
            exclusive_range_type: Rc::new(ResolvedExclusiveRangeType),
        }
    }

    pub fn unit_type(&self) -> ResolvedType {
        ResolvedType::Unit(self.unit_type.clone())
    }

    pub fn int_type(&self) -> ResolvedType {
        ResolvedType::Int(self.int_type.clone())
    }

    pub fn float_type(&self) -> ResolvedType {
        ResolvedType::Float(self.float_type.clone())
    }

    pub fn string_type(&self) -> ResolvedType {
        ResolvedType::String(self.string_type.clone())
    }
    pub fn bool_type(&self) -> ResolvedType {
        ResolvedType::Bool(self.bool_type.clone())
    }
}

// Mutable part
#[derive(Debug)]
pub struct ResolvedProgramState {
    pub array_types: Vec<ResolvedArrayTypeRef>,
    pub number: TypeNumber,
    pub external_function_number: ExternalFunctionId,
}

impl ResolvedProgramState {
    pub fn new() -> Self {
        Self {
            array_types: Vec::new(),
            number: 0,
            external_function_number: 0,
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

#[derive(Debug)]
pub struct ResolvedProgram {
    pub types: ResolvedProgramTypes,
    pub state: ResolvedProgramState,
    pub modules: ResolvedModules,
}

impl ResolvedProgram {
    pub fn new() -> Self {
        Self {
            types: ResolvedProgramTypes::new(),
            state: ResolvedProgramState::new(),
            modules: ResolvedModules::new(),
        }
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

#[derive(Debug)]
pub struct ResolvedStaticCallGeneric {
    pub function: Rc<ResolvedFunction>,
    pub arguments: Vec<ResolvedExpression>,
    pub generic_types: Vec<ResolvedType>,
}
