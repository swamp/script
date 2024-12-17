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

type FileId = u16;

#[derive(PartialEq, Eq, Hash, Default)]
pub struct Span {
    pub file_id: FileId,
    pub offset: u32,
    pub length: u16,
}

impl Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}:{} ({})>", self.offset, self.length, self.file_id)
    }
}




#[derive(Debug)]
pub struct ResolvedParameter {
    pub name: ResolvedLocalIdentifier,
    pub resolved_type: ResolvedType,
    pub is_mutable: bool,
}

impl Display for ResolvedParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.resolved_type)
    }
}

#[derive(Debug)]
pub struct ResolvedFunctionSignature {
    pub parameters: Vec<ResolvedParameter>,
    pub return_type: ResolvedType,
}

#[derive(Debug)]
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
}

fn compare_struct_types(a: &ResolvedStructTypeRef, b: &ResolvedStructTypeRef) -> bool {
    let struct_a = a.borrow();
    let struct_b = b.borrow();

    if struct_a.number != struct_b.number {
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
pub struct Node {
    pub span: Span,
}

#[derive(Debug)]
pub struct ResolvedLocalIdentifier(pub Node);

#[derive(Debug)]
pub struct ResolvedInternalFunctionDefinition {
    pub statements: Vec<ResolvedStatement>,
    pub name: ResolvedLocalIdentifier,
    pub signature: ResolvedFunctionSignature,
}

pub type ResolvedInternalFunctionDefinitionRef = Rc<ResolvedInternalFunctionDefinition>;

pub type ExternalFunctionId = u32;

#[derive(Debug)]
pub struct ResolvedExternalFunctionDefinition {
    pub name: ResolvedLocalIdentifier,
    pub signature: ResolvedFunctionSignature,
    pub id: ExternalFunctionId,
}

pub type ResolvedExternalFunctionDefinitionRef = Rc<crate::ResolvedExternalFunctionDefinition>;

#[derive(Debug)]
pub struct ResolvedVariable {
    pub node: Node,
    pub resolved_type: ResolvedType,
    pub scope_index: usize,
    pub variable_index: usize,
    pub is_mutable: bool,
}

pub type ResolvedVariableRef = Rc<ResolvedVariable>;

#[derive(Debug)]
pub struct ResolvedMutVariable {
    pub variable_ref: ResolvedVariableRef,
}
type ResolvedMutVariableRef = Rc<ResolvedMutVariable>;

#[derive(Debug)]
pub enum ResolvedBinaryOperatorKind {
    Add(Node),
}

#[derive(Debug)]
pub struct ResolvedBinaryOperator {
    pub left: Box<ResolvedExpression>,
    pub right: Box<ResolvedExpression>,
    pub kind: ResolvedBinaryOperatorKind,
    pub resolved_type: ResolvedType,
}

#[derive(Debug)]
pub enum ResolvedUnaryOperatorKind {
    Add(Node),
}
#[derive(Debug)]
pub struct ResolvedUnaryOperator {
    pub left: Box<ResolvedExpression>,
    pub kind: ResolvedUnaryOperatorKind,
    pub resolved_type: ResolvedType,
}

#[derive(Debug)]
pub enum ResolvedPostfixOperatorKind {
    Add(Node),
}
#[derive(Debug)]
pub struct ResolvedPostfixOperator {
    pub left: Box<ResolvedExpression>,
    pub kind: ResolvedPostfixOperatorKind,
    pub resolved_type: ResolvedType,
}

#[derive(Debug)]
pub struct ResolvedInternalFunctionCall {
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
    pub arguments: Vec<ResolvedExpression>,
    pub function_definition: ResolvedExternalFunctionDefinitionRef,
    pub function_expression: Box<ResolvedExpression>,
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
pub struct ResolvedMemberCall {
    pub function: ResolvedFunctionRef,
    pub arguments: Vec<ResolvedExpression>,
    pub self_expression: Box<ResolvedExpression>,
    pub self_is_mutable: bool,
}

#[derive(Debug)]
pub enum ResolvedAccess {
    FieldIndex(usize),
    ArrayIndex(ResolvedExpression),
    MapIndex(ResolvedExpression),
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
    pub field_name: ResolvedLocalIdentifier,
    pub resolved_type: ResolvedType,
    pub struct_expression: Box<ResolvedExpression>,
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

pub type ResolvedArrayItemRef = Rc<ResolvedArrayItem>;

#[derive(Debug)]
pub struct ResolvedIndexType {
    pub expression: Box<ResolvedExpression>,
    pub resolved_type: ResolvedType,
}

#[derive(Debug)]
pub enum ResolvedFormatSpecifier {
    Hex(Node),
}

#[derive(Debug)]
pub enum ResolvedStringPart {
    Literal(String),
    Interpolation(ResolvedExpression, Option<ResolvedFormatSpecifier>),
}

#[derive(Debug)]
pub struct ResolvedMutTupleField {
    #[allow(unused)]
    ast: ResolvedExpression,
}

pub type ResolvedFunctionRef = Rc<ResolvedFunction>;

#[derive(Debug)]
pub enum ResolvedFunction {
    Internal(ResolvedInternalFunctionDefinitionRef),
    External(ResolvedExternalFunctionDefinitionRef),
}

pub type MutMemberRef = Rc<MutMember>;

#[derive(Debug)]
pub struct MutMember {
    #[allow(unused)]
    expression: ResolvedExpression,
}

pub type MemberRef = Rc<Member>;

#[derive(Debug)]
pub struct Member {
    #[allow(unused)]
    expression: ResolvedExpression,
}

#[derive(Debug)]
pub struct ResolvedBooleanExpression {
    #[allow(unused)]
    pub expression: ResolvedExpression,
}

#[derive(Debug)]
pub struct ResolvedMatch {
    pub arms: Vec<ResolvedMatchArm>,
    pub expression: Box<ResolvedExpression>,
}

#[derive(Debug)]
pub struct ResolvedMatchArm {
    #[allow(unused)]
    pub pattern: ResolvedPattern,
    pub expression: Box<ResolvedExpression>,
    pub expression_type: ResolvedType,
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
    Wildcard(Node),
}

#[derive(Debug)]
pub struct ResolvedIterator {
    pub key_type: Option<ResolvedType>, // It does not have to support a key type
    pub value_type: ResolvedType,
    pub resolved_expression: ResolvedExpression,
    pub is_mutable: bool,
}

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
pub enum ResolvedCompoundOperator {}

#[derive(Debug)]
pub struct ResolvedVariableCompoundAssignment {
    pub variable_ref: ResolvedVariableRef, // compound only support single variable
    pub expression: Box<ResolvedExpression>,
    pub ast_operator: ResolvedCompoundOperator,
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

    FieldCompoundAssignment(
        Box<ResolvedExpression>,
        Vec<ResolvedAccess>,
        ResolvedCompoundOperator,
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
pub struct ResolvedStringConst(pub Node);

#[derive(Debug)]
pub enum ResolvedLiteral {
    FloatLiteral(Fp, ResolvedFloatTypeRef),
    UnitLiteral(ResolvedUnitTypeRef),
    NoneLiteral,
    IntLiteral(i32, ResolvedIntTypeRef),
    StringLiteral(String, ResolvedStringTypeRef),
    BoolLiteral(bool, ResolvedBoolTypeRef),
    EnumVariantLiteral(ResolvedEnumVariantTypeRef, ResolvedEnumLiteralData),
    TupleLiteral(ResolvedTupleTypeRef, Vec<ResolvedExpression>),
    Array(ResolvedArrayTypeRef, Vec<ResolvedExpression>),
    Map(
        ResolvedMapTypeRef,
        Vec<(ResolvedExpression, ResolvedExpression)>,
    ),
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

#[derive(Debug)]
pub struct ResolvedModulePathItem(pub Node);
#[derive(Debug)]
pub struct ResolvedModulePath(pub Vec<ResolvedModulePathItem>);

pub type ResolvedStructTypeRef = Rc<RefCell<ResolvedStructType>>;

pub type TypeNumber = u32;

#[derive(Debug)]
pub struct ResolvedIdentifierName(pub Node);

#[derive(Debug)]
pub struct ResolvedLocalTypeIdentifier(pub Node);

#[derive(Debug)]
pub struct ResolvedFieldName(pub Node);

#[derive(Debug)]
pub struct ResolvedStructField {
    pub identifier: ResolvedFieldName,
    pub field_type: ResolvedType,
}

#[derive(Debug)]
pub struct ResolvedStructType {
    // TODO:  pub defined_in_module: ResolvedModuleRef,
    pub module_path: ResolvedModulePath,
    pub name: ResolvedLocalTypeIdentifier,
    pub defined_fields: SeqMap<String, ResolvedStructField>,

    pub number: TypeNumber,
    pub fields: SeqMap<String, ResolvedType>,
    pub functions: SeqMap<String, ResolvedFunctionRef>,
}

impl ResolvedStructType {
    pub fn new(
        // TODO: defined_in_module: ResolvedModuleRef,
        module_path: ResolvedModulePath,
        name: ResolvedLocalTypeIdentifier,
        fields: SeqMap<ResolvedIdentifierName, ResolvedType>,
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

    pub fn field_index(&self, field_name: &str) -> Option<usize> {
        self.fields.get_index(&field_name.to_string())
    }

    pub fn name(&self) -> &ResolvedLocalTypeIdentifier {
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

pub type ResolvedEnumVariantStructTypeRef = Rc<ResolvedEnumVariantStructType>;

#[derive(Debug)]
pub struct CommonEnumVariantType {
    pub number: TypeNumber,
    pub module_path: ResolvedModulePath,
    pub variant_name: ResolvedLocalTypeIdentifier,
    pub enum_ref: ResolvedEnumTypeRef,
}

#[derive(Debug)]
pub struct ResolvedAnonymousStruct {
    pub defined_fields: SeqMap<String, ResolvedStructField>,
}

#[derive(Debug)]
pub struct ResolvedEnumVariantStructType {
    pub common: CommonEnumVariantType,

    pub fields: SeqMap<ResolvedIdentifierName, ResolvedType>, // Anonymous Struct
    pub ast_struct: ResolvedAnonymousStruct,
}

pub type ResolvedEnumVariantTupleTypeRef = Rc<ResolvedEnumVariantTupleType>;

#[derive(Debug)]
pub struct ResolvedEnumVariantTupleType {
    pub common: CommonEnumVariantType,

    pub fields_in_order: Vec<ResolvedType>,
}

pub type ResolvedTupleTypeRef = Rc<ResolvedTupleType>;

#[derive(Debug)]
pub struct ResolvedTupleType(pub Vec<ResolvedType>);

impl ResolvedTupleType {
    pub fn new(types: Vec<ResolvedType>) -> Self {
        Self(types)
    }
}

pub type ResolvedEnumTypeRef = Rc<ResolvedEnumType>;

#[derive(Debug)]
pub struct ResolvedEnumType {
    pub name: ResolvedLocalTypeIdentifier,
    pub module_path: ResolvedModulePath,
    pub number: TypeNumber,
}

impl ResolvedEnumType {
    pub fn new(
        name: ResolvedLocalTypeIdentifier,
        module_path: ResolvedModulePath,
        number: TypeNumber,
    ) -> Self {
        Self {
            name,
            module_path,
            number,
        }
    }

    pub fn name(&self) -> &ResolvedLocalTypeIdentifier {
        &self.name
    }
}

pub type ResolvedEnumVariantTypeRef = Rc<ResolvedEnumVariantType>;

#[derive(Debug)]
pub struct ResolvedEnumVariantType {
    pub owner: ResolvedEnumTypeRef,
    pub data: ResolvedEnumVariantContainerType,
    pub name: ResolvedLocalTypeIdentifier,
    pub number: TypeNumber,
}

pub type ResolvedEnumVariantStructFieldTypeRef = Rc<ResolvedEnumVariantStructFieldType>;

#[derive(Debug)]
pub struct ResolvedEnumVariantStructFieldType {
    pub name: ResolvedLocalIdentifier,
    pub field_index: usize,
    pub enum_variant: ResolvedEnumVariantTypeRef,
    pub resolved_type: ResolvedType,
}

pub type ResolvedEnumVariantTupleFieldTypeRef = Rc<ResolvedEnumVariantTupleFieldType>;

#[derive(Debug)]
pub struct ResolvedEnumVariantTupleFieldType {
    pub name: ResolvedLocalIdentifier,
    pub field_index: usize,
    pub enum_variant: ResolvedEnumVariantTypeRef,
    pub resolved_type: ResolvedType,
}

impl ResolvedEnumVariantType {
    pub fn new(
        owner: ResolvedEnumTypeRef,
        name: ResolvedLocalTypeIdentifier,
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

    pub fn name(&self) -> &ResolvedLocalTypeIdentifier {
        &self.name
    }

    pub fn complete_name(&self) -> String {
        self.owner.name.text.to_string() + "::" + &*self.name.text.to_string()
    }
}

#[derive(Debug, Clone)]
pub enum ResolvedEnumVariantContainerType {
    Struct(ResolvedEnumVariantStructTypeRef),
    Tuple(ResolvedEnumVariantTupleTypeRef),
    Nothing,
}

#[derive(Debug)]
pub struct ResolvedImplMember {}

#[derive(Debug)]
pub struct ImplType {
    pub members: SeqMap<String, ResolvedImplMember>,
    pub associated_with_struct_type: ResolvedStructTypeRef,
}

impl ImplType {
    pub fn new(
        members: SeqMap<String, ResolvedImplMember>,
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
    pub module_path: ResolvedModulePath,
}

pub type ResolvedModuleRef = Rc<RefCell<ResolvedModule>>;

impl ResolvedModule {
    pub fn new(module_path: ResolvedModulePath) -> Self {
        Self {
            module_path,
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
    ImplType(ResolvedType),
    FunctionDef(ResolvedFunction),
    Alias(ResolvedType),
    Comment(String),
}

#[derive(Debug)]
pub struct ResolvedModules {
    pub modules: HashMap<ResolvedModulePath, ResolvedModuleRef>,
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
        module_path: ResolvedModulePath,
        module: ResolvedModuleRef,
    ) -> Result<(), SemanticError> {
        self.modules.insert(module_path.clone(), module);
        Ok(())
    }

    pub fn get(&self, module_path: &ResolvedModulePath) -> Option<&ResolvedModuleRef> {
        self.modules.get(module_path)
    }

    pub fn get_mut(&mut self, module_path: &ResolvedModulePath) -> Option<&mut ResolvedModuleRef> {
        self.modules.get_mut(module_path)
    }

    pub fn contains_key(&self, module_path: ResolvedModulePath) -> bool {
        self.modules.contains_key(&module_path)
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

#[derive(Debug)]
pub enum ResolvedEnumLiteralData {
    Nothing,
    Tuple(Vec<ResolvedExpression>),
    Struct(Vec<ResolvedExpression>),
}

#[derive(Debug)]
pub struct ResolvedStaticCallGeneric {
    pub function: Rc<ResolvedFunction>,
    pub arguments: Vec<ResolvedExpression>,
    pub generic_types: Vec<ResolvedType>,
}
