pub mod module;
pub mod ns;

use crate::ns::ResolvedModuleNamespace;
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
pub enum ResolvedType {
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
    Function(ResolvedFunctionRef),
    FunctionInternal(ResolvedInternalFunctionDefinitionRef),
    Void,
    ExclusiveRange,
    Any,
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
            /*
            Array(ResolvedArrayTypeRef),
            Tuple(ResolvedTupleTypeRef),
            Struct(ResolvedStructTypeRef),
            Enum(ResolvedEnumTypeRef),
            EnumVariant(ResolvedEnumVariantTypeRef),
            Function(ResolvedFunctionRef),
            FunctionInternal(ResolvedInternalFunctionDefinitionRef),
            Void,
            Range,
            Any,

             */
            _ => false,
        }
    }
}

pub fn same_type(p0: &ResolvedType, p1: &ResolvedType) -> bool {
    p0 == p1
}

#[derive(Debug)]
pub struct ResolvedInternalFunctionDefinition {
    //pub signature: ResolvedFunctionSignature,
    pub statements: Vec<ResolvedStatement>,
    pub name: LocalIdentifier,
    pub parameters: Vec<ResolvedParameter>,
    pub resolved_return_type: ResolvedType,
}

impl Display for ResolvedInternalFunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "(fn_def {}({}) -> {})",
            self.name,
            comma(&self.parameters),
            self.resolved_return_type
        )
    }
}

pub type ResolvedInternalFunctionDefinitionRef = Rc<ResolvedInternalFunctionDefinition>;

pub type ExternalFunctionId = u32;

#[derive(Debug)]
pub struct ResolvedExternalFunctionDefinition {
    //pub signature: ResolvedFunctionSignature,
    pub name: LocalIdentifier,
    pub parameters: Vec<ResolvedParameter>,
    pub resolved_return_type: ResolvedType,
    pub id: ExternalFunctionId,
}

impl Display for crate::ResolvedExternalFunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "(ext_fn_def {}({}) -> {})",
            self.name,
            comma(&self.parameters),
            self.resolved_return_type
        )
    }
}

pub type ResolvedExternalFunctionDefinitionRef = Rc<crate::ResolvedExternalFunctionDefinition>;

//pub type ResolvedTypeRef = Rc<ResolvedType>;

impl Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedType::Int(_int_type) => write!(f, "Int"),
            ResolvedType::Float(_) => write!(f, "Float"),
            ResolvedType::String(_string_type_ref) => write!(f, "String"),
            ResolvedType::Bool(_) => todo!(),
            ResolvedType::Unit(_) => todo!(),
            ResolvedType::Array(array_type_ref) => write!(f, "{array_type_ref}"),
            ResolvedType::Tuple(_) => todo!(),
            ResolvedType::Struct(struct_type) => {
                write!(f, "{}", struct_type.borrow())
            }
            ResolvedType::Enum(enum_ref) => write!(f, "{enum_ref}"),
            ResolvedType::EnumVariant(_) => todo!(),
            ResolvedType::Function(_) => todo!(),
            ResolvedType::Void => todo!(),
            ResolvedType::ExclusiveRange => todo!(),
            ResolvedType::Any => todo!(),
            ResolvedType::FunctionInternal(function_def_ref) => {
                write!(f, "{function_def_ref}")
            }
        }
    }
}

/*
pub type ResolvedFunctionSignature = (Vec<ResolvedParameter>, ResolvedType);
pub enum ResolvedFunctionReference {
    External(LocalTypeIdentifier, ResolvedFunctionSignature),
    Internal(
        LocalTypeIdentifier,
        ResolvedFunctionSignature,
        Vec<ResolvedStatement>,
    ),
}

impl Debug for ResolvedFunctionReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "FunctionRef {}({:?})",
            self.identifier(),
            self.parameters(),
        )
    }
}

impl ResolvedFunctionReference {
    pub fn parameters(&self) -> &Vec<ResolvedParameter> {
        match self {
            ResolvedFunctionReference::External(_, (params, _)) => params,
            ResolvedFunctionReference::Internal(_, (params, _), _) => params,
        }
    }

    pub fn identifier(&self) -> &LocalTypeIdentifier {
        match self {
            ResolvedFunctionReference::External(debug_name, _) => debug_name,
            ResolvedFunctionReference::Internal(debug_name, _, _) => debug_name,
        }
    }
}
*/

#[derive(Debug)]
pub struct ResolvedVariable {
    pub resolved_type: ResolvedType,
    pub ast_variable: Variable,
    pub scope_index: usize,
    pub variable_index: usize,
}

impl ResolvedVariable {
    pub fn is_mutable(&self) -> bool {
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

pub struct ResolvedVariableAccess {
    pub variable_ref: ResolvedVariableRef,
}

#[derive(Debug)]
pub struct ResolvedMutVariable {}
type ResolvedMutVariableRef = Rc<ResolvedMutVariable>;

#[derive(Debug)]

pub struct ResolvedBinaryOperator {
    pub left: Box<ResolvedExpression>,
    pub right: Box<ResolvedExpression>,
    pub ast_operator_type: BinaryOperator,
}

#[derive(Debug)]
pub struct ResolvedInternalFunctionCall {
    pub resolved_type: ResolvedType,
    pub arguments: Vec<ResolvedExpression>,
    pub function_definition: ResolvedInternalFunctionDefinitionRef,
    pub function_expression: Box<ResolvedExpression>,
}

#[derive(Debug)]
pub struct ResolvedExternalFunctionCall {
    pub resolved_type: ResolvedType,
    pub arguments: Vec<ResolvedExpression>,
    pub function_definition: ResolvedExternalFunctionDefinitionRef,
    pub function_expression: Box<ResolvedExpression>,
}

impl Display for ResolvedExternalFunctionCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "external function {}", self.resolved_type)
    }
}

pub fn comma<T: Display>(values: &[T]) -> String {
    match values.len() {
        0 => String::new(),
        1 => values[0].to_string(),
        _ => {
            let mut result = String::new();
            for (i, value) in values.iter().enumerate() {
                if i > 0 {
                    result.push_str(", ");
                }
                result.push_str(&value.to_string());
            }
            result
        }
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

pub fn comma_tuple<K: Display, V: Display>(values: &[(K, V)]) -> String {
    let mut result = String::new();
    for (i, (key, value)) in values.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        result.push_str(format!("{}: {}", key, value).as_str());
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
        write!(f, "({}({}))", self.resolved_type, comma(&self.arguments))
    }
}

#[derive(Debug)]
pub struct ResolvedMemberCall {
    pub arguments: Vec<ResolvedExpression>,
    pub struct_type_ref: ResolvedStructTypeRef,
    pub impl_member: ResolvedImplMemberRef,

    /*
    MemberRef, LocalTypeIdentifier, Vec<ResolvedExpression>
     */
    pub resolved_expression: Box<ResolvedExpression>,
}

impl Display for ResolvedMemberCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(< {} >.{}", self.resolved_expression, self.impl_member)?;

        if !self.arguments.is_empty() {
            write!(f, " <- {}", comma(&self.arguments))?;
        }

        write!(f, ")")
    }
}

type ResolvedMutStructFieldRef = Rc<ResolvedMutStructField>;
#[derive(Debug)]
pub struct ResolvedMutStructField {
    #[allow(unused)]
    ast: Expression,

    pub target_expression: ResolvedExpression,
    pub index: usize,
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
pub struct ResolvedMutArray {}

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
pub struct ResolvedIndexType {}
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
pub struct ResolvedFunction {
    #[allow(unused)]
    pub ast: Expression,

    pub function: ResolvedInternalFunctionDefinitionRef,
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
    ast: Expression,
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
    EnumTuple(ResolvedEnumTypeRef),
    EnumStruct(ResolvedEnumTypeRef),
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
            ResolvedPattern::EnumTuple(_) => write!(f, "_"),
            ResolvedPattern::EnumStruct(_) => write!(f, "_"),
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

    StructFieldAssignment(ResolvedMutStructFieldRef, Box<ResolvedExpression>),

    TupleFieldAssignment(ResolvedMutTupleFieldRef, Box<ResolvedExpression>),

    // Operators
    BinaryOp(ResolvedBinaryOperator),

    UnaryOp(UnaryOperator, Box<ResolvedExpression>),

    // Calls
    FunctionInternalCall(ResolvedInternalFunctionCall), // ResolvedFunctionReference, Vec<ResolvedExpression>
    FunctionExternalCall(ResolvedExternalFunctionCall),

    MutMemberCall(MutMemberRef, Vec<ResolvedExpression>),
    MemberCall(ResolvedMemberCall),

    Block(Vec<ResolvedStatement>),

    InterpolatedString(ResolvedStringTypeRef, Vec<ResolvedStringPart>),

    // Constructing
    StructInstantiation(ResolvedStructInstantiation),

    Array(ResolvedArrayInstantiation),
    Tuple(Vec<ResolvedExpression>),

    //Map(HashMap<ResolvedExpression, ResolvedExpression>), // Not implemented yet. Maybe call this a dictionary or similar, to avoid confusion with map()
    ExclusiveRange(Box<ResolvedExpression>, Box<ResolvedExpression>),
    //Literal(Literal),

    // Comparing
    IfElse(
        Box<ResolvedBooleanExpression>,
        Box<ResolvedExpression>,
        Box<ResolvedExpression>,
    ),
    Match(ResolvedMatch),
    LetVar(ResolvedVariableRef, Box<ResolvedExpression>),

    Literal(ResolvedLiteral),
}

#[derive(Debug)]
pub enum ResolvedLiteral {
    FloatLiteral(f32, ResolvedFloatTypeRef),
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
            ResolvedExpression::FieldAccess(field_lookup) => write!(f, "{}", field_lookup),
            ResolvedExpression::VariableAccess(variable) => write!(f, "VarRead({})", variable),
            ResolvedExpression::InternalFunctionAccess(internal_function_ref) => {
                write!(f, "{:?}", internal_function_ref)
            }
            ResolvedExpression::MutRef(_) => todo!(),
            ResolvedExpression::ArrayAccess(array_item_ref) => {
                write!(f, "[{}]", array_item_ref.item_type)
            }
            ResolvedExpression::VariableAssignment(var_assignment) => write!(
                f,
                "< {}={} >",
                var_assignment.variable_ref, var_assignment.expression
            ),
            ResolvedExpression::ArrayAssignment(_, _, _) => todo!(),
            ResolvedExpression::StructFieldAssignment(_, _) => todo!(),
            ResolvedExpression::TupleFieldAssignment(_, _) => todo!(),
            ResolvedExpression::BinaryOp(_) => todo!(),
            ResolvedExpression::UnaryOp(unary_op, expression) => {
                write!(f, "{:?}({})", unary_op, expression)
            }
            ResolvedExpression::FunctionInternalCall(resolved_call) => {
                write!(f, "{resolved_call}")
            }
            ResolvedExpression::FunctionExternalCall(resolved_call) => write!(f, "{resolved_call}"),
            ResolvedExpression::MutMemberCall(_, _) => todo!(),
            ResolvedExpression::MemberCall(member_call) => write!(f, "{member_call}"),
            ResolvedExpression::Block(_) => todo!(),
            ResolvedExpression::InterpolatedString(_string_type, parts) => {
                write!(f, "'{}'", comma(parts))
            }
            ResolvedExpression::StructInstantiation(struct_instantiation) => {
                let borrowed = struct_instantiation.struct_type_ref.borrow();
                let zipped: Vec<_> = borrowed
                    .fields
                    .keys()
                    .zip(struct_instantiation.expressions_in_order.iter())
                    .collect();

                write!(f, "{{ {} }}", comma_tuple(&zipped))?;
                Ok(())
            }
            ResolvedExpression::Array(array_instantiation) => {
                write!(f, "[{:?}]", array_instantiation.expressions)
            }
            ResolvedExpression::Tuple(_) => todo!(),
            ResolvedExpression::ExclusiveRange(_, _) => todo!(),
            ResolvedExpression::IfElse(_, _, _) => todo!(),
            ResolvedExpression::Match(resolved_match) => write!(f, "{resolved_match}"),
            ResolvedExpression::LetVar(_, _) => todo!(),
            ResolvedExpression::Literal(resolved_literal) => match resolved_literal {
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
            ResolvedStatement::ForLoop(_, _, _) => todo!(),
            ResolvedStatement::WhileLoop(_, _) => todo!(),
            ResolvedStatement::Return(_) => todo!(),
            ResolvedStatement::Break => todo!(),
            ResolvedStatement::Continue => todo!(),
            ResolvedStatement::Expression(expression) => write!(f, "{}", expression),
            ResolvedStatement::Block(_) => todo!(),
            ResolvedStatement::If(_, _, _) => todo!(),
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

#[derive(Debug)]
pub struct ResolvedStructType {
    // TODO:  pub defined_in_module: ResolvedModuleRef,
    pub number: TypeNumber,
    pub module_path: ModulePath,
    pub fields: SeqMap<IdentifierName, ResolvedType>,
    pub name: LocalTypeIdentifier,
    pub ast_struct: StructType,
    pub impl_members: SeqMap<IdentifierName, ResolvedImplMemberRef>,
    pub impl_functions: SeqMap<IdentifierName, ResolvedFunctionDataRef>,
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
            impl_members: SeqMap::default(),
            impl_functions: SeqMap::default(),
        }
    }

    pub fn field_index(&self, field_name: &LocalIdentifier) -> Option<usize> {
        self.fields
            .get_index(&IdentifierName(field_name.text.to_string()))
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

pub type ResolvedEnumVariantContainerStructTypeRef = Rc<ResolvedEnumVariantContainerStructType>;

#[derive(Debug)]
pub struct ResolvedEnumVariantContainerStructType {
    // TODO:  pub defined_in_module: ResolvedModuleRef,
    pub number: TypeNumber,
    pub module_path: ModulePath,
    pub fields: SeqMap<IdentifierName, ResolvedType>,
    pub variant_name: LocalTypeIdentifier,
    pub ast_struct: AnonymousStruct,
}

impl Display for ResolvedEnumVariantContainerStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ {} }}", comma_seq(&self.fields))
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
    Struct(ResolvedEnumVariantContainerStructTypeRef),
    Tuple(ResolvedTupleTypeRef),
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

#[derive(Debug)]
pub struct ResolvedImplMember {
    pub ast_member: ImplMember,
    pub parameters: Vec<ResolvedParameter>,
    pub return_type: ResolvedType,
    pub struct_ref: ResolvedStructTypeRef,
    pub body: Vec<ResolvedStatement>,
}

impl Display for ResolvedImplMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({}({}) -> {})",
            //self.struct_ref.borrow().name,
            self.ast_member.name,
            comma(&self.parameters),
            self.return_type
        )
    }
}

pub type ResolvedImplMemberRef = Rc<ResolvedImplMember>;

#[derive(Debug)]
pub struct ResolvedFunctionData {
    pub parameters: Vec<ResolvedParameter>,
    pub return_type: ResolvedType,
    pub statements: Vec<ResolvedStatement>,
}

pub type ResolvedFunctionDataRef = Rc<ResolvedFunctionData>;

pub struct ResolvedImplType {
    pub items: Vec<ResolvedImplItem>,
}

pub enum ResolvedImplItem {
    Member(ResolvedImplMember),
    Function(ResolvedFunctionData),
}

impl ResolvedProgram {
    pub fn new() -> Self {
        Self {
            modules: Modules::new(),
            int_type: Rc::new(ResolvedIntType {}),
            float_type: Rc::new(ResolvedFloatType),
            string_type: Rc::new(ResolvedStringType),
            bool_type: Rc::new(ResolvedBoolType),
            unit_type: Rc::new(ResolvedUnitType),
            array_types: Vec::new(),
            number: 0,
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
}

#[derive(Debug)]
pub struct Modules {
    pub modules: SeqMap<ModulePath, ResolvedModuleRef>,
}

impl Modules {
    pub fn new() -> Self {
        Self {
            modules: SeqMap::new(),
        }
    }

    pub fn add_module(&mut self, module_path: ModulePath, module: ResolvedModuleRef) {
        self.modules.insert(module_path, module).unwrap()
    }

    pub fn get(&self, module_path: &ModulePath) -> Option<&ResolvedModuleRef> {
        self.modules.get(module_path)
    }
}

impl Display for Modules {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (module_path, module) in &self.modules {
            writeln!(f, "{}\n  {}", module_path, module)?
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct ResolvedProgram {
    pub modules: Modules,
    pub int_type: ResolvedIntTypeRef,
    pub float_type: ResolvedFloatTypeRef,
    pub string_type: ResolvedStringTypeRef,
    pub bool_type: ResolvedBoolTypeRef,
    pub unit_type: ResolvedUnitTypeRef,
    pub array_types: Vec<ResolvedArrayTypeRef>,
    pub number: TypeNumber,
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
            ResolvedEnumLiteralData::Nothing => Ok(()),
            ResolvedEnumLiteralData::Tuple(tuple_data) => write!(f, "{:?}", tuple_data),
            ResolvedEnumLiteralData::Struct(struct_data) => write!(f, "{:?}", struct_data),
        }
    }
}
