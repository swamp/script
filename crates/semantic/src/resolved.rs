use crate::ns::{
    ResolvedArrayTypeRef, ResolvedBoolTypeRef, ResolvedEnumTypeRef, ResolvedEnumVariantTypeRef,
    ResolvedFloatTypeRef, ResolvedIntTypeRef, ResolvedStringTypeRef, ResolvedStructTypeRef,
    ResolvedTupleTypeRef, ResolvedUnitTypeRef,
};
use crate::ResolvedImplMemberRef;
use seq_map::SeqMap;
use std::env::var;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::rc::Rc;
use swamp_script_ast::{
    BinaryOperator, Expression, FormatSpecifier, IdentifierName, LocalIdentifier, MatchArm,
    Parameter, StringConst, UnaryOperator, Variable,
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
    Range,
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
            ResolvedType::Enum(_) => todo!(),
            ResolvedType::EnumVariant(_) => todo!(),
            ResolvedType::Function(_) => todo!(),
            ResolvedType::Void => todo!(),
            ResolvedType::Range => todo!(),
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
            "{}{}: {}",
            prefix, self.ast_variable.name, self.resolved_type
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
}

impl Display for ResolvedMemberCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} <- {})", self.impl_member, comma(&self.arguments))
    }
}

#[derive(Debug)]
pub struct ResolvedStructTypeField {
    pub struct_type_ref: ResolvedStructTypeRef,
    pub index: usize,
    pub resolved_type: ResolvedType,
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

type ResolvedMutStructFieldRef = Rc<ResolvedMutStructField>;
#[derive(Debug)]
pub struct ResolvedMutStructField {
    #[allow(unused)]
    ast: Expression,
}

pub type ResolvedMutTupleFieldRef = Rc<ResolvedMutTupleField>;

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

pub struct ResolvedMatchArm {
    #[allow(unused)]
    ast_match_arm: MatchArm,
}

#[derive(Debug)]
pub enum ResolvedPattern {
    VariableAssignment(ResolvedVariableRef),
}

#[derive(Debug)]
pub struct ResolvedIterator {}

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
    MutMemberCall(MutMemberRef, Vec<ResolvedExpression>),
    MemberCall(ResolvedMemberCall),

    Block(Vec<ResolvedStatement>),

    InterpolatedString(Vec<ResolvedStringPart>),

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
    Match(Box<ResolvedExpression>, Vec<ResolvedMatchArm>),
    LetVar(ResolvedVariableRef, Box<ResolvedExpression>),
    FloatLiteral(f32, ResolvedFloatTypeRef),
    UnitLiteral(ResolvedUnitTypeRef),
    IntLiteral(i32, ResolvedIntTypeRef),
    StringLiteral(StringConst, ResolvedStringTypeRef),
    BoolLiteral(bool, ResolvedBoolTypeRef),
}

//pub type ResolvedExpressionRef = Rc<ResolvedExpression>;

impl Display for ResolvedExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedExpression::FieldAccess(_) => todo!(),
            ResolvedExpression::VariableAccess(_) => todo!(),
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
            ResolvedExpression::UnaryOp(_, _) => todo!(),
            ResolvedExpression::FunctionInternalCall(resolved_call) => {
                write!(f, "{resolved_call}")
            }
            ResolvedExpression::MutMemberCall(_, _) => todo!(),
            ResolvedExpression::MemberCall(member_call) => write!(f, "{member_call}"),
            ResolvedExpression::Block(_) => todo!(),
            ResolvedExpression::InterpolatedString(_) => todo!(),
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
            ResolvedExpression::Match(_, _) => todo!(),
            ResolvedExpression::LetVar(_, _) => todo!(),
            ResolvedExpression::FloatLiteral(value, _float_type) => {
                write!(f, "FloatLit({value:?})")
            }
            ResolvedExpression::IntLiteral(value, _int_type) => write!(f, "IntLit({value:?})"),
            ResolvedExpression::StringLiteral(value, _string_type) => {
                write!(f, "StringLit({value:?})")
            }
            ResolvedExpression::UnitLiteral(_unit_lit) => write!(f, "UnitLit"),
            ResolvedExpression::BoolLiteral(value, _bool_type_ref) => {
                write!(f, "BoolLit({value:?})")
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
