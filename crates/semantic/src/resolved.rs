use crate::ns::{
    ResolveBoolTypeRef, ResolvedArrayTypeRef, ResolvedEnumTypeRef, ResolvedEnumVariantTypeRef,
    ResolvedFloatTypeRef, ResolvedIntTypeRef, ResolvedStructTypeRef, ResolvedTupleTypeRef,
    StringTypeRef, UnitTypeRef,
};
use crate::ResolvedImplMemberRef;
use std::fmt::{write, Debug, Display};
use std::rc::Rc;
use swamp_script_ast::{
    BinaryOperator, Expression, FormatSpecifier, LocalIdentifier, LocalTypeIdentifier, MatchArm,
    Parameter, UnaryOperator, Variable,
};
use swamp_script_parser::Rule::expression;

#[derive(Debug, Clone)]
pub struct ResolvedParameter {
    pub name: String,
    pub resolved_type: ResolvedType,
    pub ast_parameter: Parameter,
}

#[derive(Debug, Clone)]
pub enum ResolvedType {
    Int(ResolvedIntTypeRef),
    Float(ResolvedFloatTypeRef),
    String(StringTypeRef),
    Bool(ResolveBoolTypeRef),
    Unit(UnitTypeRef),
    Array(ResolvedArrayTypeRef),
    Tuple(ResolvedTupleTypeRef),
    Struct(ResolvedStructTypeRef),
    Enum(ResolvedEnumTypeRef),
    EnumVariant(ResolvedEnumVariantTypeRef),
    Function(ResolvedFunctionRef),
    Void,
    Range,
    Any,
}

pub type ResolvedTypeRef = Rc<ResolvedType>;

impl Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedType::Int(_int_type) => write!(f, "Int"),
            ResolvedType::Float(_) => write!(f, "Float"),
            ResolvedType::String(_) => todo!(),
            ResolvedType::Bool(_) => todo!(),
            ResolvedType::Unit(_) => todo!(),
            ResolvedType::Array(array_type_ref) => write!(f, "{array_type_ref}"),
            ResolvedType::Tuple(_) => todo!(),
            ResolvedType::Struct(struct_type) => {
                write!(f, "{}", struct_type)
            }
            ResolvedType::Enum(_) => todo!(),
            ResolvedType::EnumVariant(_) => todo!(),
            ResolvedType::Function(_) => todo!(),
            ResolvedType::Void => todo!(),
            ResolvedType::Range => todo!(),
            ResolvedType::Any => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct InternalFunctionDefinition {
    pub signature: ResolvedFunctionSignature,
    pub statements: Vec<ResolvedStatement>,
    pub name: LocalIdentifier,
}

pub type InternalFunctionDefinitionRef = Rc<InternalFunctionDefinition>;

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

#[derive(Debug)]

pub struct ResolvedVariable {
    pub resolved_type: ResolvedType,
    pub ast_variable: Variable,
}

impl Display for ResolvedVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Variable({}, {})",
            self.ast_variable.name, self.resolved_type
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
    pub left: ResolvedExpressionRef,
    pub right: ResolvedExpressionRef,
    pub ast_operator_type: BinaryOperator,
}

#[derive(Debug)]
pub struct ResolvedInternalFunctionCall {
    pub resolved_type: ResolvedType,
    pub arguments: Vec<ResolvedExpression>,
    pub function_definition: InternalFunctionDefinitionRef,
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

    pub function: InternalFunctionDefinitionRef,
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

#[derive(Debug)]
pub struct ResolvedBoolExpression(pub ResolvedExpression);

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
    FunctionCall(ResolvedInternalFunctionCall), // ResolvedFunctionReference, Vec<ResolvedExpression>
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
    IntLiteral(i32, ResolvedIntTypeRef),
}

pub type ResolvedExpressionRef = Rc<ResolvedExpression>;

impl Display for ResolvedExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedExpression::FieldAccess(_) => todo!(),
            ResolvedExpression::VariableAccess(_) => todo!(),
            ResolvedExpression::MutRef(_) => todo!(),
            ResolvedExpression::ArrayAccess(array_item_ref) => {
                write!(f, "[{}]", array_item_ref.item_type)
            }
            ResolvedExpression::VariableAssignment(_) => todo!(),
            ResolvedExpression::ArrayAssignment(_, _, _) => todo!(),
            ResolvedExpression::StructFieldAssignment(_, _) => todo!(),
            ResolvedExpression::TupleFieldAssignment(_, _) => todo!(),
            ResolvedExpression::BinaryOp(_) => todo!(),
            ResolvedExpression::UnaryOp(_, _) => todo!(),
            ResolvedExpression::FunctionCall(_) => todo!(),
            ResolvedExpression::MutMemberCall(_, _) => todo!(),
            ResolvedExpression::MemberCall(_) => todo!(),
            ResolvedExpression::Block(_) => todo!(),
            ResolvedExpression::InterpolatedString(_) => todo!(),
            ResolvedExpression::StructInstantiation(_) => todo!(),
            ResolvedExpression::Array(array_instantiation) => {
                write!(f, "[{:?}]", array_instantiation.expressions)
            }
            ResolvedExpression::Tuple(_) => todo!(),
            ResolvedExpression::ExclusiveRange(_, _) => todo!(),
            ResolvedExpression::IfElse(_, _, _) => todo!(),
            ResolvedExpression::Match(_, _) => todo!(),
            ResolvedExpression::LetVar(_, _) => todo!(),
            ResolvedExpression::FloatLiteral(_, _) => todo!(),
            ResolvedExpression::IntLiteral(_, _) => todo!(),
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
    Let(ResolvedPattern, ResolvedExpression),
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
    LetVar(ResolvedVariableRef, ResolvedExpression),
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
            ResolvedStatement::Expression(_) => todo!(),
            ResolvedStatement::Block(_) => todo!(),
            ResolvedStatement::If(_, _, _) => todo!(),
            ResolvedStatement::LetVar(variable_ref, expr) => write!(f, "{variable_ref} {expr}"),
        }
    }
}
