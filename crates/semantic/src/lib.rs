use crate::ns::{EnumTypeRef, EnumVariantTypeRef, StructTypeRef, TupleTypeRef};
use std::fmt::Debug;
use std::rc::Rc;
use seq_map::SeqMap;
use swamp_script_ast::{BinaryOperator, Expression, FormatSpecifier, LocalTypeIdentifier, MatchArm, UnaryOperator};

pub mod module;
pub mod ns;

pub struct ResolvedParameter {
    pub name: String,
    pub resolved_type: ResolvedType,
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum ResolvedType {
    Int,
    Float,
    String,
    Bool,
    Array(Box<ResolvedType>),
    Tuple(TupleTypeRef),
    Struct(StructTypeRef), // The name of the struct
    Enum(EnumTypeRef), // Combination of the EnumTypeName and the variant name and the type of the data
    EnumVariant(EnumVariantTypeRef),
    Function,
    Void,
    Range,
    Any,
}

type FunctionDef = (Vec<ResolvedParameter>, ResolvedType);

pub enum ResolvedFunctionRef {
    External(LocalTypeIdentifier, FunctionDef),
    Internal(LocalTypeIdentifier, FunctionDef, Vec<ResolvedStatement>),
}

impl Debug for ResolvedFunctionRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "FunctionRef {}({:?})",
            self.identifier(),
            self.parameters(),
        )
    }
}

impl Clone for ResolvedFunctionRef {
    fn clone(&self) -> Self {
        match self {
            FunctionRef::External(debug_name, def) => {
                FunctionRef::External(debug_name.clone(), def.clone())
            }
            FunctionRef::Internal(debug_name, def, body) => {
                FunctionRef::Internal(debug_name.clone(), def.clone(), body.clone())
            }
        }
    }
}

impl ResolvedFunctionRef {
    pub fn parameters(&self) -> &Vec<ResolvedParameter> {
        match self {
            FunctionRef::External(_, (params, _)) => params,
            FunctionRef::Internal(_, (params, _), _) => params,
        }
    }

    pub fn identifier(&self) -> &LocalTypeIdentifier {
        match self {
            FunctionRef::External(debug_name, _) => debug_name,
            FunctionRef::Internal(debug_name, _, _) => debug_name,
        }
    }
}

pub struct ResolvedVariable {}
type ResolvedVariableRef = Rc<ResolvedVariable>;
pub struct ResolvedMutVariable {}
type ResolvedMutVariableRef = Rc<ResolvedMutVariable>;

pub struct ResolvedBinaryOperator {
    pub left: ResolvedExpression,
    pub right: ResolvedExpression,
    pub ast_operator_type: BinaryOperator,
}


pub struct ResolvedStruct {}
type ResolvedStructRef = Rc<ResolvedStruct>;
pub struct ResolvedStructField {}

type ResolvedStructFieldRef = Rc<ResolvedStructField>;

type ResolvedArrayRef = Rc<ResolvedArray>;
type ResolvedMutArrayRef = Rc<ResolvedMutArray>;
pub struct ResolvedArray {}
pub struct ResolvedMutArray {}
pub struct ResolvedIndexType {}
type ResolvedIndexTypeRef = Rc<ResolvedIndexType>;

enum ResolvedStringPart {
    Literal(String),
    Interpolation(Box<ResolvedExpression>, Option<FormatSpecifier>),
}

type ResolvedMutStructFieldRef = Rc<ResolvedMutStructField>;
pub struct ResolvedMutStructField {
    ast: Expression,
}

type ResolvedMutTupleFieldRef = Rc<ResolvedMutTupleField>;

pub struct ResolvedMutTupleField {
    ast: Expression,
}

type ResolvedFunctionRef = Rc<ResolvedFunction>;
pub struct ResolvedFunction {
    ast: Expression,
}

type MutMemberRef = Rc<MutMember>;
pub struct MutMember {
    ast: Expression,
}

type MemberRef = Rc<Member>;
pub struct Member {
    ast: Expression,
}

pub struct ResolvedBooleanExpression {
    ast: Expression,
}

pub struct ResolvedMatchArm {
    ast_match_arm: MatchArm,
}

pub struct ResolvedPattern {

}

pub struct ResolvedIterator {

}

pub enum ResolvedExpression {
    // Access Lookup values
    FieldAccess(ResolvedStructFieldRef),
    VariableAccess(ResolvedVariableRef),
    MutRef(ResolvedMutVariableRef), // Used when passing with mut keyword. mut are implicitly passed by reference
    ArrayAccess(ResolvedArrayRef, ResolvedIndexTypeRef), // Read from an array: arr[3]

    // Assignment

    // Since it is a cool language, we can "chain" assignments together. like a = b = c = 1. Even for field assignments, like a.b = c.d = e.f = 1
    VariableAssignment(ResolvedMutVariableRef, ResolvedIndexTypeRef),

    ArrayAssignment(
        ResolvedMutArrayRef,
        ResolvedIndexTypeRef,
        Box<ResolvedExpression>,
    ), // target, index, source. Write to an index in an array: arr[3] = 42

    StructFieldAssignment(
        ResolvedMutStructFieldRef,
        Box<ResolvedExpression>,
    ),

    TupleFieldAssignment(
        ResolvedMutTupleFieldRef,
        Box<ResolvedExpression>,
    ),

    // Operators
    BinaryOp(
        Box<ResolvedExpression>,
        BinaryOperator,
        Box<ResolvedExpression>,
    ),

    UnaryOp(UnaryOperator, Box<ResolvedExpression>),

    // Calls
    FunctionCall(ResolvedFunctionRef, Vec<ResolvedExpression>),

    MutMemberCall(
        MutMemberRef,
        Vec<ResolvedExpression>,
    ),


    MemberCall(
        MemberRef,
        LocalTypeIdentifier,
        Vec<ResolvedExpression>,
    ),

    Block(Vec<ResolvedStatement>),

    InterpolatedString(Vec<ResolvedStringPart>),

    // Constructing
    StructInstantiation(
        ResolvedStructRef,
        SeqMap<ResolvedStructFieldRef, ResolvedExpression>,
    ),

    Array(Vec<ResolvedExpression>),
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
}

pub enum ResolvedStatement {
    // Standard
    Let(ResolvedPattern, ResolvedExpression),
    ForLoop(ResolvedPattern, ResolvedIterator, Vec<ResolvedStatement>),
    WhileLoop(ResolvedBooleanExpression, Vec<ResolvedStatement>),
    Return(ResolvedExpression),
    Break,                                   // Return with void
    Continue,                                //
    Expression(Expression), // Used for expressions with side effects (mutation, i/o)
    Block(Vec<ResolvedStatement>),
    If(
        Expression,
        Vec<ResolvedStatement>,
        Option<Vec<ResolvedStatement>>,
    ),
}
