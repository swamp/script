use crate::dep::{DependencyError, DependencyGraph, ModuleInfo};
use crate::ns::{ResolvedArrayType, ResolvedArrayTypeRef, BoolType, BoolTypeRef, EnumTypeRef, EnumVariantTypeRef, FloatType, FloatTypeRef, IntType, IntTypeRef, StringType, StringTypeRef, StructTypeRef, TupleTypeRef, UnitTypeRef, UnitType, StructType};
use pest::error::Error;
use seq_map::SeqMap;
use std::fmt::{Debug, Display};
use std::path::PathBuf;
use std::rc::Rc;
use std::{env, fs};
use swamp_script_ast::{
    BinaryOperator, Definition, Expression, FormatSpecifier, LocalIdentifier, LocalTypeIdentifier,
    MatchArm, ModulePath, Program, Type, UnaryOperator,
};
use swamp_script_parser::{AstParser, Rule};
use tracing::info;
use crate::ResolvedType::Unit;

mod dep;
pub mod module;
pub mod ns;

#[derive(Debug, Clone)]
pub struct ResolvedParameter {
    pub name: String,
    pub resolved_type: ResolvedType,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ResolvedType {
    Int(IntTypeRef),
    Float(FloatTypeRef),
    String(StringTypeRef),
    Bool(BoolTypeRef),
    Unit(UnitTypeRef),
    Array(ResolvedArrayTypeRef),
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

pub enum ResolvedFunctionReference {
    External(LocalTypeIdentifier, FunctionDef),
    Internal(LocalTypeIdentifier, FunctionDef, Vec<ResolvedStatement>),
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

pub struct ResolvedVariable {}
type ResolvedVariableRef = Rc<ResolvedVariable>;
pub struct ResolvedMutVariable {}
type ResolvedMutVariableRef = Rc<ResolvedMutVariable>;

pub struct ResolvedBinaryOperator {
    pub left: ResolvedExpression,
    pub right: ResolvedExpression,
    pub ast_operator_type: BinaryOperator,
}

pub struct ResolvedStructType {
    pub defined_in_module: ResolvedModuleRef,
    pub ast_struct: StructType,
}

pub struct ResolvedStruct {}

type ResolvedStructRef = Rc<ResolvedStruct>;
#[derive(Hash, Eq, PartialEq)]
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

pub struct ResolvedPattern {}

pub struct ResolvedIterator {}

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

    StructFieldAssignment(ResolvedMutStructFieldRef, Box<ResolvedExpression>),

    TupleFieldAssignment(ResolvedMutTupleFieldRef, Box<ResolvedExpression>),

    // Operators
    BinaryOp(
        Box<ResolvedExpression>,
        BinaryOperator,
        Box<ResolvedExpression>,
    ),

    UnaryOp(UnaryOperator, Box<ResolvedExpression>),

    // Calls
    FunctionCall(ResolvedFunctionReference, Vec<ResolvedExpression>),

    MutMemberCall(MutMemberRef, Vec<ResolvedExpression>),

    MemberCall(MemberRef, LocalTypeIdentifier, Vec<ResolvedExpression>),

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
    Break,                  // Return with void
    Continue,               //
    Expression(Expression), // Used for expressions with side effects (mutation, i/o)
    Block(Vec<ResolvedStatement>),
    If(
        Expression,
        Vec<ResolvedStatement>,
        Option<Vec<ResolvedStatement>>,
    ),
}

#[derive(Debug)]
pub enum ResolveError {
    DependencyError(DependencyError),
}

pub enum ResolvedDefinition {
    StructType(),
    EnumType(),
    Function(),
    ExternalFunction(),
    ImplType(),
}

pub struct Modules {}

pub struct ResolvedProgram {
    pub modules: Modules,
    int_type: IntTypeRef,
    float_type: FloatTypeRef,
    string_type: StringTypeRef,
    bool_type: BoolTypeRef,
    unit_type: UnitTypeRef,
    array_types: Vec<ResolvedArrayTypeRef>,
}

impl Display for ResolvedProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "resolved program")
    }
}

#[derive(Debug)]
pub struct ParseModule {
    pub ast_program: Program,
}

pub struct ParseRoot {
    pub base_path: PathBuf,
}

#[derive(Debug)]
pub enum ParseRootError {
    IoError(std::io::Error),
    ParseRule(Error<Rule>),
}

impl From<std::io::Error> for ParseRootError {
    fn from(err: std::io::Error) -> Self {
        Self::IoError(err)
    }
}

impl From<pest::error::Error<Rule>> for ParseRootError {
    fn from(value: Error<Rule>) -> Self {
        Self::ParseRule(value)
    }
}

#[derive(Debug)]
pub struct RelativePath(pub String);

fn to_relative_path(path: &ModulePath) -> RelativePath {
    RelativePath(
        path.0
            .iter()
            .map(|local_type_identifier| local_type_identifier.0.as_str())
            .collect::<Vec<_>>()
            .join("/"),
    )
}

impl ParseRoot {
    pub fn new(base_path: PathBuf) -> Self {
        Self { base_path }
    }

    fn to_file_system_path(&self, path: RelativePath) -> PathBuf {
        info!("converting from {path:?}");
        let mut path_buf = self.base_path.to_path_buf();

        path_buf.push(path.0);
        path_buf.set_extension("swamp");

        info!("converted to {path_buf:?}");
        path_buf
    }
    pub fn parse(&self, module_path: &ModulePath) -> Result<ParseModule, ParseRootError> {
        let path_buf = self.to_file_system_path(to_relative_path(module_path));
        let contents = fs::read_to_string(path_buf)?;

        let parser = AstParser::new();

        let ast_program = parser.parse_script(&*contents)?;

        Ok(ParseModule { ast_program })
    }
}

pub type ResolvedModuleRef = Rc<ResolvedModule>;

pub struct ResolvedModule {
    pub definitions: Vec<ResolvedDefinition>,
}

impl ResolvedProgram {
    pub fn new() -> Self {
        Self {
            modules: Modules {},
            int_type: Rc::new(IntType{}),
            float_type: Rc::new(FloatType),
            string_type: Rc::new(StringType),
            bool_type: Rc::new(BoolType),
            unit_type: Rc::new(UnitType),
            array_types: Vec::new(),
        }
    }

    pub fn resolve_array_type(&mut self, ast_type: &Type) -> Result<ResolvedArrayTypeRef, ResolveError> {
        // TODO: Check for an existing array type with exact same type

        let resolved_type = self.resolve_type(ast_type)?;

        let original_array_type = ResolvedArrayType {
            item_type: resolved_type.clone(),
            ast_type: ast_type.clone(),
        };

        let rc_array = Rc::new(original_array_type);

        self.array_types.push(rc_array);
    }

    pub fn resolve_type(&mut self, ast_type: &Type) -> Result<ResolvedType, ResolveError> {
        let resolved = match ast_type {
            Type::Int => ResolvedType::Int(self.int_type.clone()),
            Type::Float => ResolvedType::Float(self.float_type.clone()),
            Type::String => ResolvedType::String(self.string_type.clone()),
            Type::Bool => ResolvedType::Bool(self.bool_type.clone()),
            Type::Unit => ResolvedType::Unit(self.unit_type.clone()),
            Type::Struct(ast_struct) => ResolvedType::Struct(self.resolve_struct_type(ast_struct)?),
            Type::Array(ast_type) => ResolvedType::Array(self.resolve_array_type(ast_type)?),
            Type::Map(_, _) => {}
            Type::Tuple(_) => {}
            Type::Enum(_) => {}
            Type::Any => {}
        };

        Ok(resolved)
    }

    pub fn resolve_struct_type(
        &mut self,
        type_name: &LocalTypeIdentifier,
        fields: &SeqMap<LocalTypeIdentifier, Type>,
    ) -> Result<ResolvedStructRef, ResolveError> {



        let mut resolved_fields = SeqMap::new();
        for (key, field_type) in fields.iter() {
            let resolved_type = self.resolve_type(field_type)?;
            resolved_fields.insert(key, resolved_type)?;
        }
        Ok(ResolvedStruct { resolved_fields })
    }

    pub fn resolve_struct_type_definition(&mut self) -> Result<ResolvedStructRef, ResolveError> {


        self.current_module.add_struct_type();
    }

    pub fn resolve_definitions(
        &mut self,
        module: &ParseModule,
    ) -> Result<Vec<ResolvedDefinition>, ResolveError> {
        let mut resolved_definitions = vec![];
        for def in module.ast_program.definitions() {
            let resolved_definition = match def {
                Definition::StructDef(ref local_type_identifier, ref fields) => {
                    self.resolve_struct_type_definition(local_type_identifier, fields)
                }
                Definition::EnumDef(_, _) => ResolvedDefinition::EnumType(),
                Definition::FunctionDef(_, _) => ResolvedDefinition::Function(),
                Definition::ImplDef(_, _) => ResolvedDefinition::ImplType(),
                Definition::ExternalFunctionDef(_, _) => ResolvedDefinition::ExternalFunction(),
                Definition::Comment(_) => continue,
                Definition::Import(_) => continue,
            };
            resolved_definitions.push(resolved_definition);
        }

        Ok(resolved_definitions)
    }

    pub(crate) fn resolve_module(
        &mut self,
        module: &ParseModule,
    ) -> Result<ResolvedModule, ResolveError> {
        let resolved_definitions = self.resolve_definitions(module)?;
        Ok(ResolvedModule {
            definitions: resolved_definitions,
        })
    }
}

impl From<DependencyError> for ResolveError {
    fn from(value: DependencyError) -> Self {
        Self::DependencyError(value)
    }
}

fn get_current_dir() -> Result<PathBuf, std::io::Error> {
    let path = env::current_dir()?;

    //let cargo_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    Ok(path)
}

pub fn resolve(base_path: PathBuf) -> Result<ResolvedProgram, ResolveError> {
    let mut resolved_program = ResolvedProgram::new();
    let mut graph = DependencyGraph::new();
    info!(
        "{:?}",
        get_current_dir().expect("failed to get current directory")
    );
    let parse_root = ParseRoot::new(base_path);
    graph.build_graph(parse_root, ModulePath(vec![LocalIdentifier::new("main")]))?;
    let module_paths = graph.get_analysis_order()?;

    let mut resolved_modules = Vec::new();

    for module_path in module_paths {
        info!(module_path=?module_path, "ordered module");

        if let Some(parse_module) = graph.get_parsed_module(&module_path) {
            let resolved_module = resolved_program.resolve_module(parse_module);
            resolved_modules.push(resolved_module);
        } else {
            panic!("not found module");
        }
    }

    Ok(resolved_program)
}
