use crate::dep::{DependencyError, DependencyGraph, ModuleInfo};
use crate::module::Module;
use crate::ns::{
    EnumTypeRef, EnumVariantTypeRef, ResolveBoolTypeRef, ResolvedArrayType, ResolvedArrayTypeRef,
    ResolvedBoolType, ResolvedFloatType, ResolvedFloatTypeRef, ResolvedIntType, ResolvedIntTypeRef,
    ResolvedModuleNamespace, ResolvedStructType, ResolvedStructTypeRef, StringType, StringTypeRef,
    TupleTypeRef, UnitType, UnitTypeRef,
};
use crate::ResolvedType::Unit;
use pest::error::Error;
use seq_map::SeqMap;
use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::mem::take;
use std::path::PathBuf;
use std::rc::Rc;
use std::{env, fs};
use swamp_script_ast::{
    BinaryOperator, Definition, Expression, FormatSpecifier, LocalIdentifier, LocalTypeIdentifier,
    MatchArm, ModulePath, Program, QualifiedTypeIdentifier, StructType, Type, UnaryOperator,
};
use swamp_script_parser::Rule::parameters;
use swamp_script_parser::{AstParser, Rule};
use tracing::info;

pub mod dep;
pub mod module;
pub mod ns;

#[derive(Debug, Clone)]
pub struct ResolvedParameter {
    pub name: String,
    pub resolved_type: ResolvedType,
}

#[derive(Debug, Clone)]
pub enum ResolvedType {
    Int(ResolvedIntTypeRef),
    Float(ResolvedFloatTypeRef),
    String(StringTypeRef),
    Bool(ResolveBoolTypeRef),
    Unit(UnitTypeRef),
    Array(ResolvedArrayTypeRef),
    Tuple(TupleTypeRef),
    Struct(ResolvedStructTypeRef),
    Enum(EnumTypeRef),
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
        ResolvedStructTypeRef,
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
    CanNotFindModule(ModulePath),
    UnknownStruct(QualifiedTypeIdentifier),
    DuplicateFieldName(LocalTypeIdentifier),
    Unknown(String),
}

impl From<String> for ResolveError {
    fn from(value: String) -> Self {
        Self::Unknown(value)
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

pub struct ResolvedProgram {
    pub modules: Modules,
    int_type: ResolvedIntTypeRef,
    float_type: ResolvedFloatTypeRef,
    string_type: StringTypeRef,
    bool_type: ResolveBoolTypeRef,
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

#[derive(Debug)]
pub struct ResolvedModule {
    pub definitions: Vec<ResolvedDefinition>,
    pub namespace: ResolvedModuleNamespace,
    pub module_path: ModulePath,
}

impl ResolvedModule {
    pub fn new(module_path: ModulePath) -> Self {
        Self {
            module_path,
            definitions: Vec::new(),
            namespace: ResolvedModuleNamespace::new(),
        }
    }
}

impl ResolvedProgram {
    pub fn new() -> Self {
        Self {
            modules: Modules::new(),
            int_type: Rc::new(ResolvedIntType {}),
            float_type: Rc::new(ResolvedFloatType),
            string_type: Rc::new(StringType),
            bool_type: Rc::new(ResolvedBoolType),
            unit_type: Rc::new(UnitType),
            array_types: Vec::new(),
        }
    }

    pub fn resolve_array_type(
        &mut self,
        current_module: &mut ResolvedModule,
        ast_type: &Type,
    ) -> Result<ResolvedArrayTypeRef, ResolveError> {
        // TODO: Check for an existing array type with exact same type

        let resolved_type = self.resolve_type(current_module, ast_type)?;

        let original_array_type = ResolvedArrayType {
            item_type: resolved_type.clone(),
            ast_type: ast_type.clone(),
        };

        let rc_array = Rc::new(original_array_type);

        self.array_types.push(rc_array.clone());

        Ok(rc_array)
    }

    pub fn resolve_type(
        &mut self,
        current_module: &mut ResolvedModule,
        ast_type: &Type,
    ) -> Result<ResolvedType, ResolveError> {
        let resolved = match ast_type {
            Type::Int => ResolvedType::Int(self.int_type.clone()),
            Type::Float => ResolvedType::Float(self.float_type.clone()),
            Type::String => ResolvedType::String(self.string_type.clone()),
            Type::Bool => ResolvedType::Bool(self.bool_type.clone()),
            Type::Unit => ResolvedType::Unit(self.unit_type.clone()),
            Type::Struct(ast_struct) => {
                ResolvedType::Struct(self.resolve_struct_type(current_module, ast_struct)?)
            }
            Type::Array(ast_type) => {
                ResolvedType::Array(self.resolve_array_type(current_module, ast_type)?)
            }
            Type::Map(_, _) => todo!(),
            Type::Tuple(_) => todo!(),
            Type::Enum(_) => todo!(),
            Type::Any => todo!(),
        };

        Ok(resolved)
    }

    pub fn find_module(&self, path: &ModulePath) -> Option<&ResolvedModuleRef> {
        self.modules.get(path)
    }

    pub fn resolve_struct_type(
        &mut self,
        current_module: &mut ResolvedModule,
        type_name: &QualifiedTypeIdentifier,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        let resolve_module = if let Some(path) = &type_name.module_path {
            &self
                .find_module(path)
                .ok_or(ResolveError::CanNotFindModule(path.clone()))?
                .namespace
        } else {
            &current_module.namespace
        };

        if let Some(found) = resolve_module.get_struct(&type_name.name) {
            Ok(found.clone())
        } else {
            Err(ResolveError::UnknownStruct(type_name.clone()))
        }
    }

    pub fn resolve_struct_type_definition(
        &mut self,
        current_module: &mut ResolvedModule,
        ast_struct: &StructType,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        let mut resolved_fields = SeqMap::new();

        for (name, field_type) in &ast_struct.fields {
            let resolved_type = self.resolve_type(current_module, field_type)?;
            resolved_fields
                .insert(name.clone(), resolved_type)
                .map_err(|_| ResolveError::DuplicateFieldName(name.clone()))?;
        }

        let resolved_struct = ResolvedStructType::new(
            current_module.module_path.clone(),
            ast_struct.identifier.clone(),
            resolved_fields,
            ast_struct.clone(),
        );

        // Move ownership to module namespace
        let resolved_struct_ref = current_module
            .namespace
            .add_struct_type(&ast_struct.identifier, resolved_struct)?;

        Ok(resolved_struct_ref)
    }

    pub fn resolve_definitions(
        &mut self,
        current_module: &mut ResolvedModule,
        module: &ParseModule,
    ) -> Result<(), ResolveError> {
        for def in module.ast_program.definitions() {
            match def {
                Definition::StructDef(ref ast_struct) => {
                    self.resolve_struct_type_definition(current_module, ast_struct)?;
                }
                Definition::EnumDef(_, _) => todo!(),
                Definition::FunctionDef(_, _) => todo!(),
                Definition::ImplDef(_, _) => todo!(),
                Definition::ExternalFunctionDef(_, _) => todo!(),
                Definition::Comment(_) => continue,
                Definition::Import(_) => continue,
            };
        }
        Ok(())
    }

    pub(crate) fn resolve_module(
        &mut self,
        module_path: ModulePath,
        module: &ParseModule,
    ) -> Result<ResolvedModuleRef, ResolveError> {
        let resolved_module = Module::new(
            None,
            LocalIdentifier::new("main"),
            ResolvedModuleNamespace::new(),
        );

        let mut resolve_module = ResolvedModule::new(module_path.clone());

        let resolved_definitions = self.resolve_definitions(&mut resolve_module, module)?;

        let module_ref = Rc::new(resolve_module);
        self.modules.add_module(module_path, module_ref.clone());

        Ok(module_ref)
    }

    /*
    fn resolve_enum_type_definition(&self, p0: &mut ResolvedModule) -> Result<ResolvedEnumType> {
        todo!()
    }

    fn resolve_function_definition(&self, p0: &mut ResolvedModule) -> _ {
        todo!()
    }

    fn resolve_impl_definition(&self, p0: &mut ResolvedModule) -> _ {
        todo!()
    }

    fn resolve_external_function_definition(&self, p0: &mut ResolvedModule) -> _ {
        todo!()
    }

     */
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
    let mut graph = DependencyGraph::new();
    resolve_with_graph(base_path, &mut graph)
}

pub fn resolve_with_graph(
    base_path: PathBuf,
    mut graph: &mut DependencyGraph,
) -> Result<ResolvedProgram, ResolveError> {
    let mut resolved_program = ResolvedProgram::new();
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
            let resolved_module = resolved_program.resolve_module(module_path, parse_module)?;
            resolved_modules.push(resolved_module);
        } else {
            panic!("not found module");
        }
    }

    Ok(resolved_program)
}
