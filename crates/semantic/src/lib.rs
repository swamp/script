use crate::dep::{DependencyError, DependencyGraph, ModuleInfo};
use crate::module::Module;
use crate::ns::{
    ResolveBoolTypeRef, ResolvedAnonymousStructType, ResolvedArrayType, ResolvedArrayTypeRef,
    ResolvedBoolType, ResolvedEnumType, ResolvedEnumTypeRef, ResolvedEnumVariantContainerType,
    ResolvedEnumVariantType, ResolvedEnumVariantTypeRef, ResolvedFloatType, ResolvedFloatTypeRef,
    ResolvedIntType, ResolvedIntTypeRef, ResolvedModuleNamespace, ResolvedStructType,
    ResolvedStructTypeRef, ResolvedTupleType, ResolvedTupleTypeRef, StringType, StringTypeRef,
    UnitType, UnitTypeRef,
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
    BinaryOperator, Definition, EnumVariant, Expression, FormatSpecifier, LocalIdentifier,
    LocalTypeIdentifier, MatchArm, ModulePath, Program, QualifiedTypeIdentifier, StructType, Type,
    UnaryOperator,
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
    Tuple(ResolvedTupleTypeRef),
    Struct(ResolvedStructTypeRef),
    Enum(ResolvedEnumTypeRef),
    EnumVariant(ResolvedEnumVariantTypeRef),
    Function,
    Void,
    Range,
    Any,
}

impl Display for ResolvedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolvedType::Int(int_type) => write!(f, "Int"),
            ResolvedType::Float(_) => todo!(),
            ResolvedType::String(_) => todo!(),
            ResolvedType::Bool(_) => todo!(),
            Unit(_) => todo!(),
            ResolvedType::Array(_) => todo!(),
            ResolvedType::Tuple(_) => todo!(),
            ResolvedType::Struct(struct_type) => {
                write!(f, "{}", struct_type)
            }
            ResolvedType::Enum(_) => todo!(),
            ResolvedType::EnumVariant(_) => todo!(),
            ResolvedType::Function => todo!(),
            ResolvedType::Void => todo!(),
            ResolvedType::Range => todo!(),
            ResolvedType::Any => todo!(),
        }
    }
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
    UnknownStructTypeReference(QualifiedTypeIdentifier),
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
            writeln!(f, "{}\n  {}", module_path, module.namespace)?
        }
        Ok(())
    }
}

#[derive(Debug)]
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
        write!(f, "modules:\n{}", self.modules)
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

impl Display for ResolvedModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "namespace: {}", self.namespace)
    }
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
            Type::Tuple(types) => todo!(),
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
            Err(ResolveError::UnknownStructTypeReference(type_name.clone()))
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
            current_module.namespace.allocate_number(),
        );

        // Move ownership to module namespace
        let resolved_struct_ref = current_module
            .namespace
            .add_struct_type(&ast_struct.identifier, resolved_struct)?;

        Ok(resolved_struct_ref)
    }

    fn resolve_enum_type_definition(
        &mut self,
        current_module: &mut ResolvedModule,
        enum_type_name: &LocalTypeIdentifier,
        ast_variants: &SeqMap<LocalTypeIdentifier, EnumVariant>,
    ) -> Result<Vec<ResolvedEnumVariantTypeRef>, ResolveError> {
        let mut resolved_variants = Vec::new();

        let resolved_parent_type = ResolvedEnumType::new(
            enum_type_name.clone(),
            current_module.namespace.allocate_number(),
        );

        let parent_ref = Rc::new(resolved_parent_type);

        for (name, ast_enum_variant) in ast_variants {
            //let resolved_type = self.resolve_type(current_module, ast_variant)?;
            //resolved_fields
            //  .insert(name.clone(), resolved_type)
            //.map_err(|_| ResolveError::DuplicateFieldName(name.clone()))?;

            let container = match ast_enum_variant {
                EnumVariant::Simple => ResolvedEnumVariantContainerType::Nothing,
                EnumVariant::Tuple(types) => {
                    let mut vec = Vec::new();
                    for tuple_type in types {
                        let resolved_type = self.resolve_type(current_module, tuple_type)?;
                        vec.push(resolved_type)
                    }

                    let resolved_tuple_type = ResolvedTupleType::new(vec);
                    let resolved_tuple_type_ref = Rc::new(resolved_tuple_type);

                    ResolvedEnumVariantContainerType::Tuple(resolved_tuple_type_ref)
                }
                EnumVariant::Struct(ast_struct_fields) => {
                    let mut fields = SeqMap::new();
                    for (field, field_type) in &ast_struct_fields.fields {
                        let resolved_type = self.resolve_type(current_module, field_type)?;
                        fields
                            .insert(field.clone(), resolved_type)
                            .map_err(|_| ResolveError::DuplicateFieldName(name.clone()))?;
                    }
                    let anonym = ResolvedAnonymousStructType::new(
                        current_module.module_path.clone(),
                        fields,
                        ast_struct_fields.clone(),
                    );
                    ResolvedEnumVariantContainerType::Struct(anonym)
                }
            };

            let variant = ResolvedEnumVariantType::new(
                parent_ref.clone(),
                name.clone(),
                container,
                current_module.namespace.allocate_number(),
            );
            let variant_ref = current_module.namespace.add_enum_variant(variant)?;

            resolved_variants.push(variant_ref);
        }

        Ok(resolved_variants)
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
                Definition::EnumDef(identifier, variants) => {
                    self.resolve_enum_type_definition(current_module, identifier, variants)?;
                }
                Definition::ImplDef(_, _) => todo!(),
                Definition::FunctionDef(_, _) => todo!(),
                Definition::ExternalFunctionDef(_, _) => todo!(),
                Definition::Comment(_) => continue,
                Definition::Import(_) => continue,
            };
        }
        Ok(())
    }

    pub fn resolve_module(
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
    let module_path = ModulePath(vec![LocalIdentifier::new("main")]);
    let mut resolved_program = ResolvedProgram::new();
    resolve_with_graph(base_path, module_path, &mut graph, &mut resolved_program)?;
    Ok(resolved_program)
}

pub fn resolve_with_graph(
    base_path: PathBuf,
    module_path: ModulePath,
    mut graph: &mut DependencyGraph,
    mut resolved_program: &mut ResolvedProgram,
) -> Result<(), ResolveError> {
    info!(
        "{:?}",
        get_current_dir().expect("failed to get current directory")
    );
    let parse_root = ParseRoot::new(base_path);
    graph.build_graph(parse_root, module_path)?;
    let module_paths = graph.get_analysis_order()?;
    info!(module_paths=?module_paths, "analysis module");
    //let mut resolved_modules = Vec::new();

    for module_path in module_paths {
        info!(module_path=?module_path, "ordered module");

        if let Some(parse_module) = graph.get_parsed_module(&module_path) {
            let resolved_module = resolved_program.resolve_module(module_path, parse_module)?;
        } else {
            panic!("not found module");
        }
    }

    Ok(())
}
