use crate::dep::{DependencyError, DependencyGraph};
use crate::module::Module;
use crate::ns::{
    ResolveBoolTypeRef, ResolvedAnonymousStructType, ResolvedArrayType, ResolvedArrayTypeRef,
    ResolvedBoolType, ResolvedEnumType, ResolvedEnumTypeRef, ResolvedEnumVariantContainerType,
    ResolvedEnumVariantType, ResolvedEnumVariantTypeRef, ResolvedFloatType, ResolvedFloatTypeRef,
    ResolvedIntType, ResolvedIntTypeRef, ResolvedModuleNamespace, ResolvedStructType,
    ResolvedStructTypeRef, ResolvedTupleType, ResolvedTupleTypeRef, StringType, StringTypeRef,
    TypeNumber, UnitType, UnitTypeRef,
};
use crate::resolved::same_type;

use crate::resolved::ResolvedType::Int;
use crate::resolved::{
    ResolvedArrayInstantiation, ResolvedArrayItem, ResolvedArrayItemRef, ResolvedBinaryOperator,
    ResolvedBooleanExpression, ResolvedExpression, ResolvedFunctionRef,
    ResolvedInternalFunctionCall, ResolvedInternalFunctionDefinition,
    ResolvedInternalFunctionDefinitionRef, ResolvedIterator, ResolvedMemberCall, ResolvedParameter,
    ResolvedPattern, ResolvedStatement, ResolvedStringPart, ResolvedStructInstantiation,
    ResolvedStructTypeFieldRef, ResolvedType, ResolvedTypeRef, ResolvedVariable,
    ResolvedVariableAccess, ResolvedVariableAssignment, ResolvedVariableRef,
};
use pest::error::Error;
use seq_map::SeqMap;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::env::var;
use std::fmt::{Debug, Display};
use std::path::PathBuf;
use std::rc::Rc;
use std::{env, fs};
use swamp_script_ast::Expression::Block;
use swamp_script_ast::{
    BinaryOperator, Definition, EnumVariant, Expression, FormatSpecifier, FunctionData, ImplItem,
    Literal, LocalIdentifier, LocalTypeIdentifier, MatchArm, ModulePath, Parameter, Pattern,
    Program, QualifiedTypeIdentifier, Statement, StringPart, Type, UnaryOperator, Variable,
};
use swamp_script_ast::{Node, Position, Span, StructType};
use swamp_script_parser::{AstParser, Rule};
use tracing::{debug, info, trace};

pub mod dep;
pub mod module;
pub mod ns;
mod resolved;

pub fn resolution(expression: &ResolvedExpression) -> ResolvedType {
    info!("Resolving expression {:?}", expression);
    let resolution_expression = match expression {
        ResolvedExpression::FieldAccess(struct_field_ref) => struct_field_ref.resolved_type.clone(),
        ResolvedExpression::VariableAccess(variable_ref) => variable_ref.resolved_type.clone(),
        ResolvedExpression::InternalFunctionAccess(internal_function_def) => {
            ResolvedType::FunctionInternal(internal_function_def.clone())
        }

        ResolvedExpression::MutRef(_) => todo!(),
        ResolvedExpression::ArrayAccess(array_item_ref) => array_item_ref.item_type.clone(),
        ResolvedExpression::VariableAssignment(_) => todo!(),
        ResolvedExpression::ArrayAssignment(_, _, _) => todo!(),
        ResolvedExpression::StructFieldAssignment(_, _) => todo!(),
        ResolvedExpression::TupleFieldAssignment(_, _) => todo!(),
        ResolvedExpression::BinaryOp(_) => todo!(),
        ResolvedExpression::UnaryOp(_, _) => todo!(),
        ResolvedExpression::FunctionInternalCall(internal_fn_call) => internal_fn_call
            .function_definition
            .resolved_return_type
            .clone(),
        ResolvedExpression::MutMemberCall(_, _) => todo!(),
        ResolvedExpression::MemberCall(member_call) => member_call.impl_member.return_type.clone(),
        ResolvedExpression::Block(_) => todo!(),
        ResolvedExpression::InterpolatedString(_) => todo!(),
        ResolvedExpression::StructInstantiation(struct_instantiation) => {
            ResolvedType::Struct(struct_instantiation.struct_type_ref.clone())
        }
        ResolvedExpression::Array(array_instantiation) => array_instantiation.array_type.clone(),
        ResolvedExpression::Tuple(_) => todo!(),
        ResolvedExpression::ExclusiveRange(_, _) => todo!(),
        ResolvedExpression::IfElse(_, _, _) => todo!(),
        ResolvedExpression::Match(_, _) => todo!(),
        ResolvedExpression::LetVar(_, _) => todo!(),
        ResolvedExpression::FloatLiteral(_float_value, float_type) => {
            ResolvedType::Float(float_type.clone())
        }
        ResolvedExpression::IntLiteral(_int_value, int_type) => ResolvedType::Int(int_type.clone()),
    };

    info!(resolution_expression=?resolution_expression, "resolution");

    resolution_expression
}

#[derive(Debug)]
pub enum ResolveError {
    DependencyError(DependencyError),
    CanNotFindModule(ModulePath),
    UnknownStructTypeReference(QualifiedTypeIdentifier),
    UnknownLocalStructTypeReference(LocalTypeIdentifier),
    DuplicateFieldName(LocalIdentifier),
    Unknown(String),
    Pest(Error<Rule>),
    UnknownImplTargetTypeReference(LocalTypeIdentifier),
    WrongFieldCountInStructInstantiation(
        ResolvedStructTypeRef,
        SeqMap<LocalIdentifier, Expression>,
    ),
    MissingFieldInStructInstantiation(LocalIdentifier, ResolvedStructTypeRef),
    ExpectedFunctionExpression(Expression),
    CouldNotFindMember(LocalIdentifier, Expression),
    UnknownVariable(Variable),
    NotAnArray(Expression),
    ArrayIndexMustBeInt(Expression),
    OverwriteExistingVariable(Variable),
    WrongNumberOfArguments(usize, usize),
    IncompatibleArguments(ResolvedType, ResolvedType),
}

impl From<String> for ResolveError {
    fn from(value: String) -> Self {
        Self::Unknown(value)
    }
}

impl From<pest::error::Error<Rule>> for ResolveError {
    fn from(value: pest::error::Error<Rule>) -> Self {
        Self::Pest(value)
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
    int_type: ResolvedIntTypeRef,
    float_type: ResolvedFloatTypeRef,
    string_type: StringTypeRef,
    bool_type: ResolveBoolTypeRef,
    unit_type: UnitTypeRef,
    array_types: Vec<ResolvedArrayTypeRef>,
}

impl ResolvedProgram {
    pub(crate) fn allocate_number(&self) -> TypeNumber {
        0
    }
}

#[derive(Debug)]
struct BlockScope {
    variables: SeqMap<String, ResolvedVariableRef>,
}

impl BlockScope {
    pub fn new() -> Self {
        Self {
            variables: SeqMap::new(),
        }
    }
}

pub struct Resolver<'a> {
    pub parent: &'a mut ResolvedProgram,
    pub current_module: &'a mut ResolvedModule,
    pub block_scope_stack: Vec<BlockScope>,
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
            .map(|local_type_identifier| local_type_identifier.text.as_str())
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
    pub parameters: Vec<ResolvedParameter>,
    pub return_type: ResolvedType,
}

pub type ResolvedImplMemberRef = Rc<ResolvedImplMember>;

pub struct ResolvedFunctionData {
    pub parameters: Vec<ResolvedParameter>,
    pub return_type: ResolvedType,
    pub statements: Vec<ResolvedStatement>,
}

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
            string_type: Rc::new(StringType),
            bool_type: Rc::new(ResolvedBoolType),
            unit_type: Rc::new(UnitType),
            array_types: Vec::new(),
        }
    }

    pub fn resolve_module(
        &mut self,
        module_path: ModulePath,
        module: &ParseModule,
    ) -> Result<ResolvedModuleRef, ResolveError> {
        let _resolved_module = Module::new(
            None,
            LocalIdentifier::new(
                Node {
                    span: Span {
                        start: Position {
                            offset: 0,
                            line: 0,
                            column: 0,
                        },
                        end: Position {
                            offset: 0,
                            line: 0,
                            column: 0,
                        },
                    },
                },
                "main",
            ),
            ResolvedModuleNamespace::new(),
        );

        let mut resolve_module = ResolvedModule::new(module_path.clone());

        for ast_def in module.ast_program.definitions() {
            info!("handling: {:?}", ast_def);
            let mut resolver = Resolver::new(self, &mut resolve_module);
            resolver.resolve_and_set_definition(&ast_def)?;
        }

        {
            let mut resolver = Resolver::new(self, &mut resolve_module);
            resolve_module.statements =
                resolver.resolve_statements(module.ast_program.statements())?;
        }

        let module_ref = Rc::new(resolve_module);
        self.modules.add_module(module_path, module_ref.clone());

        Ok(module_ref)
    }
}

impl<'a> Resolver<'a> {
    pub fn new(parent: &'a mut ResolvedProgram, current_module: &'a mut ResolvedModule) -> Self {
        let mut scope_stack = Vec::new();
        scope_stack.push(BlockScope::new());
        Self {
            parent,
            current_module,
            block_scope_stack: scope_stack,
        }
    }

    pub fn resolve_array_type(
        &mut self,
        ast_type: &Type,
    ) -> Result<ResolvedArrayTypeRef, ResolveError> {
        // TODO: Check for an existing array type with exact same type

        let resolved_type = self.resolve_type(ast_type)?;

        let original_array_type = ResolvedArrayType {
            item_type: resolved_type.clone(),
            //TODO: ast_type: ast_type.clone(),
        };

        let rc_array = Rc::new(original_array_type);

        self.parent.array_types.push(rc_array.clone());

        Ok(rc_array)
    }

    pub fn resolve_type(&mut self, ast_type: &Type) -> Result<ResolvedType, ResolveError> {
        let resolved = match ast_type {
            Type::Int => ResolvedType::Int(self.parent.int_type.clone()),
            Type::Float => ResolvedType::Float(self.parent.float_type.clone()),
            Type::String => ResolvedType::String(self.parent.string_type.clone()),
            Type::Bool => ResolvedType::Bool(self.parent.bool_type.clone()),
            Type::Unit => ResolvedType::Unit(self.parent.unit_type.clone()),
            Type::Struct(ast_struct) => ResolvedType::Struct(self.find_struct_type(ast_struct)?),
            Type::Array(ast_type) => ResolvedType::Array(self.resolve_array_type(ast_type)?),
            Type::Map(_, _) => todo!(),
            Type::Tuple(_types) => todo!(),
            Type::Enum(_) => todo!(),
            Type::Any => todo!(),
        };

        Ok(resolved)
    }

    pub fn find_module(&self, path: &ModulePath) -> Option<&ResolvedModuleRef> {
        self.parent.modules.get(path)
    }

    pub fn find_struct_type(
        &self,
        type_name: &QualifiedTypeIdentifier,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        let resolve_module = if let Some(path) = &type_name.module_path {
            &self
                .find_module(path)
                .ok_or(ResolveError::CanNotFindModule(path.clone()))?
                .namespace
        } else {
            &self.current_module.namespace
        };

        if let Some(found) = resolve_module.get_struct(&type_name.name) {
            Ok(found.clone())
        } else {
            Err(ResolveError::UnknownStructTypeReference(type_name.clone()))
        }
    }

    pub fn find_struct_type_local(
        &mut self,
        type_name: &LocalTypeIdentifier,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        if let Some(found) = self.current_module.namespace.get_struct(&type_name) {
            Ok(found.clone())
        } else {
            Err(ResolveError::UnknownLocalStructTypeReference(
                type_name.clone(),
            ))
        }
    }

    fn resolve_impl_definition(
        &mut self,
        attached_to_type: &LocalTypeIdentifier,
        functions: &SeqMap<LocalIdentifier, ImplItem>,
    ) -> Result<ResolvedImplType, ResolveError> {
        // Can only attach to earlier type in same module
        let _found_struct = self.find_struct_type_local(&attached_to_type)?;
        let mut items = Vec::new();

        for (_local_ident, member_item) in functions {
            let item = match member_item {
                ImplItem::Member(impl_member) => {
                    let resolved_parameters = self.resolve_parameters(&impl_member.params)?;
                    let resolved_return = self.resolve_type(&impl_member.return_type)?;

                    let resolved_impl_member = ResolvedImplMember {
                        parameters: resolved_parameters,
                        return_type: resolved_return.clone(),
                    };

                    ResolvedImplItem::Member(resolved_impl_member)
                }
                ImplItem::Function(function_data) => {
                    let resolved_parameters = self.resolve_parameters(&function_data.params)?;
                    let resolved_return = self.resolve_type(&function_data.return_type)?;
                    let resolved_statements = self.resolve_statements(&function_data.body)?;

                    ResolvedImplItem::Function(ResolvedFunctionData {
                        parameters: resolved_parameters,
                        return_type: resolved_return.clone(),
                        statements: resolved_statements,
                    })
                }
            };

            items.push(item);
        }
        Ok(ResolvedImplType { items })
    }

    pub fn resolve_struct_type_definition(
        &mut self,
        ast_struct: &StructType,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        let mut resolved_fields = SeqMap::new();

        for (name, field_type) in ast_struct.fields.iter() {
            let resolved_type = self.resolve_type(field_type)?;
            resolved_fields
                .insert(name.clone(), resolved_type)
                .map_err(|_| ResolveError::DuplicateFieldName(name.clone()))?;
        }

        let resolved_struct = ResolvedStructType::new(
            self.current_module.module_path.clone(),
            ast_struct.identifier.clone(),
            resolved_fields,
            ast_struct.clone(),
            self.parent.allocate_number(),
        );

        // Move ownership to module namespace
        let resolved_struct_ref = self
            .current_module
            .namespace
            .add_struct_type(&ast_struct.identifier, resolved_struct)?;

        Ok(resolved_struct_ref)
    }

    fn resolve_enum_type_definition(
        &mut self,
        enum_type_name: &LocalTypeIdentifier,
        ast_variants: &SeqMap<LocalTypeIdentifier, EnumVariant>,
    ) -> Result<Vec<ResolvedEnumVariantTypeRef>, ResolveError> {
        let mut resolved_variants = Vec::new();

        let resolved_parent_type = ResolvedEnumType::new(
            enum_type_name.clone(),
            self.current_module.namespace.allocate_number(),
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
                        let resolved_type = self.resolve_type(tuple_type)?;
                        vec.push(resolved_type)
                    }

                    let resolved_tuple_type = ResolvedTupleType::new(vec);
                    let resolved_tuple_type_ref = Rc::new(resolved_tuple_type);

                    ResolvedEnumVariantContainerType::Tuple(resolved_tuple_type_ref)
                }
                EnumVariant::Struct(ast_struct_fields) => {
                    let mut fields = SeqMap::new();
                    for (field, field_type) in ast_struct_fields.fields.iter() {
                        let resolved_type = self.resolve_type(field_type)?;
                        fields
                            .insert(field.clone(), resolved_type)
                            .map_err(|_| ResolveError::DuplicateFieldName(field.clone()))?;
                    }
                    let anonym = ResolvedAnonymousStructType::new(
                        self.current_module.module_path.clone(),
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
                self.current_module.namespace.allocate_number(),
            );
            let variant_ref = self.current_module.namespace.add_enum_variant(variant)?;

            resolved_variants.push(variant_ref);
        }

        Ok(resolved_variants)
    }

    fn resolve_function_definition(
        &mut self,
        identifier: &LocalIdentifier,
        function_data: &FunctionData,
    ) -> Result<ResolvedInternalFunctionDefinitionRef, ResolveError> {
        let parameters = self.resolve_parameters(&function_data.params)?;
        let resolved_return_type = self.resolve_type(&function_data.return_type)?;

        for param in &parameters {
            info!("setting variable {:?}", param);
            self.set_variable_with_type(
                &Variable::new(&param.name.clone(), param.is_mutable),
                &param.resolved_type.clone(),
            )?;
        }

        let statements = self.resolve_statements(&function_data.body)?;

        let function_def = ResolvedInternalFunctionDefinition {
            parameters,
            resolved_return_type,
            statements,
            name: identifier.clone(),
        };

        // Move ownership to module namespace
        let resolved_struct_ref = self
            .current_module
            .namespace
            .add_internal_function(identifier.text.clone(), function_def)?;

        Ok(resolved_struct_ref)
    }

    pub fn resolve_and_set_definition(&mut self, ast_def: &Definition) -> Result<(), ResolveError> {
        match ast_def {
            Definition::StructDef(ref ast_struct) => {
                self.resolve_struct_type_definition(ast_struct)?;
            }
            Definition::EnumDef(identifier, variants) => {
                self.resolve_enum_type_definition(identifier, variants)?;
            }
            Definition::ImplDef(identifier, impl_items) => {
                self.resolve_impl_definition(identifier, impl_items)?;
            }
            Definition::FunctionDef(identifier, function_info) => {
                self.resolve_function_definition(identifier, function_info)?;
            }
            Definition::ExternalFunctionDef(_, _) => todo!(),
            Definition::Comment(_) => todo!(),
            Definition::Import(_) => todo!(),
        };

        Ok(())
    }

    fn resolve_let_statement(
        &mut self,
        ast_pattern: &Pattern,
        ast_expression: &Expression,
    ) -> Result<ResolvedStatement, ResolveError> {
        let expr = self.resolve_expression(ast_expression)?;

        let resolved_let = match ast_pattern {
            Pattern::VariableAssignment(variable) => {
                let resolved_variable_ref = self.set_variable(&variable, &expr)?;
                ResolvedStatement::LetVar(resolved_variable_ref, expr)
            }
            Pattern::Tuple(_) => todo!(),
            Pattern::Struct(_) => todo!(),
            Pattern::Literal(_) => todo!(),
            Pattern::EnumTuple(_, _) => todo!(),
            Pattern::EnumStruct(_, _) => todo!(),
            Pattern::EnumSimple(_) => todo!(),
            Pattern::Wildcard => todo!(),
        };

        Ok(resolved_let)
    }

    fn resolve_let(
        &mut self,
        ast_pattern: &Pattern,
        ast_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let expr = self.resolve_expression(ast_expression)?;

        let resolved_let = match ast_pattern {
            Pattern::VariableAssignment(variable) => {
                let variable_ref = self.find_variable(variable)?;
                ResolvedExpression::LetVar(variable_ref, Box::from(expr))
            }
            Pattern::Tuple(_) => todo!(),
            Pattern::Struct(_) => todo!(),
            Pattern::Literal(_) => todo!(),
            Pattern::EnumTuple(_, _) => todo!(),
            Pattern::EnumStruct(_, _) => todo!(),
            Pattern::EnumSimple(_) => todo!(),
            Pattern::Wildcard => todo!(),
        };

        Ok(resolved_let)
    }

    fn resolve_statement(
        &mut self,
        statement: &Statement,
    ) -> Result<ResolvedStatement, ResolveError> {
        let converted = match statement {
            Statement::Let(pattern, expr) => self.resolve_let_statement(pattern, expr)?,
            Statement::ForLoop(pattern, expression, statements) => ResolvedStatement::ForLoop(
                self.resolve_pattern(pattern)?,
                self.resolve_iterator(expression)?,
                self.resolve_statements(statements)?,
            ),
            Statement::WhileLoop(expression, statements) => ResolvedStatement::WhileLoop(
                self.resolve_bool_expression(expression)?,
                self.resolve_statements(statements)?,
            ),
            Statement::Return(expression) => {
                ResolvedStatement::Return(self.resolve_expression(expression)?)
            }
            Statement::Break => ResolvedStatement::Break,
            Statement::Continue => ResolvedStatement::Break,
            Statement::Expression(expression) => {
                ResolvedStatement::Expression(self.resolve_expression(expression)?)
            }
            Statement::Block(statements) => {
                ResolvedStatement::Block(self.resolve_statements(statements)?)
            }
            Statement::If(expression, statements, maybe_else_statements) => {
                let else_statements_vector: Option<Vec<ResolvedStatement>> = maybe_else_statements
                    .as_ref()
                    .map(|else_statements| self.resolve_statements(else_statements))
                    .transpose()?;

                ResolvedStatement::If(
                    self.resolve_bool_expression(expression)?,
                    self.resolve_statements(statements)?,
                    else_statements_vector,
                )
            }
        };
        Ok(converted)
    }

    fn resolve_statements(
        &mut self,
        statements: &Vec<Statement>,
    ) -> Result<Vec<ResolvedStatement>, ResolveError> {
        let mut resolved_statements = Vec::new();
        for statement in statements {
            let resolved_statement = self.resolve_statement(statement)?;
            resolved_statements.push(resolved_statement);
        }

        Ok(resolved_statements)
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
    fn resolve_parameters(
        &mut self,
        parameters: &Vec<Parameter>,
    ) -> Result<Vec<ResolvedParameter>, ResolveError> {
        let mut resolved_parameters = Vec::new();
        for parameter in parameters {
            let param_type = self.resolve_type(&parameter.param_type)?;
            resolved_parameters.push(ResolvedParameter {
                name: parameter.variable.name.clone(),
                resolved_type: param_type,
                ast_parameter: parameter.clone(),
                is_mutable: parameter.is_mutable,
            })
        }
        Ok(resolved_parameters)
    }

    fn resolve_expression(
        &mut self,
        ast_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let expression = match ast_expression {
            // Lookups
            Expression::FieldAccess(expression, field_name) => {
                let struct_field_ref =
                    self.resolve_into_struct_field_ref(expression.as_ref(), field_name.clone())?;
                ResolvedExpression::FieldAccess(struct_field_ref)
            }
            Expression::VariableAccess(variable) => {
                self.resolve_variable_or_function_access(variable)?
            }
            Expression::MutRef(_) => todo!(),
            Expression::ArrayAccess(array_expression, lookup) => ResolvedExpression::ArrayAccess(
                self.resolve_array_access(array_expression, lookup)?,
            ),

            // Assignments
            Expression::VariableAssignment(variable_expression, source_expression) => {
                ResolvedExpression::VariableAssignment(
                    self.resolve_variable_assignment(variable_expression, source_expression)?,
                )
            }
            Expression::ArrayAssignment(_, _, _) => todo!(),
            Expression::FieldAssignment(_, _, _) => todo!(),

            // Operator
            Expression::BinaryOp(resolved_a, operator, resolved_b) => ResolvedExpression::BinaryOp(
                self.resolve_binary_op(resolved_a, operator, resolved_b)?,
            ),
            Expression::UnaryOp(operator, expression) => ResolvedExpression::UnaryOp(
                operator.clone(),
                Box::from(self.resolve_expression(expression)?),
            ),

            // Calls
            Expression::FunctionCall(function_expression, parameter_expressions) => {
                ResolvedExpression::FunctionInternalCall(
                    self.resolve_function_call(function_expression, parameter_expressions)?,
                )
            }
            Expression::MemberCall(ast_member_expression, ast_identifier, ast_arguments) => {
                ResolvedExpression::MemberCall(self.resolve_member_call(
                    ast_member_expression,
                    ast_identifier,
                    ast_arguments,
                )?)
            }
            Expression::Block(statements) => {
                let statements = self.resolve_statements(statements)?;
                ResolvedExpression::Block(statements)
            }
            Expression::InterpolatedString(string_parts) => ResolvedExpression::InterpolatedString(
                self.resolve_interpolated_string(string_parts)?,
            ),

            // Creation
            Expression::StructInstantiation(struct_identifier, fields) => {
                ResolvedExpression::StructInstantiation(
                    self.resolve_struct_instantiation(struct_identifier, fields)?,
                )
            }
            Expression::ExclusiveRange(_, _) => todo!(),

            Expression::Literal(literal) => match literal {
                Literal::Int(value) => {
                    ResolvedExpression::IntLiteral(*value, self.parent.int_type.clone())
                }
                Literal::Float(value) => {
                    ResolvedExpression::FloatLiteral(*value, self.parent.float_type.clone())
                }
                Literal::String(_) => todo!(),
                Literal::Bool(_) => todo!(),
                Literal::EnumVariant(_, _, _) => todo!(),
                Literal::Tuple(_) => todo!(),
                Literal::Array(items) => {
                    ResolvedExpression::Array(self.resolve_array_literal(items)?)
                }
                Literal::Map(_) => todo!(),
                Literal::Unit => todo!(),
            },

            // Comparison
            Expression::IfElse(_, _, _) => todo!(),
            Expression::Match(_, _) => todo!(),
        };

        Ok(expression)
    }

    fn resolve_into_struct_field_ref(
        &mut self,
        struct_expression: &Expression,
        _name: LocalIdentifier,
    ) -> Result<ResolvedStructTypeFieldRef, ResolveError> {
        let _expr = self.resolve_expression(struct_expression)?;
        //if let ResolvedExpression::StructInstantiation(data) = expr {
        //} else {
        //            Err(ResolveError::UnknownStructTypeReference(expr))
        //      }
        todo!()
    }

    #[allow(unused)]
    fn resolve_into_struct_ref(
        &self,
        _struct_expression: &Expression,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        todo!()
    }

    fn resolve_struct_instantiation(
        &mut self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
        ast_fields: &SeqMap<LocalIdentifier, Expression>,
    ) -> Result<ResolvedStructInstantiation, ResolveError> {
        let struct_to_instantiate = self.find_struct_type(qualified_type_identifier)?;

        if ast_fields.len() != struct_to_instantiate.fields.len() {
            return Err(ResolveError::WrongFieldCountInStructInstantiation(
                struct_to_instantiate,
                ast_fields.clone(),
            ));
        }

        let mut expressions_in_order = Vec::new();

        for (field_name, _field) in &struct_to_instantiate.fields {
            if let Some(found) = ast_fields.get(&field_name) {
                let resolved_expression = self.resolve_expression(found)?;
                expressions_in_order.push(resolved_expression);
            } else {
                return Err(ResolveError::MissingFieldInStructInstantiation(
                    field_name.clone(),
                    struct_to_instantiate.clone(),
                ));
            }
        }

        Ok(ResolvedStructInstantiation {
            expressions_in_order,
            struct_type_ref: struct_to_instantiate.clone(),
        })
    }

    fn resolve_bool_expression(
        &mut self,
        _p0: &Expression,
    ) -> Result<ResolvedBooleanExpression, ResolveError> {
        todo!()
    }

    fn resolve_pattern_variable(
        &mut self,
        variable: &Variable,
    ) -> Result<ResolvedPattern, ResolveError> {
        let variable_ref = self.find_variable(variable)?;
        Ok(ResolvedPattern::VariableAssignment(variable_ref))
    }

    fn resolve_pattern(&mut self, ast_pattern: &Pattern) -> Result<ResolvedPattern, ResolveError> {
        match ast_pattern {
            Pattern::VariableAssignment(variable) => self.resolve_pattern_variable(variable),
            Pattern::Tuple(_) => todo!(),
            Pattern::Struct(_) => todo!(),
            Pattern::Literal(_) => todo!(),
            Pattern::EnumTuple(_, _) => todo!(),
            Pattern::EnumStruct(_, _) => todo!(),
            Pattern::EnumSimple(_) => todo!(),
            Pattern::Wildcard => todo!(),
        }
    }

    fn resolve_iterator(&self, _p0: &Expression) -> Result<ResolvedIterator, ResolveError> {
        todo!()
    }

    fn resolve_function_call(
        &mut self,
        function_expression: &Expression,
        arguments: &Vec<Expression>,
    ) -> Result<ResolvedInternalFunctionCall, ResolveError> {
        let function_expr = self.resolve_expression(function_expression)?;
        let resolution_type = resolution(&function_expr);

        let fn_ref = match resolution_type {
            ResolvedType::FunctionInternal(ref function_call) => function_call,
            _ => {
                return Err(ResolveError::ExpectedFunctionExpression(
                    function_expression.clone(),
                ))
            }
        };

        let resolved_arguments = self.resolve_expressions(arguments)?;
        if resolved_arguments.len() != fn_ref.parameters.len() {
            return Err(ResolveError::WrongNumberOfArguments(
                resolved_arguments.len(),
                fn_ref.parameters.len(),
            ));
        }

        for (parameter_index, resolved_argument_expression) in resolved_arguments.iter().enumerate()
        {
            let parameter_type = &fn_ref.parameters[parameter_index].resolved_type;
            let argument_type = resolution(resolved_argument_expression);
            if !same_type(&argument_type, parameter_type) {
                return Err(ResolveError::IncompatibleArguments(
                    argument_type.clone(),
                    parameter_type.clone(),
                ));
            }
        }

        Ok(ResolvedInternalFunctionCall {
            resolved_type: resolution_type.clone(),
            arguments: resolved_arguments,
            function_definition: fn_ref.clone(),
        })
    }

    fn resolve_member_call(
        &mut self,
        ast_member_expression: &Expression,
        ast_member_function_name: &LocalIdentifier,
        ast_arguments: &Vec<Expression>,
    ) -> Result<ResolvedMemberCall, ResolveError> {
        let resolved_struct_type_ref = self.resolve_into_struct_ref(ast_member_expression)?;
        let resolved_arguments = self.resolve_expressions(ast_arguments)?;

        if let Some(impl_member) = resolved_struct_type_ref
            .impl_members
            .get(ast_member_function_name)
        {
            Ok(ResolvedMemberCall {
                impl_member: impl_member.clone(),
                arguments: resolved_arguments,
                struct_type_ref: resolved_struct_type_ref,
            })
        } else {
            Err(ResolveError::CouldNotFindMember(
                ast_member_function_name.clone(),
                ast_member_expression.clone(),
            ))
        }
    }

    fn resolve_binary_op(
        &mut self,
        ast_left: &Box<Expression>,
        ast_op: &BinaryOperator,
        ast_right: &Box<Expression>,
    ) -> Result<ResolvedBinaryOperator, ResolveError> {
        let left = self.resolve_expression(ast_left)?;
        let right = self.resolve_expression(ast_right)?;

        Ok(ResolvedBinaryOperator {
            left: Box::new(left),
            right: Box::new(right),
            ast_operator_type: ast_op.clone(),
        })
    }

    fn resolve_expressions(
        &mut self,
        ast_expressions: &Vec<Expression>,
    ) -> Result<Vec<ResolvedExpression>, ResolveError> {
        let mut resolved_expressions = Vec::new();
        for expression in ast_expressions {
            resolved_expressions.push(self.resolve_expression(expression)?);
        }
        Ok(resolved_expressions)
    }

    fn resolve_interpolated_string(
        &mut self,
        p0: &Vec<StringPart>,
    ) -> Result<Vec<ResolvedStringPart>, ResolveError> {
        let mut resolved_parts = Vec::new();
        for part in p0.iter() {
            let resolved_string_part = match part {
                StringPart::Literal(string) => ResolvedStringPart::Literal(string.to_string()),
                StringPart::Interpolation(expression, format_specifier) => {
                    let expr = self.resolve_expression(expression)?;
                    ResolvedStringPart::Interpolation(expr, format_specifier.clone())
                }
            };

            resolved_parts.push(resolved_string_part);
        }

        Ok(resolved_parts)
    }

    fn resolve_variable_or_function_access(
        &self,
        variable: &Variable,
    ) -> Result<ResolvedExpression, ResolveError> {
        if let Some(variable_ref) = self.try_find_variable(variable) {
            Ok(ResolvedExpression::VariableAccess(variable_ref))
        } else {
            if let Some(function_ref) = self.current_module.namespace.get_function(&variable.name) {
                Ok(ResolvedExpression::InternalFunctionAccess(
                    function_ref.clone(),
                ))
            } else {
                Err(ResolveError::UnknownVariable(variable.clone()))
            }
        }
    }

    fn find_variable(&self, variable: &Variable) -> Result<ResolvedVariableRef, ResolveError> {
        if let Some(variable_ref) = self.try_find_variable(variable) {
            Ok(variable_ref)
        } else {
            Err(ResolveError::UnknownVariable(variable.clone()))
        }
    }

    fn try_find_variable(&self, variable: &Variable) -> Option<ResolvedVariableRef> {
        trace!("lookup_in_scope -> Block for {variable:?}");
        // Look through scopes until we hit a Function scope
        for scope in self.block_scope_stack.iter().rev() {
            trace!("...checking in scope {scope:?}");
            if let Some(value) = scope.variables.get(&variable.name) {
                return Some(value.clone());
            }
        }

        None
    }

    fn resolve_array_access(
        &mut self,
        array_expression: &Expression,
        usize_expression: &Expression,
    ) -> Result<ResolvedArrayItemRef, ResolveError> {
        let resolved_array_expression = self.resolve_expression(&array_expression)?;
        debug!(resolved_array_expression=?resolved_array_expression, "resolve_array_access");
        let array_resolution = resolution(&resolved_array_expression);
        debug!(array_resolution=?array_resolution, "array_resolution");
        let item_type = match &array_resolution {
            ResolvedType::Array(item_type) => item_type,
            _ => return Err(ResolveError::NotAnArray(array_expression.clone())),
        };

        let lookup_expression = self.resolve_expression(usize_expression)?;
        debug!(lookup_expression=?lookup_expression, "lookup_expression");
        let lookup_resolution = resolution(&lookup_expression);
        debug!(lookup_resolution=?lookup_resolution, "lookup_resolution");

        match &lookup_resolution {
            ResolvedType::Int(_) => {}
            _ => Err(ResolveError::ArrayIndexMustBeInt(usize_expression.clone()))?,
        }

        let array_item = ResolvedArrayItem {
            array_type: array_resolution.clone(),
            item_type: item_type.item_type.clone(),
            int_expression: lookup_expression,
        };

        Ok(Rc::new(array_item))
    }

    fn resolve_variable_assignment(
        &mut self,
        ast_variable: &Variable,
        ast_expression: &Box<Expression>,
    ) -> Result<ResolvedVariableAssignment, ResolveError> {
        let variable_ref = self.find_variable(ast_variable)?;
        let converted_expression = self.resolve_expression(ast_expression)?;

        Ok(ResolvedVariableAssignment {
            variable_ref,
            expression: Box::from(converted_expression),
        })
    }

    fn resolve_array_literal(
        &mut self,
        items: &Vec<Expression>,
    ) -> Result<ResolvedArrayInstantiation, ResolveError> {
        let expressions = self.resolve_expressions(items)?;
        let item_type = if expressions.is_empty() {
            ResolvedType::Any
        } else {
            resolution(&expressions[0])
        };

        let array_type = ResolvedArrayType {
            item_type: item_type.clone(),
        };

        let array_type_ref = Rc::new(array_type);

        let arr = ResolvedArrayInstantiation {
            expressions,
            item_type,
            array_type_ref: array_type_ref.clone(),
            array_type: ResolvedType::Array(array_type_ref),
        };
        Ok(arr)
    }

    fn push_scope(&mut self) {
        self.block_scope_stack.push(BlockScope {
            variables: Default::default(),
        })
    }

    fn set_variable(
        &mut self,
        variable: &Variable,
        expression: &ResolvedExpression,
    ) -> Result<ResolvedVariableRef, ResolveError> {
        let variable_type_ref = resolution(expression);
        self.set_variable_with_type(variable, &variable_type_ref)
    }

    fn set_variable_with_type(
        &mut self,
        variable: &Variable,
        variable_type_ref: &ResolvedType,
    ) -> Result<ResolvedVariableRef, ResolveError> {
        if self.find_variable(&variable).is_ok() {
            return Err(ResolveError::OverwriteExistingVariable(variable.clone()));
        }

        let resolved_variable = ResolvedVariable {
            resolved_type: variable_type_ref.clone(),
            ast_variable: variable.clone(),
        };

        let variable_ref = Rc::new(resolved_variable);
        self.block_scope_stack
            .last_mut()
            .expect("block scope should have at least one scope")
            .variables
            .insert(variable.name.clone(), variable_ref.clone());

        Ok(variable_ref)
    }

    fn pop_scope(&mut self) {
        self.block_scope_stack.pop();
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
    let mut graph = DependencyGraph::new();
    let module_path = ModulePath(vec![LocalIdentifier::new(
        Node {
            span: Span {
                start: Position {
                    offset: 0,
                    line: 0,
                    column: 0,
                },
                end: Position {
                    offset: 0,
                    line: 0,
                    column: 0,
                },
            },
        },
        "main",
    )]);
    let mut resolved_program = ResolvedProgram::new();
    resolve_with_graph(base_path, module_path, &mut graph, &mut resolved_program)?;
    Ok(resolved_program)
}

pub fn resolve_with_graph(
    base_path: PathBuf,
    module_path: ModulePath,
    graph: &mut DependencyGraph,
    resolved_program: &mut ResolvedProgram,
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
            let _resolved_module = resolved_program.resolve_module(module_path, parse_module)?;
        } else {
            panic!("not found module");
        }
    }

    Ok(())
}
