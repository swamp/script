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

use crate::resolved::{
    ResolvedBinaryOperator, ResolvedBooleanExpression, ResolvedExpression,
    ResolvedInternalFunctionCall, ResolvedIterator, ResolvedMemberCall, ResolvedParameter,
    ResolvedPattern, ResolvedStatement, ResolvedStructInstantiation, ResolvedStructTypeFieldRef,
    ResolvedType, ResolvedTypeRef,
};
use pest::error::Error;
use seq_map::SeqMap;
use std::fmt::{Debug, Display};
use std::path::PathBuf;
use std::rc::Rc;
use std::{env, fs};
use swamp_script_ast::{
    BinaryOperator, Definition, EnumVariant, Expression, FormatSpecifier, ImplItem,
    LocalIdentifier, LocalTypeIdentifier, MatchArm, ModulePath, Parameter, Pattern, Program,
    QualifiedTypeIdentifier, Statement, Type, UnaryOperator,
};
use swamp_script_ast::{Node, Position, Span, StructType};
use swamp_script_parser::{AstParser, Rule};
use tracing::info;

pub mod dep;
pub mod module;
pub mod ns;
mod resolved;

pub fn resolution(expression: &ResolvedExpression) -> ResolvedType {
    match expression {
        ResolvedExpression::FieldAccess(struct_field_ref) => struct_field_ref.resolved_type.clone(),
        ResolvedExpression::VariableAccess(variable_ref) => variable_ref.resolved_type.clone(),
        ResolvedExpression::MutRef(_) => todo!(),
        ResolvedExpression::ArrayAccess(_, _) => todo!(),
        ResolvedExpression::VariableAssignment(_, _) => todo!(),
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
        ResolvedExpression::StructInstantiation(struct_instantiation) => {
            ResolvedType::Struct(struct_instantiation.struct_type_ref.clone())
        }
        ResolvedExpression::Array(_) => todo!(),
        ResolvedExpression::Tuple(_) => todo!(),
        ResolvedExpression::ExclusiveRange(_, _) => todo!(),
        ResolvedExpression::IfElse(_, _, _) => todo!(),
        ResolvedExpression::Match(_, _) => todo!(),
    }
}

#[derive(Debug)]
pub enum ResolveError {
    DependencyError(DependencyError),
    CanNotFindModule(ModulePath),
    UnknownStructTypeReference(QualifiedTypeIdentifier),
    UnknownLocalStructTypeReference(LocalTypeIdentifier),
    DuplicateFieldName(LocalIdentifier),
    Unknown(String),
    UnknownImplTargetTypeReference(LocalTypeIdentifier),
    WrongFieldCountInStructInstantiation(
        ResolvedStructTypeRef,
        SeqMap<LocalIdentifier, Expression>,
    ),
    MissingFieldInStructInstantiation(LocalIdentifier, ResolvedStructTypeRef),
    ExpectedFunctionExpression(Expression),
    CouldNotFindMember(LocalIdentifier, Expression),
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

impl ResolvedProgram {
    pub(crate) fn allocate_number(&self) -> TypeNumber {
        0
    }
}

pub struct Resolver<'a> {
    pub parent: &'a mut ResolvedProgram,
    pub current_module: &'a mut ResolvedModule,
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

        let mut resolver = Resolver::new(self, &mut resolve_module);

        resolver.resolve_definitions(module)?;

        let module_ref = Rc::new(resolve_module);
        self.modules.add_module(module_path, module_ref.clone());

        Ok(module_ref)
    }
}

impl<'a> Resolver<'a> {
    pub fn new(parent: &'a mut ResolvedProgram, current_module: &'a mut ResolvedModule) -> Self {
        Self {
            parent,
            current_module,
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
            ast_type: ast_type.clone(),
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
        functions: &SeqMap<LocalTypeIdentifier, ImplItem>,
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

    pub fn resolve_definitions(&mut self, module: &ParseModule) -> Result<(), ResolveError> {
        for def in module.ast_program.definitions() {
            match def {
                Definition::StructDef(ref ast_struct) => {
                    self.resolve_struct_type_definition(ast_struct)?;
                }
                Definition::EnumDef(identifier, variants) => {
                    self.resolve_enum_type_definition(identifier, variants)?;
                }
                Definition::ImplDef(identifier, impl_items) => {
                    self.resolve_impl_definition(identifier, impl_items)?;
                }
                Definition::FunctionDef(_, _) => todo!(),
                Definition::ExternalFunctionDef(_, _) => todo!(),
                Definition::Comment(_) => continue,
                Definition::Import(_) => continue,
            };
        }
        Ok(())
    }

    fn resolve_statement(
        &mut self,
        statement: &Statement,
    ) -> Result<ResolvedStatement, ResolveError> {
        let converted = match statement {
            Statement::Let(pattern, expr) => ResolvedStatement::Let(
                self.resolve_pattern(pattern)?,
                self.resolve_expression(expr)?,
            ),
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
            })
        }
        Ok(resolved_parameters)
    }

    fn resolve_expression(
        &self,
        ast_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let expression = match ast_expression {
            Expression::FieldAccess(expression, field_name) => {
                let struct_field_ref =
                    self.resolve_into_struct_field_ref(expression.as_ref(), field_name.clone())?;
                ResolvedExpression::FieldAccess(struct_field_ref)
            }
            Expression::VariableAccess(_variable) => todo!(),
            Expression::MutRef(_) => todo!(),
            Expression::ArrayAccess(_, _) => todo!(),
            Expression::VariableAssignment(_, _) => todo!(),
            Expression::ArrayAssignment(_, _, _) => todo!(),
            Expression::FieldAssignment(_, _, _) => todo!(),
            Expression::BinaryOp(resolved_a, operator, resolved_b) => ResolvedExpression::BinaryOp(
                self.resolve_binary_op(resolved_a, operator, resolved_b)?,
            ),
            Expression::UnaryOp(operator, expression) => ResolvedExpression::UnaryOp(
                operator.clone(),
                Box::from(self.resolve_expression(expression)?),
            ),
            Expression::FunctionCall(function_expression, parameter_expressions) => {
                ResolvedExpression::FunctionCall(
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
            Expression::Block(_) => todo!(),
            Expression::InterpolatedString(_) => todo!(),
            Expression::StructInstantiation(struct_identifier, fields) => {
                ResolvedExpression::StructInstantiation(
                    self.resolve_struct_instantiation(struct_identifier, fields)?,
                )
            }
            Expression::Array(_) => todo!(),
            Expression::Tuple(_) => todo!(),
            Expression::Map(_) => todo!(),
            Expression::ExclusiveRange(_, _) => todo!(),
            Expression::Literal(_) => todo!(),
            Expression::IfElse(_, _, _) => todo!(),
            Expression::Match(_, _) => todo!(),
        };

        Ok(expression)
    }

    fn resolve_into_struct_field_ref(
        &self,
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
        &self,
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

    fn resolve_pattern(&self, _pattern: &Pattern) -> Result<ResolvedPattern, ResolveError> {
        todo!()
    }

    fn resolve_iterator(&self, _p0: &Expression) -> Result<ResolvedIterator, ResolveError> {
        todo!()
    }

    fn resolve_function_call(
        &self,
        function_expression: &Expression,
        arguments: &Vec<Expression>,
    ) -> Result<ResolvedInternalFunctionCall, ResolveError> {
        let function_expr = self.resolve_expression(function_expression)?;
        let resolution_type = resolution(&function_expr);

        let fn_ref = match resolution_type {
            ResolvedType::Function(ref function_call) => function_call,
            _ => {
                return Err(ResolveError::ExpectedFunctionExpression(
                    function_expression.clone(),
                ))
            }
        };

        let resolved_arguments = self.resolve_expressions(arguments)?;

        Ok(ResolvedInternalFunctionCall {
            resolved_type: resolution_type.clone(),
            arguments: resolved_arguments,
            function_definition: fn_ref.function.clone(),
        })
    }

    fn resolve_member_call(
        &self,
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
        &self,
        p0: &Box<Expression>,
        p1: &BinaryOperator,
        p2: &Box<Expression>,
    ) -> Result<ResolvedBinaryOperator, ResolveError> {
        todo!()
    }

    fn resolve_expressions(
        &self,
        ast_expressions: &Vec<Expression>,
    ) -> Result<Vec<ResolvedExpression>, ResolveError> {
        let mut resolved_expressions = Vec::new();
        for expression in ast_expressions {
            resolved_expressions.push(self.resolve_expression(expression)?);
        }
        Ok(resolved_expressions)
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
