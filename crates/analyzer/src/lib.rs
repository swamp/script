use crate::dep::{DependencyError, DependencyParser};

use pest::error::Error;
use seq_map::SeqMap;
use std::fmt::Display;
use std::path::PathBuf;
use std::rc::Rc;
use std::{env, fs};
use swamp_script_ast::prelude::*;
use swamp_script_parser::{AstParser, Rule};
use swamp_script_semantic::ns::{ResolvedModuleNamespace, SemanticError};
use swamp_script_semantic::prelude::*;
use tracing::{debug, error, info, trace};

pub mod dep;

pub fn resolution(expression: &ResolvedExpression) -> ResolvedType {
    trace!("resolution expression {}", expression);
    let resolution_expression = match expression {
        ResolvedExpression::FieldAccess(struct_field_ref) => struct_field_ref.resolved_type.clone(),
        ResolvedExpression::VariableAccess(variable_ref) => variable_ref.resolved_type.clone(),
        ResolvedExpression::InternalFunctionAccess(internal_function_def) => {
            ResolvedType::FunctionInternal(internal_function_def.clone())
        }
        ResolvedExpression::ExternalFunctionAccess(external_function_def) => {
            ResolvedType::FunctionExternal(external_function_def.clone())
        }
        ResolvedExpression::MutRef(mut_var_ref) => mut_var_ref.variable_ref.resolved_type.clone(),
        ResolvedExpression::ArrayAccess(array_item_ref) => array_item_ref.item_type.clone(),
        ResolvedExpression::VariableAssignment(variable_assignment) => {
            variable_assignment.variable_ref.resolved_type.clone()
        }
        ResolvedExpression::ArrayAssignment(_, _, _) => todo!(),
        ResolvedExpression::StructFieldAssignment(_, _) => todo!(),
        ResolvedExpression::TupleFieldAssignment(_, _) => todo!(),
        ResolvedExpression::BinaryOp(binary_op) => binary_op.resolved_type.clone(),
        ResolvedExpression::UnaryOp(unary_op) => unary_op.resolved_type.clone(),
        ResolvedExpression::FunctionInternalCall(internal_fn_call) => internal_fn_call
            .function_definition
            .resolved_return_type
            .clone(),
        ResolvedExpression::FunctionExternalCall(external_fn_call) => external_fn_call
            .function_definition
            .resolved_return_type
            .clone(),
        ResolvedExpression::MutMemberCall(_, _) => todo!(),
        ResolvedExpression::MemberCall(member_call) => member_call.impl_member.return_type.clone(),
        ResolvedExpression::Block(_statements) => ResolvedType::Unit(Rc::new(ResolvedUnitType)), // TODO: Fix this
        ResolvedExpression::InterpolatedString(string_type, _parts) => {
            ResolvedType::String(string_type.clone())
        }
        ResolvedExpression::StructInstantiation(struct_instantiation) => {
            ResolvedType::Struct(struct_instantiation.struct_type_ref.clone())
        }
        ResolvedExpression::Array(array_instantiation) => array_instantiation.array_type.clone(),
        ResolvedExpression::Tuple(_) => todo!(),
        ResolvedExpression::ExclusiveRange(range_type, _, _) => {
            ResolvedType::ExclusiveRange(range_type.clone())
        }
        ResolvedExpression::IfElse(_condition, consequence, _alternate) => resolution(&consequence),
        ResolvedExpression::Match(resolved_match) => resolved_match.arms[0].expression_type.clone(),
        ResolvedExpression::LetVar(_, _) => todo!(),
        ResolvedExpression::Literal(literal) => match literal {
            ResolvedLiteral::BoolLiteral(_value, bool_type_ref) => {
                ResolvedType::Bool(bool_type_ref.clone())
            }
            ResolvedLiteral::FloatLiteral(_float_value, float_type) => {
                ResolvedType::Float(float_type.clone())
            }
            ResolvedLiteral::IntLiteral(_int_value, int_type) => {
                ResolvedType::Int(int_type.clone())
            }
            ResolvedLiteral::StringLiteral(_string_value, string_type) => {
                ResolvedType::String(string_type.clone())
            }
            ResolvedLiteral::UnitLiteral(unit_literal) => ResolvedType::Unit(unit_literal.clone()),
            ResolvedLiteral::EnumVariantLiteral(variant_ref, _data) => {
                ResolvedType::Enum(variant_ref.owner.clone())
            }
            ResolvedLiteral::TupleLiteral(tuple_type_ref, _data) => {
                ResolvedType::Tuple(tuple_type_ref.clone())
            }
            ResolvedLiteral::Array(array_type_ref, _data) => {
                ResolvedType::Array(array_type_ref.clone())
            }
        },
    };

    trace!(resolution_expression=%resolution_expression, "resolution first");
    //    trace!(resolution_expression=?resolution_expression, "resolution");

    resolution_expression
}

#[derive(Debug)]
pub enum ResolveError {
    DependencyError(DependencyError),
    CanNotFindModule(ModulePath),
    UnknownStructTypeReference(QualifiedTypeIdentifier),
    UnknownLocalStructTypeReference(LocalTypeIdentifier),
    DuplicateFieldName(IdentifierName),
    Unknown(String),
    Pest(pest::error::Error<Rule>), // Should not have dependency towards `pest` and Parser.
    UnknownImplTargetTypeReference(LocalTypeIdentifier),
    WrongFieldCountInStructInstantiation(ResolvedStructTypeRef, SeqMap<IdentifierName, Expression>),
    MissingFieldInStructInstantiation(IdentifierName, ResolvedStructTypeRef),
    ExpectedFunctionExpression(Expression),
    CouldNotFindMember(LocalIdentifier, Expression),
    UnknownVariable(Variable),
    NotAnArray(Expression),
    ArrayIndexMustBeInt(Expression),
    OverwriteVariableWithAnotherType(Variable),
    WrongNumberOfArguments(usize, usize),
    IncompatibleArguments(ResolvedType, ResolvedType),
    CanOnlyOverwriteVariableWithMut(Variable),
    OverwriteVariableNotAllowedHere(Variable),
    NotNamedStruct(Expression),
    UnknownEnumVariantType(QualifiedTypeIdentifier, LocalTypeIdentifier),
    WasNotStructType(Expression),
    UnknownStructField(LocalIdentifier),
    MustBeEnumType(Pattern),
    UnknownEnumVariantTypeInPattern(LocalTypeIdentifier),
    ExpectedEnumInPattern(LocalTypeIdentifier),
    WrongEnumVariantContainer(ResolvedEnumVariantTypeRef),
    VariableIsNotMutable(Variable),
    ArgumentIsNotMutable,
    WrongNumberOfTupleDeconstructVariables,
    UnknownTypeReference(LocalTypeIdentifier),
    SemanticError(SemanticError),
}

impl From<SemanticError> for ResolveError {
    fn from(value: SemanticError) -> Self {
        Self::SemanticError(value)
    }
}

impl From<pest::error::Error<Rule>> for ResolveError {
    fn from(value: pest::error::Error<Rule>) -> Self {
        Self::Pest(value)
    }
}

#[derive(Debug)]
pub struct BlockScope {
    variables: SeqMap<String, ResolvedVariableRef>,
}

impl Display for BlockScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", comma_seq(&self.variables))
    }
}

impl Default for BlockScope {
    fn default() -> Self {
        Self::new()
    }
}

impl BlockScope {
    #[must_use]
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

#[derive(Debug)]
pub struct ParseModule {
    pub ast_program: swamp_script_ast::Module,
}

impl ParseModule {
    pub fn declare_external_function(
        &mut self,
        name: String,
        parameters: Vec<Parameter>,
        return_type: Type,
    ) {
        self.ast_program.definitions.insert(
            0,
            Definition::ExternalFunctionDef(
                // TODO: Workaround to push external declarations so they come before internal functions
                LocalIdentifier {
                    node: Node {
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
                    text: name,
                },
                FunctionSignature {
                    params: parameters,
                    return_type,
                },
            ),
        );
    }
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
            .map(|local_type_identifier| local_type_identifier.as_str())
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
            Type::Any => ResolvedType::Any,
            Type::Int => ResolvedType::Int(self.parent.int_type.clone()),
            Type::Float => ResolvedType::Float(self.parent.float_type.clone()),
            Type::String => ResolvedType::String(self.parent.string_type.clone()),
            Type::Bool => ResolvedType::Bool(self.parent.bool_type.clone()),
            Type::Unit => ResolvedType::Unit(self.parent.unit_type.clone()),
            Type::Struct(ast_struct) => ResolvedType::Struct(self.find_struct_type(ast_struct)?),
            Type::Array(ast_type) => ResolvedType::Array(self.resolve_array_type(ast_type)?),
            Type::Tuple(types) => {
                ResolvedType::Tuple(ResolvedTupleType(self.resolve_types(&types)?).into())
            }
            Type::Enum(_) => todo!(),
            Type::Map(_, _) => todo!(),
            Type::TypeReference(ast_type_reference) => {
                self.find_type_reference(ast_type_reference)?
            }
        };

        Ok(resolved)
    }

    fn resolve_types(&mut self, types: &Vec<Type>) -> Result<Vec<ResolvedType>, ResolveError> {
        let mut resolved_types = Vec::new();
        for some_type in types.iter() {
            resolved_types.push(self.resolve_type(some_type)?);
        }
        Ok(resolved_types)
    }

    #[must_use]
    pub fn find_module(&self, path: &ModulePath) -> Option<&ResolvedModuleRef> {
        self.parent.modules.get(path)
    }

    fn find_type_reference(
        &self,
        type_name_to_find: &QualifiedTypeIdentifier,
    ) -> Result<ResolvedType, ResolveError> {
        let namespace = self.get_namespace(type_name_to_find)?;
        let type_ident = &type_name_to_find.name;

        let resolved_type = if let Some(found) = namespace.get_struct(type_ident) {
            ResolvedType::Struct(found.clone())
        } else if let Some(found) = namespace.get_enum(type_ident) {
            ResolvedType::Enum(found.clone())
        } else {
            Err(ResolveError::UnknownTypeReference(type_ident.clone()))?
        };

        Ok(resolved_type)
    }
    pub fn find_struct_type(
        &self,
        type_name: &QualifiedTypeIdentifier,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        let namespace = self.get_namespace(type_name)?;

        namespace.get_struct(&type_name.name).map_or_else(
            || Err(ResolveError::UnknownStructTypeReference(type_name.clone())),
            |found| Ok(found.clone()),
        )
    }

    pub fn find_struct_type_local_mut(
        &mut self,
        type_name: &LocalTypeIdentifier,
    ) -> Result<&ResolvedStructTypeRef, ResolveError> {
        self.current_module
            .namespace
            .get_struct(type_name)
            .map_or_else(
                || {
                    Err(ResolveError::UnknownLocalStructTypeReference(
                        type_name.clone(),
                    ))
                },
                |found| Ok(found),
            )
    }

    fn new_resolver(&mut self) -> Resolver {
        Resolver::new(self.parent, self.current_module)
    }

    fn resolve_member_item(
        &mut self,
        member_item: &ImplItem,
        attached_to_type: &LocalTypeIdentifier,
        local_ident: &IdentifierName,
    ) -> Result<(), ResolveError> {
        match member_item {
            ImplItem::Member(impl_member) => {
                let resolved_return = self.resolve_type(&impl_member.return_type)?;

                let found_struct = self.find_struct_type_local_mut(attached_to_type)?.clone();
                {
                    let self_is_mutable = impl_member.self_param.is_mutable;

                    let self_parameter = ResolvedParameter {
                        name: "self".to_string(),
                        resolved_type: ResolvedType::Struct(found_struct.clone()),
                        ast_parameter: Parameter {
                            variable: Variable {
                                name: "self".to_string(),
                                is_mutable: false,
                            },
                            param_type: Type::Int,
                            is_mutable: self_is_mutable,
                        },
                        is_mutable: self_is_mutable,
                    };

                    let mut resolved_parameters = Vec::new();
                    resolved_parameters.push(self_parameter);
                    resolved_parameters.extend(self.resolve_parameters(&impl_member.params)?);

                    for param in &resolved_parameters {
                        let param_var = Variable {
                            name: param.name.clone(),
                            is_mutable: param.is_mutable,
                        };
                        self.set_variable_with_type(&param_var, &param.resolved_type)?;
                    }

                    let body = self.resolve_statements(&impl_member.body)?;

                    let resolved_impl_member = ResolvedImplMember {
                        parameters: resolved_parameters,
                        return_type: resolved_return.clone(),
                        ast_member: impl_member.clone(),
                        struct_ref: found_struct.clone(),
                        body,
                    };
                    let resolved_impl_member_ref = Rc::new(resolved_impl_member);
                    {
                        found_struct
                            .borrow_mut()
                            .impl_members
                            .insert(
                                IdentifierName(local_ident.0.clone()),
                                resolved_impl_member_ref,
                            )
                            .expect("should insert impl_member");
                    }
                }
            }

            ImplItem::Function(function_data) => {
                let resolved_parameters =
                    self.resolve_parameters(&function_data.signature.params)?;
                let resolved_return = self.resolve_type(&function_data.signature.return_type)?;
                let resolved_statements = self.resolve_statements(&function_data.body)?;

                let member_function = ResolvedFunctionData {
                    parameters: resolved_parameters,
                    return_type: resolved_return.clone(),
                    statements: resolved_statements,
                };

                let member_function_ref = Rc::new(member_function);
                {
                    let found_struct = self.find_struct_type_local_mut(&attached_to_type)?;
                    found_struct
                        .borrow_mut()
                        .impl_functions
                        .insert(IdentifierName(local_ident.0.clone()), member_function_ref)
                        .expect("should insert impl_member");
                }
            }
        }

        Ok(())
    }

    fn resolve_impl_definition(
        &mut self,
        attached_to_type: &LocalTypeIdentifier,
        functions: &SeqMap<IdentifierName, ImplItem>,
    ) -> Result<(), ResolveError> {
        // Can only attach to earlier type in same module
        for (local_ident, member_item) in functions {
            let mut inner_resolver = self.new_resolver();
            inner_resolver.resolve_member_item(member_item, attached_to_type, local_ident)?;
        }

        Ok(())
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

        let parent_number = self.parent.allocate_number();

        let parent_ref = self
            .current_module
            .namespace
            .create_enum_type(enum_type_name, parent_number)?;

        for (name, ast_enum_variant) in ast_variants {
            let container = match ast_enum_variant {
                EnumVariant::Simple => ResolvedEnumVariantContainerType::Nothing,
                EnumVariant::Tuple(types) => {
                    let mut vec = Vec::new();
                    for tuple_type in types {
                        let resolved_type = self.resolve_type(tuple_type)?;
                        vec.push(resolved_type)
                    }

                    let number = self.parent.allocate_number();

                    let common = CommonEnumVariantType {
                        number,
                        module_path: ModulePath(vec![]), // TODO:
                        variant_name: name.clone(),
                        enum_ref: parent_ref.clone(),
                    };

                    let resolved_tuple_type = ResolvedEnumVariantTupleType {
                        common,
                        fields_in_order: vec,
                    };
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

                    let number = self.parent.allocate_number();

                    let common = CommonEnumVariantType {
                        number,
                        module_path: ModulePath(vec![]), // TODO:
                        variant_name: name.clone(),
                        enum_ref: parent_ref.clone(),
                    };

                    let enum_variant_struct_type = ResolvedEnumVariantStructType {
                        common,
                        fields,
                        ast_struct: ast_struct_fields.clone(),
                    };
                    let enum_variant_struct_type_ref = Rc::new(enum_variant_struct_type);

                    ResolvedEnumVariantContainerType::Struct(enum_variant_struct_type_ref)
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
        let parameters = self.resolve_parameters(&function_data.signature.params)?;
        let resolved_return_type = self.resolve_type(&function_data.signature.return_type)?;

        for param in &parameters {
            debug!("setting variable {:?}", param);
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
        let resolved_internal_function_def_ref = self
            .current_module
            .namespace
            .add_internal_function(identifier.text.clone(), function_def)?;

        Ok(resolved_internal_function_def_ref)
    }

    fn resolve_external_definition(
        &mut self,
        identifier: &LocalIdentifier,
        signature: &FunctionSignature,
    ) -> Result<ResolvedExternalFunctionDefinitionRef, ResolveError> {
        let parameters = self.resolve_parameters(&signature.params)?;
        let resolved_return_type = self.resolve_type(&signature.return_type)?;

        self.parent.external_function_number += 1;

        let function_def = ResolvedExternalFunctionDefinition {
            name: identifier.clone(),
            parameters,
            resolved_return_type,
            id: self.parent.external_function_number,
        };

        // Move ownership to module namespace
        let resolved_external_function_def_ref = self
            .current_module
            .namespace
            .add_external_function_declaration(identifier.text.clone(), function_def)?;

        Ok(resolved_external_function_def_ref)
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
            Definition::InternalFunctionDef(identifier, function_info) => {
                self.resolve_function_definition(identifier, function_info)?;
            }
            Definition::ExternalFunctionDef(identifier, function_info) => {
                self.resolve_external_definition(identifier, function_info)?;
            }
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
            Pattern::VariableAssignment(variable) => self.assign_variable(&variable, expr)?,
            Pattern::Tuple(_) => todo!(),         // TODO: PBJ
            Pattern::Struct(_) => todo!(),        // TODO: PBJ
            Pattern::Literal(_) => todo!(),       // TODO: PBJ
            Pattern::EnumTuple(_, _) => todo!(),  // TODO: PBJ
            Pattern::EnumStruct(_, _) => todo!(), // TODO: PBJ
            Pattern::EnumSimple(_) => todo!(),    // TODO: PBJ
            Pattern::Wildcard => todo!(),         // TODO: PBJ
        };

        Ok(resolved_let)
    }

    fn resolve_statement(
        &mut self,
        statement: &Statement,
    ) -> Result<ResolvedStatement, ResolveError> {
        let converted = match statement {
            Statement::Let(pattern, expr) => self.resolve_let_statement(pattern, expr)?,
            Statement::ForLoop(pattern, expression, statements) => {
                let resolved_iterator = self.resolve_iterator(expression)?;

                self.push_block_scope();
                let pattern = self.resolve_pattern(pattern, &resolved_iterator.item_type)?;
                let resolved_statements = self.resolve_statements(statements)?;
                self.pop_block_scope();

                ResolvedStatement::ForLoop(pattern, resolved_iterator, resolved_statements)
            }
            Statement::WhileLoop(expression, statements) => {
                let condition = self.resolve_bool_expression(expression)?;
                self.push_block_scope();
                let resolved_statements = self.resolve_statements(statements)?;
                self.pop_block_scope();

                ResolvedStatement::WhileLoop(condition, resolved_statements)
            }
            Statement::Return(expression) => {
                ResolvedStatement::Return(self.resolve_expression(expression)?)
            }
            Statement::Break => ResolvedStatement::Break,
            Statement::Continue => ResolvedStatement::Continue,
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
            });
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
            Expression::MutRef(variable) => self.resolve_mut_ref(variable)?,
            Expression::ArrayAccess(array_expression, lookup) => ResolvedExpression::ArrayAccess(
                self.resolve_array_access(array_expression, lookup)?,
            ),

            // Assignments
            Expression::VariableAssignment(variable_expression, source_expression) => {
                ResolvedExpression::VariableAssignment(
                    self.resolve_variable_assignment(variable_expression, source_expression)?,
                )
            }
            Expression::ArrayAssignment(target_expr, index_expr, source_expr) => {
                let resolved_target_expr = self.resolve_into_mut_array(target_expr)?;
                let resolved_index_expr = self.resolve_into_index(index_expr)?;
                let resolved_source_expr = self.resolve_expression(source_expr)?;
                ResolvedExpression::ArrayAssignment(
                    resolved_target_expr,
                    resolved_index_expr,
                    Box::from(resolved_source_expr),
                )
            }
            Expression::FieldAssignment(ast_struct_expr, ast_field_name, ast_expression) => {
                // Box<Expression>, LocalIdentifier, Box<Expression>
                let source_expression = self.resolve_expression(ast_expression)?;

                let mut_struct_field = self
                    .resolve_into_struct_field_mut_ref(ast_struct_expr, ast_field_name.clone())?;

                ResolvedExpression::StructFieldAssignment(
                    mut_struct_field.clone(),
                    Box::from(source_expression),
                )
            }

            // Operator
            Expression::BinaryOp(resolved_a, operator, resolved_b) => ResolvedExpression::BinaryOp(
                self.resolve_binary_op(resolved_a, operator, resolved_b)?,
            ),
            Expression::UnaryOp(operator, expression) => {
                ResolvedExpression::UnaryOp(self.resolve_unary_op(operator, expression)?)
            }

            // Calls
            Expression::FunctionCall(function_expression, parameter_expressions) => {
                self.resolve_function_call(function_expression, parameter_expressions)?
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
                self.parent.string_type.clone(),
                self.resolve_interpolated_string(string_parts)?,
            ),

            // Creation
            Expression::StructInstantiation(struct_identifier, fields) => {
                ResolvedExpression::StructInstantiation(
                    self.resolve_struct_instantiation(struct_identifier, fields)?,
                )
            }
            Expression::ExclusiveRange(min_value, max_value) => {
                let min_expression = self.resolve_expression(min_value)?;
                let max_expression = self.resolve_expression(max_value)?;
                ResolvedExpression::ExclusiveRange(
                    self.parent.exclusive_range_type.clone(),
                    Box::from(min_expression),
                    Box::from(max_expression),
                )
            }

            Expression::Literal(literal) => {
                swamp_script_semantic::ResolvedExpression::Literal(match literal {
                    Literal::Int(value) => {
                        ResolvedLiteral::IntLiteral(*value, self.parent.int_type.clone())
                    }
                    Literal::Float(value) => {
                        ResolvedLiteral::FloatLiteral(*value, self.parent.float_type.clone())
                    }
                    Literal::String(value) => ResolvedLiteral::StringLiteral(
                        value.clone(),
                        self.parent.string_type.clone(),
                    ),
                    Literal::Unit => ResolvedLiteral::UnitLiteral(self.parent.unit_type.clone()),
                    Literal::Bool(value) => {
                        ResolvedLiteral::BoolLiteral(*value, self.parent.bool_type.clone())
                    }

                    Literal::EnumVariant(qualified_type_identifier, variant_name, data) => self
                        .resolve_enum_variant_literal(
                            qualified_type_identifier,
                            variant_name,
                            data,
                        )?,

                    Literal::Tuple(expressions) => {
                        let mut resolved_expressions = Vec::new();
                        for expression in expressions {
                            resolved_expressions.push(self.resolve_expression(expression)?);
                        }
                        let mut resolved_types = Vec::new();
                        for resolved_expression in &resolved_expressions {
                            resolved_types.push(resolution(&resolved_expression));
                        }

                        let tuple_type = ResolvedTupleType::new(resolved_types);
                        let tuple_type_ref = Rc::new(tuple_type);

                        ResolvedLiteral::TupleLiteral(tuple_type_ref, resolved_expressions)
                    }

                    Literal::Array(items) => {
                        let (array_type_ref, expressions) =
                            self.resolve_array_type_helper(items)?;

                        ResolvedLiteral::Array(array_type_ref, expressions)
                    }

                    Literal::Map(_) => todo!(),
                })
            }

            // Comparison
            Expression::IfElse(condition, consequence, alternate) => {
                let resolved_condition = self.resolve_bool_expression(condition)?;
                let resolved_consequence = self.resolve_expression(&consequence)?;
                let resolved_alternate = self.resolve_expression(&alternate)?;

                ResolvedExpression::IfElse(
                    Box::from(resolved_condition),
                    Box::from(resolved_consequence),
                    Box::from(resolved_alternate),
                )
            }
            Expression::Match(expression, arms) => {
                ResolvedExpression::Match(self.resolve_match(expression, arms)?)
            }
        };

        Ok(expression)
    }

    fn resolve_into_struct_field_ref(
        &mut self,
        struct_expression: &Expression,
        name: LocalIdentifier,
    ) -> Result<ResolvedStructTypeFieldRef, ResolveError> {
        let resolved_expr = self.resolve_expression(struct_expression)?;
        let resolved_type = resolution(&resolved_expr);
        let ResolvedType::Struct(struct_ref) = &resolved_type else {
            return Err(ResolveError::WasNotStructType(struct_expression.clone()));
        };

        let borrowed_struct = struct_ref.borrow();

        if let Some(field_index) = borrowed_struct
            .fields
            .get_index(&IdentifierName(name.text.clone()))
        {
            let field_resolved_type = borrowed_struct
                .fields
                .get(&IdentifierName(name.text.clone()))
                .expect("checked earlier");

            let field = ResolvedStructTypeField {
                struct_type_ref: struct_ref.clone(),
                index: field_index,
                resolved_type: field_resolved_type.clone(),
                field_name: name.clone(),
                struct_expression: Box::from(resolved_expr),
            };

            let field_ref = Rc::new(field);

            Ok(field_ref)
        } else {
            Err(ResolveError::UnknownStructField(name.clone()))
        }
    }

    fn resolve_into_mut_array(
        &mut self,
        array_expr: &Expression,
    ) -> Result<ResolvedMutArrayRef, ResolveError> {
        let resolved_expr = self.resolve_expression(array_expr)?;
        let resolved_type = resolution(&resolved_expr);
        let array_type_ref = match resolved_type {
            ResolvedType::Array(array_type_ref) => array_type_ref,
            _ => Err(ResolveError::NotAnArray(array_expr.clone()))?,
        };

        let mut_array = ResolvedMutArray {
            expression: resolved_expr,
            array_type_ref,
        };

        Ok(Rc::new(mut_array))
    }

    fn resolve_into_index(
        &mut self,
        index_expr: &Expression,
    ) -> Result<ResolvedIndexTypeRef, ResolveError> {
        let resolved_expr = self.resolve_expression(index_expr)?;

        let resolved_type = resolution(&resolved_expr);

        let index_type = ResolvedIndexType {
            expression: resolved_expr,
            resolved_type: resolved_type.clone(),
        };

        Ok(Rc::new(index_type))
    }

    fn resolve_into_struct_field_mut_ref(
        &mut self,
        expression: &Expression,
        field_identifier: LocalIdentifier,
    ) -> Result<ResolvedMutStructTypeFieldRef, ResolveError> {
        Ok(Rc::new(ResolvedMutStructTypeField {
            inner: self.resolve_into_struct_field_ref(expression, field_identifier)?,
        }))
    }

    #[allow(unused)]
    fn resolve_into_named_struct_ref(
        &mut self,
        struct_expression: &Expression,
    ) -> Result<(ResolvedStructTypeRef, ResolvedExpression), ResolveError> {
        let resolved = self.resolve_expression(struct_expression)?;

        let resolved_type = resolution(&resolved);
        match resolved_type {
            ResolvedType::Struct(named_struct) => Ok((named_struct, resolved)),
            _ => Err(ResolveError::NotNamedStruct(struct_expression.clone())),
        }
    }

    fn resolve_struct_instantiation(
        &mut self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
        ast_fields: &SeqMap<IdentifierName, Expression>,
    ) -> Result<ResolvedStructInstantiation, ResolveError> {
        let struct_to_instantiate = self.find_struct_type(qualified_type_identifier)?;

        if ast_fields.len() != struct_to_instantiate.borrow().fields.len() {
            return Err(ResolveError::WrongFieldCountInStructInstantiation(
                struct_to_instantiate,
                ast_fields.clone(),
            ));
        }

        let mut expressions_in_order = Vec::new();

        for (field_name, _field) in &struct_to_instantiate.borrow().fields {
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
        expression: &Expression,
    ) -> Result<ResolvedBooleanExpression, ResolveError> {
        let resolved_expression = self.resolve_expression(expression)?;

        Ok(ResolvedBooleanExpression {
            ast: expression.clone(),
            expression: resolved_expression,
        })
    }

    fn resolve_pattern_variable(
        &mut self,
        variable: &Variable,
        expression_type: &ResolvedType,
    ) -> Result<ResolvedPattern, ResolveError> {
        self.set_variable_with_type(variable, expression_type)
            .expect("could not set variable_with_type in pattern");
        let variable_ref = self.find_variable(variable)?;
        Ok(ResolvedPattern::VariableAssignment(variable_ref))
    }

    fn find_variant_in_pattern(
        &mut self,
        expression_type: &ResolvedType,
        ast_name: &LocalTypeIdentifier,
    ) -> Result<ResolvedEnumVariantTypeRef, ResolveError> {
        let enum_type_ref = match expression_type {
            ResolvedType::Enum(enum_type_ref) => enum_type_ref,
            _ => Err(ResolveError::ExpectedEnumInPattern(ast_name.clone()))?,
        };

        self.current_module
            .namespace
            .get_enum_variant_type(enum_type_ref.name(), &ast_name)
            .map_or_else(
                || {
                    Err(ResolveError::UnknownEnumVariantTypeInPattern(
                        ast_name.clone(),
                    ))
                },
                |found| Ok(found.clone()),
            )
    }

    fn resolve_pattern(
        &mut self,
        ast_pattern: &Pattern,
        expression_type: &ResolvedType,
    ) -> Result<ResolvedPattern, ResolveError> {
        let resolved_pattern = match ast_pattern {
            Pattern::VariableAssignment(variable) => {
                self.resolve_pattern_variable(variable, expression_type)?
            }
            Pattern::Tuple(_) => todo!(),  // TODO: PBJ
            Pattern::Struct(_) => todo!(), // TODO: PBJ
            Pattern::Literal(ast_literal) => self.resolve_pattern_literal(ast_literal),
            Pattern::EnumTuple(variant_name, variables_in_order) => {
                let enum_variant_type_ref =
                    self.find_variant_in_pattern(expression_type, variant_name)?;

                let tuple_container = match &enum_variant_type_ref.data {
                    ResolvedEnumVariantContainerType::Tuple(tuple_type_ref) => tuple_type_ref,

                    _ => Err(ResolveError::WrongEnumVariantContainer(
                        enum_variant_type_ref.clone(),
                    ))?,
                };

                if variables_in_order.len() != tuple_container.fields_in_order.len() {
                    return Err(ResolveError::WrongNumberOfTupleDeconstructVariables);
                }

                let mut field_refs = Vec::new();

                for (index, (expected_type, field_name)) in tuple_container
                    .fields_in_order
                    .iter()
                    .zip(variables_in_order.iter())
                    .enumerate()
                {
                    field_refs.push(ResolvedEnumVariantTupleFieldType {
                        name: field_name.clone(),
                        field_index: index,
                        enum_variant: enum_variant_type_ref.clone(),
                        resolved_type: expected_type.clone(),
                    });
                }
                ResolvedPattern::EnumTuple(tuple_container.clone(), field_refs)
            }
            Pattern::EnumStruct(variant_name, variables) => {
                let enum_variant_type_ref =
                    self.find_variant_in_pattern(expression_type, variant_name)?;

                let struct_container = match &enum_variant_type_ref.data {
                    ResolvedEnumVariantContainerType::Struct(struct_data) => struct_data,

                    _ => Err(ResolveError::WrongEnumVariantContainer(
                        enum_variant_type_ref.clone(),
                    ))?,
                };

                let mut field_refs = Vec::new();

                for field_name in variables {
                    if let Some(found_index) = struct_container
                        .fields
                        .get_index(&IdentifierName(field_name.text.clone()))
                    {
                        let resolved_field_type = struct_container
                            .fields
                            .get(&IdentifierName(field_name.text.clone()))
                            .expect("could not get struct enum field");
                        field_refs.push(ResolvedEnumVariantStructFieldType {
                            name: field_name.clone(),
                            field_index: found_index,
                            enum_variant: enum_variant_type_ref.clone(),
                            resolved_type: resolved_field_type.clone(),
                        });
                    } else {
                        return Err(ResolveError::UnknownStructField(field_name.clone()));
                    }
                }

                ResolvedPattern::EnumStruct(enum_variant_type_ref.clone(), field_refs)
            }
            Pattern::EnumSimple(ast_name) => {
                let enum_variant_type_ref =
                    self.find_variant_in_pattern(expression_type, ast_name)?;
                ResolvedPattern::EnumSimple(enum_variant_type_ref.clone())
            }
            Pattern::Wildcard => ResolvedPattern::Wildcard,
        };

        Ok(resolved_pattern)
    }

    fn resolve_iterator(
        &mut self,
        expression: &Expression,
    ) -> Result<ResolvedIterator, ResolveError> {
        let resolved_expression = self.resolve_expression(expression)?;
        let resolved_type = resolution(&resolved_expression);
        let item_type = match resolved_type {
            ResolvedType::Array(array_type) => array_type.item_type.clone(),
            ResolvedType::ExclusiveRange(_) => resolved_type.clone(),
            _ => todo!(),
        };

        Ok(ResolvedIterator {
            item_type,
            resolved_expression,
        })
    }

    fn resolve_and_verify_parameters(
        &mut self,
        fn_parameters: &[ResolvedParameter],
        arguments: &[Expression],
    ) -> Result<Vec<ResolvedExpression>, ResolveError> {
        let resolved_arguments = self.resolve_expressions(arguments)?;
        if resolved_arguments.len() != fn_parameters.len() {
            return Err(ResolveError::WrongNumberOfArguments(
                resolved_arguments.len(),
                fn_parameters.len(),
            ));
        }

        for (parameter_index, resolved_argument_expression) in resolved_arguments.iter().enumerate()
        {
            let parameter = &fn_parameters[parameter_index];
            let parameter_type = &parameter.resolved_type;
            let argument_type = resolution(resolved_argument_expression);
            if !same_type(&argument_type, parameter_type) {
                error!("{argument_type}: {resolved_argument_expression}");
                return Err(ResolveError::IncompatibleArguments(
                    argument_type.clone(),
                    parameter_type.clone(),
                ));
            }

            if parameter.is_mutable
                && !matches!(resolved_argument_expression, ResolvedExpression::MutRef(_))
            {
                return Err(ResolveError::ArgumentIsNotMutable);
            }
        }

        Ok(resolved_arguments)
    }

    fn resolve_internal_function_call(
        &mut self,
        fn_def: &ResolvedInternalFunctionDefinitionRef,
        function_expr: ResolvedExpression,
        arguments: &[Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_arguments =
            self.resolve_and_verify_parameters(&fn_def.parameters, arguments)?;

        Ok(ResolvedExpression::FunctionInternalCall(
            ResolvedInternalFunctionCall {
                arguments: resolved_arguments,
                function_definition: fn_def.clone(),
                function_expression: Box::from(function_expr),
            },
        ))
    }

    fn resolve_external_function_call(
        &mut self,
        fn_def: &ResolvedExternalFunctionDefinitionRef,
        function_expr: ResolvedExpression,
        arguments: &[Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_arguments =
            self.resolve_and_verify_parameters(&fn_def.parameters, arguments)?;

        Ok(ResolvedExpression::FunctionExternalCall(
            ResolvedExternalFunctionCall {
                arguments: resolved_arguments,
                function_definition: fn_def.clone(),
                function_expression: Box::from(function_expr),
            },
        ))
    }

    fn resolve_function_call(
        &mut self,
        function_expression: &Expression,
        arguments: &[Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        let function_expr = self.resolve_expression(function_expression)?;
        let resolution_type = resolution(&function_expr);

        match resolution_type {
            ResolvedType::FunctionInternal(ref function_call) => {
                self.resolve_internal_function_call(&function_call, function_expr, arguments)
            }
            ResolvedType::FunctionExternal(ref function_call) => {
                self.resolve_external_function_call(&function_call, function_expr, arguments)
            }
            _ => Err(ResolveError::ExpectedFunctionExpression(
                function_expression.clone(),
            )),
        }
    }

    fn resolve_member_call(
        &mut self,
        ast_member_expression: &Expression,
        ast_member_function_name: &LocalIdentifier,
        ast_arguments: &Vec<Expression>,
    ) -> Result<ResolvedMemberCall, ResolveError> {
        let (resolved_struct_type_ref, resolved_expression) =
            self.resolve_into_named_struct_ref(ast_member_expression)?;

        let x = if let Some(impl_member) = resolved_struct_type_ref
            .clone()
            .borrow()
            .impl_members
            .get(&IdentifierName(ast_member_function_name.text.clone()))
        {
            let resolved_arguments = self.resolve_expressions(ast_arguments)?;

            Ok(ResolvedMemberCall {
                impl_member: impl_member.clone(),
                arguments: resolved_arguments,
                self_expression: Box::new(resolved_expression),
                struct_type_ref: resolved_struct_type_ref,
                self_is_mutable: impl_member.ast_member.self_param.is_mutable,
            })
        } else {
            Err(ResolveError::CouldNotFindMember(
                ast_member_function_name.clone(),
                ast_member_expression.clone(),
            ))
        };
        x
    }

    fn resolve_binary_op(
        &mut self,
        ast_left: &Box<Expression>,
        ast_op: &BinaryOperator,
        ast_right: &Box<Expression>,
    ) -> Result<ResolvedBinaryOperator, ResolveError> {
        let left = self.resolve_expression(ast_left)?;
        let right = self.resolve_expression(ast_right)?;
        let resolved_type = resolution(&left);

        Ok(ResolvedBinaryOperator {
            left: Box::new(left),
            right: Box::new(right),
            ast_operator_type: ast_op.clone(),
            resolved_type,
        })
    }

    fn resolve_unary_op(
        &mut self,
        ast_op: &UnaryOperator,
        ast_left: &Box<Expression>,
    ) -> Result<ResolvedUnaryOperator, ResolveError> {
        let left = self.resolve_expression(ast_left)?;
        let resolved_type = resolution(&left);

        Ok(ResolvedUnaryOperator {
            left: Box::new(left),
            ast_operator_type: ast_op.clone(),
            resolved_type,
        })
    }

    fn resolve_expressions(
        &mut self,
        ast_expressions: &[Expression],
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
        self.try_find_variable(variable).map_or_else(
            || {
                self.current_module
                    .namespace
                    .get_function(&variable.name)
                    .map_or_else(
                        || {
                            self.current_module
                                .namespace
                                .get_external_function_declaration(&variable.name)
                                .map_or_else(
                                    || Err(ResolveError::UnknownVariable(variable.clone())),
                                    |external_function_ref| {
                                        Ok(ResolvedExpression::ExternalFunctionAccess(
                                            external_function_ref.clone(),
                                        ))
                                    },
                                )
                        },
                        |function_ref| {
                            Ok(ResolvedExpression::InternalFunctionAccess(
                                function_ref.clone(),
                            ))
                        },
                    )
            },
            |variable_ref| Ok(ResolvedExpression::VariableAccess(variable_ref)),
        )
    }

    fn resolve_mut_ref(
        &self,
        variable: &MutVariableRef,
    ) -> Result<ResolvedExpression, ResolveError> {
        let var = self.find_variable(&variable.0)?;
        if !var.is_mutable() {
            Err(ResolveError::VariableIsNotMutable(variable.0.clone()))?;
        }
        let mut_var = ResolvedMutVariable { variable_ref: var };
        Ok(ResolvedExpression::MutRef(Rc::new(mut_var)))
    }

    fn find_variable(&self, variable: &Variable) -> Result<ResolvedVariableRef, ResolveError> {
        self.try_find_variable(variable)
            .map_or_else(|| Err(ResolveError::UnknownVariable(variable.clone())), Ok)
    }

    fn try_find_variable(&self, variable: &Variable) -> Option<ResolvedVariableRef> {
        trace!("trying to find variable {variable:?}");
        // Look through scopes until we hit a Function scope
        for scope in self.block_scope_stack.iter().rev() {
            trace!("...checking in scope {scope}");
            if let Some(value) = scope.variables.get(&variable.name) {
                return Some(value.clone());
            }
        }

        None
    }

    fn try_find_local_variable(&self, variable: &Variable) -> Option<&ResolvedVariableRef> {
        trace!("trying to find local {variable:?}");

        let current_scope = self
            .block_scope_stack
            .iter()
            .last()
            .expect("no scope stack available");

        current_scope.variables.get(&variable.name)
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
        let ResolvedType::Array(item_type) = &array_resolution else {
            return Err(ResolveError::NotAnArray(array_expression.clone()));
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
            array_expression: resolved_array_expression,
        };

        Ok(Rc::new(array_item))
    }

    fn resolve_variable_assignment(
        &mut self,
        ast_variable: &Variable,
        ast_expression: &Box<Expression>,
    ) -> Result<ResolvedVariableAssignment, ResolveError> {
        let converted_expression = self.resolve_expression(ast_expression)?;
        let expression_type = resolution(&converted_expression);
        let (variable_ref, _overwritten) =
            self.set_or_overwrite_variable_with_type(ast_variable, &expression_type)?;

        Ok(ResolvedVariableAssignment {
            variable_ref,
            expression: Box::from(converted_expression),
        })
    }

    fn resolve_array_type_helper(
        &mut self,
        items: &Vec<Expression>,
    ) -> Result<(ResolvedArrayTypeRef, Vec<ResolvedExpression>), ResolveError> {
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

        Ok((array_type_ref, expressions))
    }

    fn push_block_scope(&mut self) {
        info!("pushing scope stack");

        self.block_scope_stack.push(BlockScope {
            variables: Default::default(),
        });
    }

    fn pop_block_scope(&mut self) {
        info!("popping scope stack");
        self.block_scope_stack.pop();
    }

    fn assign_variable(
        &mut self,
        variable: &Variable,
        expression: ResolvedExpression,
    ) -> Result<ResolvedStatement, ResolveError> {
        let variable_type_ref = resolution(&expression);
        let (variable_ref, was_overwrite) =
            self.set_or_overwrite_variable_with_type(variable, &variable_type_ref)?;
        let statement = if was_overwrite {
            ResolvedStatement::SetVar(variable_ref, expression)
        } else {
            ResolvedStatement::LetVar(variable_ref, expression)
        };
        Ok(statement)
    }

    fn set_or_overwrite_variable_with_type(
        &mut self,
        variable: &Variable,
        variable_type_ref: &ResolvedType,
    ) -> Result<(ResolvedVariableRef, bool), ResolveError> {
        if let Some(existing_variable) = self.try_find_variable(&variable) {
            if !same_type(&existing_variable.resolved_type, variable_type_ref) {
                return Err(ResolveError::OverwriteVariableWithAnotherType(
                    variable.clone(),
                ));
            }
            if !existing_variable.ast_variable.is_mutable {
                return Err(ResolveError::CanOnlyOverwriteVariableWithMut(
                    variable.clone(),
                ));
            }

            return Ok((existing_variable.clone(), true));
        }
        let scope_index = self.block_scope_stack.len() - 1;

        let variables = &mut self
            .block_scope_stack
            .last_mut()
            .expect("block scope should have at least one scope")
            .variables;

        let resolved_variable = ResolvedVariable {
            resolved_type: variable_type_ref.clone(),
            ast_variable: variable.clone(),
            scope_index,
            variable_index: variables.len(),
        };

        let variable_ref = Rc::new(resolved_variable);

        {
            variables
                .insert(variable.name.clone(), variable_ref.clone())
                .expect("should have checked earlier for variable");
        }

        Ok((variable_ref, false))
    }

    fn set_variable_with_type(
        &mut self,
        variable: &Variable,
        variable_type_ref: &ResolvedType,
    ) -> Result<ResolvedVariableRef, ResolveError> {
        info!("VAR trying to set {variable:?}");

        if let Some(_existing_variable) = self.try_find_local_variable(variable) {
            return Err(ResolveError::OverwriteVariableNotAllowedHere(
                variable.clone(),
            ));
        }

        let scope_index = self.block_scope_stack.len() - 1;

        let variables = &mut self
            .block_scope_stack
            .last_mut()
            .expect("block scope should have at least one scope")
            .variables;

        let resolved_variable = ResolvedVariable {
            resolved_type: variable_type_ref.clone(),
            ast_variable: variable.clone(),
            scope_index,
            variable_index: variables.len(),
        };

        info!("VAR setting {resolved_variable:?}");

        let variable_ref = Rc::new(resolved_variable);

        variables
            .insert(variable.name.clone(), variable_ref.clone())
            .expect("should have checked earlier for variable");

        Ok(variable_ref)
    }

    fn resolve_enum_variant_ref(
        &self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
        variant_name: &LocalTypeIdentifier,
    ) -> Result<ResolvedEnumVariantTypeRef, ResolveError> {
        let namespace = self.get_namespace(qualified_type_identifier)?;

        namespace
            .get_enum_variant_type(&qualified_type_identifier.name, variant_name)
            .map_or_else(
                || {
                    Err(ResolveError::UnknownEnumVariantType(
                        qualified_type_identifier.clone(),
                        variant_name.clone(),
                    ))
                },
                |found| Ok(found.clone()),
            )
    }

    fn resolve_enum_variant_literal(
        &mut self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
        variant_name: &LocalTypeIdentifier,
        variant_data: &EnumLiteralData,
    ) -> Result<ResolvedLiteral, ResolveError> {
        let variant_ref = self.resolve_enum_variant_ref(qualified_type_identifier, variant_name)?;

        let resolved_data = match variant_data {
            EnumLiteralData::Nothing => ResolvedEnumLiteralData::Nothing,
            EnumLiteralData::Tuple(expressions) => {
                ResolvedEnumLiteralData::Tuple(self.resolve_expressions(expressions)?)
            }
            EnumLiteralData::Struct(anonym_struct) => {
                let values = &anonym_struct.values().cloned().collect::<Vec<_>>();
                let resolved_expressions = self.resolve_expressions(values)?;
                ResolvedEnumLiteralData::Struct(resolved_expressions)
            }
        };

        Ok(ResolvedLiteral::EnumVariantLiteral(
            variant_ref,
            resolved_data,
        ))
    }

    fn get_namespace(
        &self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
    ) -> Result<&ResolvedModuleNamespace, ResolveError> {
        if let Some(path) = &qualified_type_identifier.module_path {
            Ok(&self
                .find_module(path)
                .ok_or(ResolveError::CanNotFindModule(path.clone()))?
                .namespace)
        } else {
            Ok(&self.current_module.namespace)
        }
    }

    fn resolve_match(
        &mut self,
        expression: &Expression,
        arms: &Vec<MatchArm>,
    ) -> Result<ResolvedMatch, ResolveError> {
        let resolved_expression = self.resolve_expression(expression)?;
        let resolved_type = resolution(&resolved_expression);

        let mut resolved_arms = Vec::new();
        for arm in arms {
            info!("resolving arm {:?}", arm);
            let resolved_arm = self.resolve_arm(arm, &resolved_expression, &resolved_type)?;
            resolved_arms.push(resolved_arm);
        }

        Ok(ResolvedMatch {
            expression: Box::new(resolved_expression),
            arms: resolved_arms,
        })
    }

    fn resolve_arm(
        &mut self,
        arm: &MatchArm,
        _expression: &ResolvedExpression,
        expression_type: &ResolvedType,
    ) -> Result<ResolvedMatchArm, ResolveError> {
        self.push_block_scope();

        let resolved_pattern = self.resolve_pattern(&arm.pattern, expression_type)?;

        info!("resolving pattern: {:?}", resolved_pattern);

        match &resolved_pattern {
            ResolvedPattern::VariableAssignment(_variable_assignment) => {} // This is handled in self.resolve_pattern
            ResolvedPattern::Tuple(_) => {}
            ResolvedPattern::EnumTuple(ref _variant_ref, ref fields) => {
                for field in fields {
                    let var = Variable {
                        name: field.name.text.clone(),
                        is_mutable: false,
                    };
                    self.set_variable_with_type(&var, &field.resolved_type)?;
                }
            }
            ResolvedPattern::EnumStruct(ref _variant_ref, ref fields) => {
                for field in fields {
                    let var = Variable {
                        name: field.name.text.clone(),
                        is_mutable: false,
                    };
                    self.set_variable_with_type(&var, &field.resolved_type)?;
                }
            }
            ResolvedPattern::Wildcard | ResolvedPattern::Literal(_) => {} // Does not need to set any variables
            ResolvedPattern::Struct(_) => todo!(),
            ResolvedPattern::EnumSimple(_) => {} // Does not need to set any variables
        }

        let resolved_expression = self.resolve_expression(&arm.expression)?;
        let resolved_type = resolution(&resolved_expression);

        self.pop_block_scope();

        Ok(ResolvedMatchArm {
            ast_match_arm: arm.clone(),
            pattern: resolved_pattern,
            expression: Box::from(resolved_expression),
            expression_type: resolved_type,
        })
    }

    fn resolve_pattern_literal(&self, ast_literal: &Literal) -> ResolvedPattern {
        let resolved_literal = match ast_literal {
            Literal::Int(value) => {
                ResolvedLiteral::IntLiteral(*value, self.parent.int_type.clone())
            }
            Literal::Float(value) => {
                ResolvedLiteral::FloatLiteral(*value, self.parent.float_type.clone())
            }
            Literal::String(value) => {
                ResolvedLiteral::StringLiteral(value.clone(), self.parent.string_type.clone())
            }
            Literal::Bool(value) => {
                ResolvedLiteral::BoolLiteral(*value, self.parent.bool_type.clone())
            }
            Literal::EnumVariant(_, _, _) => todo!(),
            Literal::Tuple(_) => todo!(),
            Literal::Array(_) => todo!(),
            Literal::Map(_) => todo!(),
            Literal::Unit => todo!(),
        };

        ResolvedPattern::Literal(resolved_literal)
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

pub fn parse_dependant_modules_and_resolve(
    base_path: PathBuf,
    module_path: ModulePath,
    dependency_parser: &mut DependencyParser,
    resolved_program: &mut ResolvedProgram,
) -> Result<(), ResolveError> {
    debug!(current_directory=?get_current_dir().expect("failed to get current directory"), "current directory");
    let parse_root = ParseRoot::new(base_path);

    dependency_parser.parse_all_dependant_modules(parse_root, module_path)?;

    let module_paths_in_order = dependency_parser.get_analysis_order()?;

    for module_path in module_paths_in_order {
        if let Some(parse_module) = dependency_parser.get_parsed_module(&module_path) {
            let _resolved_module = resolve_module(resolved_program, module_path, parse_module)?;
        } else {
            panic!("not found module");
        }
    }

    Ok(())
}

pub fn resolve_module(
    resolved_program: &mut ResolvedProgram,
    module_path: ModulePath,
    module: &ParseModule,
) -> Result<ResolvedModuleRef, ResolveError> {
    let mut resolve_module = ResolvedModule::new(module_path.clone());

    for ast_def in module.ast_program.definitions() {
        let mut resolver = Resolver::new(resolved_program, &mut resolve_module);
        resolver.resolve_and_set_definition(ast_def)?;
    }

    {
        let mut resolver = Resolver::new(resolved_program, &mut resolve_module);
        resolve_module.statements = resolver.resolve_statements(module.ast_program.statements())?;
    }

    let module_ref = Rc::new(resolve_module);
    resolved_program
        .modules
        .add_module(module_path, module_ref.clone())?;

    Ok(module_ref)
}
