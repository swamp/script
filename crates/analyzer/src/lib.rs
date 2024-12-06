/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use seq_map::{SeqMap, SeqMapError};
use std::fmt::Display;
use std::rc::Rc;
use swamp_script_ast::prelude::*;
use swamp_script_ast::{CompoundOperator, Function, PostfixOperator};
use swamp_script_semantic::ns::{LocalTypeName, ResolvedModuleNamespace, SemanticError};
use swamp_script_semantic::{prelude::*, ResolvedStaticCallGeneric};
use swamp_script_semantic::{
    ResolvedDefinition, ResolvedEnumTypeRef, ResolvedFunction, ResolvedFunctionRef,
    ResolvedFunctionSignature, ResolvedMapTypeRef, ResolvedMutMap, ResolvedStaticCall,
};
use swamp_script_semantic::{ResolvedMapIndexLookup, ResolvedProgramTypes};
use swamp_script_semantic::{ResolvedMapType, ResolvedProgramState};
use swamp_script_semantic::{ResolvedModules, ResolvedPostfixOperator};
use tracing::{debug, error, info, trace};

pub fn signature(f: &ResolvedFunction) -> &ResolvedFunctionSignature {
    match f {
        ResolvedFunction::Internal(internal) => &internal.signature,
        ResolvedFunction::External(external) => &external.signature,
    }
}

pub fn unalias_type(resolved_type: ResolvedType) -> ResolvedType {
    match resolved_type {
        ResolvedType::Alias(_identifier, reference_to_type) => {
            unalias_type((*reference_to_type).clone())
        }

        _ => resolved_type,
    }
}

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
        ResolvedExpression::MapIndexAccess(map_item) => map_item.item_type.clone(),
        ResolvedExpression::VariableAssignment(variable_assignment) => {
            variable_assignment.variable_ref.resolved_type.clone()
        }

        ResolvedExpression::ArrayAssignment(_, _, _) => todo!(),
        ResolvedExpression::MapAssignment(_, _, _) => todo!(),
        ResolvedExpression::StructFieldAssignment(_, _) => todo!(),
        ResolvedExpression::BinaryOp(binary_op) => binary_op.resolved_type.clone(),
        ResolvedExpression::UnaryOp(unary_op) => unary_op.resolved_type.clone(),
        ResolvedExpression::PostfixOp(postfix_op) => postfix_op.resolved_type.clone(),

        ResolvedExpression::FunctionInternalCall(internal_fn_call) => internal_fn_call
            .function_definition
            .signature
            .return_type
            .clone(),
        ResolvedExpression::FunctionExternalCall(external_fn_call) => external_fn_call
            .function_definition
            .signature
            .return_type
            .clone(),
        ResolvedExpression::MutMemberCall(_, _) => todo!(),
        ResolvedExpression::MemberCall(member_call) => {
            signature(&member_call.function).return_type.clone()
        }
        ResolvedExpression::StaticCall(static_call) => {
            signature(&static_call.function).return_type.clone()
        },
        ResolvedExpression::StaticCallGeneric(static_call_generic) => {
            signature(&static_call_generic.function).return_type.clone()
        },

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
        ResolvedExpression::IfElse(_condition, consequence, _alternate) => resolution(consequence),
        ResolvedExpression::IfElseOnlyVariable { true_block, .. } => resolution(true_block),
        ResolvedExpression::IfElseAssignExpression { true_block, .. } => resolution(true_block),
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
            ResolvedLiteral::Map(map_type_ref, _data) => ResolvedType::Map(map_type_ref.clone()),
            ResolvedLiteral::NoneLiteral => ResolvedType::Any,
        },
        ResolvedExpression::Option(inner_opt) => match inner_opt {
            None => {
                todo!("Handle None type inference")
            }
            Some(inner_expr) => {
                let inner_type = resolution(inner_expr);
                ResolvedType::Optional(Box::new(inner_type))
            }
        },
        ResolvedExpression::ArrayExtend(variable_ref, _) => variable_ref.resolved_type.clone(),
        ResolvedExpression::ArrayPush(variable_ref, _) => variable_ref.resolved_type.clone(),
        ResolvedExpression::ArrayRemoveIndex(variable_ref, _) => variable_ref.resolved_type.clone(),
        ResolvedExpression::ArrayClear(variable_ref) => variable_ref.resolved_type.clone(),
    };

    trace!(resolution_expression=%resolution_expression, "resolution first");

    resolution_expression
}

#[derive(Debug)]
pub enum ResolveError {
    CanNotFindModule(ModulePath),
    UnknownStructTypeReference(QualifiedTypeIdentifier),
    UnknownLocalStructTypeReference(LocalTypeIdentifier),
    DuplicateFieldName(IdentifierName),
    Unknown(String),
    //Pest(pest::error::Error<Rule>), // Should not have dependency towards `pest` and Parser.
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
    UnknownTypeReference(QualifiedTypeIdentifier),
    SemanticError(SemanticError),
    SeqMapError(SeqMapError),
    ExpectedMemberCall(Expression),
    CouldNotFindStaticMember(LocalIdentifier, LocalTypeIdentifier),
    TypeAliasNotAStruct(QualifiedTypeIdentifier),
    ModuleNotUnique,
    ExpressionIsOfWrongFieldType,
    ExpectedOptional,
    ExpectedVariable,
    EmptyMapLiteral,
    MapKeyTypeMismatch {
        expected: ResolvedType,
        found: ResolvedType,
    },
    MapValueTypeMismatch {
        expected: ResolvedType,
        found: ResolvedType,
    },
    TypeIsNotAnIndexCollection(ResolvedType),
    NotSameKeyTypeForMapIndex(ResolvedType, ResolvedType),
    NonUniqueKeyValueInMap(SeqMapError),
    UnknownIndexAwareCollection,
    InvalidOperatorForArray(CompoundOperator),
    IncompatibleTypes(ResolvedType, ResolvedType),
    ExpectedArray(ResolvedType),
    UnknownMemberFunction,
}

impl From<SemanticError> for ResolveError {
    fn from(value: SemanticError) -> Self {
        Self::SemanticError(value)
    }
}

impl From<SeqMapError> for ResolveError {
    fn from(value: SeqMapError) -> Self {
        Self::SeqMapError(value)
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
    pub types: &'a ResolvedProgramTypes,
    pub state: &'a mut ResolvedProgramState,
    pub modules: &'a ResolvedModules,
    pub current_module: &'a mut ResolvedModule,
    pub block_scope_stack: Vec<BlockScope>,
}

impl<'a> Resolver<'a> {
    pub fn new(
        types: &'a ResolvedProgramTypes,
        state: &'a mut ResolvedProgramState,
        modules: &'a ResolvedModules,
        current_module: &'a mut ResolvedModule,
    ) -> Self {
        let mut scope_stack = Vec::new();
        scope_stack.push(BlockScope::new());
        Self {
            types,
            state,
            modules,
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
            item_type: resolved_type,
        };

        let rc_array = Rc::new(original_array_type);

        self.state.array_types.push(rc_array.clone());

        Ok(rc_array)
    }

    pub fn resolve_map_type(
        &mut self,
        ast_key_type: &Type,
        ast_value_type: &Type,
    ) -> Result<ResolvedMapTypeRef, ResolveError> {
        // TODO: Check for an existing map type with exact same type

        let key_type = self.resolve_type(ast_key_type)?;
        let value_type = self.resolve_type(ast_value_type)?;

        let original_map_type = ResolvedMapType {
            key_type,
            value_type,
        };

        let map_type_ref = Rc::new(original_map_type);

        //self.state.map_types.push(rc_array.clone());

        Ok(map_type_ref)
    }

    pub fn resolve_type(&mut self, ast_type: &Type) -> Result<ResolvedType, ResolveError> {
        let resolved = match ast_type {
            Type::Any => ResolvedType::Any,
            Type::Int => ResolvedType::Int(self.types.int_type.clone()),
            Type::Float => ResolvedType::Float(self.types.float_type.clone()),
            Type::String => ResolvedType::String(self.types.string_type.clone()),
            Type::Bool => ResolvedType::Bool(self.types.bool_type.clone()),
            Type::Unit => ResolvedType::Unit(self.types.unit_type.clone()),
            Type::Struct(ast_struct) => {
                let (display_type, _struct_ref) = self.get_struct_types(ast_struct)?;
                display_type
            }
            Type::Array(ast_type) => ResolvedType::Array(self.resolve_array_type(ast_type)?),
            Type::Map(key_type, value_type) => {
                ResolvedType::Map(self.resolve_map_type(key_type, value_type)?)
            }
            Type::Tuple(types) => {
                ResolvedType::Tuple(ResolvedTupleType(self.resolve_types(types)?).into())
            }
            Type::Generic(base_type, generic_types) => {
                let base_type = self.resolve_type(base_type)?;
                ResolvedType::Generic(Box::new(base_type), self.resolve_types(generic_types)?)
            }
            Type::Enum(_) => todo!(),
            Type::TypeReference(ast_type_reference) => {
                self.find_type_reference(ast_type_reference)?
            }
            Type::Optional(inner_type_ast) => {
                let inner_resolved_type = self.resolve_type(inner_type_ast)?;
                ResolvedType::Optional(Box::from(inner_resolved_type))
            }
        };

        Ok(resolved)
    }

    fn resolve_types(&mut self, types: &[Type]) -> Result<Vec<ResolvedType>, ResolveError> {
        let mut resolved_types = Vec::new();
        for some_type in types {
            resolved_types.push(self.resolve_type(some_type)?);
        }
        Ok(resolved_types)
    }

    #[must_use]
    pub fn find_module(&self, path: &ModulePath) -> Option<&ResolvedModule> {
        self.modules.get(path)
    }

    fn find_type_reference(
        &self,
        type_name_to_find: &QualifiedTypeIdentifier,
    ) -> Result<ResolvedType, ResolveError> {
        let namespace = self.get_namespace(type_name_to_find)?;
        let type_ident = &type_name_to_find.name;

        let resolved_type = if let Some(aliased_type) = namespace.get_type_alias(&type_ident.text) {
            aliased_type.clone()
        } else if let Some(found) = namespace.get_struct(type_ident) {
            ResolvedType::Struct(found.clone())
        } else if let Some(found) = namespace.get_enum(type_ident) {
            ResolvedType::Enum(found.clone())
        } else {
            Err(ResolveError::UnknownTypeReference(
                type_name_to_find.clone(),
            ))?
        };

        Ok(resolved_type)
    }
    pub fn find_struct_type(
        &self,
        type_name: &QualifiedTypeIdentifier,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        let namespace = self.get_namespace(type_name)?;

        if let Some(aliased_type) = namespace.get_type_alias(&type_name.name.text) {
            let unaliased = unalias_type(aliased_type.clone());
            match unaliased {
                ResolvedType::Struct(struct_ref) => return Ok(struct_ref.clone()),
                _ => return Err(ResolveError::TypeAliasNotAStruct(type_name.clone())),
            }
        }

        namespace.get_struct(&type_name.name).map_or_else(
            || Err(ResolveError::UnknownStructTypeReference(type_name.clone())),
            |found| Ok(found.clone()),
        )
    }

    pub fn find_struct_type_local_mut(
        &mut self,
        type_name: &LocalTypeIdentifier,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        self.find_struct_type(&QualifiedTypeIdentifier::new(type_name.clone(), vec![]))
    }

    fn new_resolver(&mut self) -> Resolver {
        Resolver::new(
            &self.types,
            &mut self.state,
            &self.modules,
            self.current_module,
        )
    }

    pub fn resolve_struct_type_definition(
        &mut self,
        ast_struct: &StructType,
    ) -> Result<ResolvedStructType, ResolveError> {
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
            self.state.allocate_number(),
        );
        Ok(resolved_struct)
    }

    pub fn insert_struct_type_definition(
        &mut self,
        struct_type: ResolvedStructType,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        // Move ownership to module namespace
        let resolved_struct_ref = self.current_module.namespace.add_struct_type(struct_type)?;

        Ok(resolved_struct_ref)
    }

    fn resolve_enum_type_definition(
        &mut self,
        enum_type_name: &LocalTypeIdentifier,
        ast_variants: &SeqMap<LocalTypeIdentifier, EnumVariant>,
    ) -> Result<(ResolvedEnumTypeRef, Vec<ResolvedEnumVariantType>), ResolveError> {
        let mut resolved_variants = Vec::new();

        let parent_number = self.state.allocate_number();

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

                    let number = self.state.allocate_number();

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

                    let number = self.state.allocate_number();

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

            resolved_variants.push(ResolvedEnumVariantType {
                owner: parent_ref.clone(),
                data: container,
                name: name.clone(),
                number: 0,
            })
        }

        Ok((parent_ref.clone(), resolved_variants))
    }

    pub fn insert_definition(
        &mut self,
        resolved_definition: ResolvedDefinition,
    ) -> Result<(), ResolveError> {
        match resolved_definition {
            ResolvedDefinition::EnumType(_parent_enum_ref, variants) => {
                for variant in variants {
                    self.current_module.namespace.add_enum_variant(variant)?;
                }
            }
            ResolvedDefinition::StructType(struct_type) => {
                self.current_module.namespace.add_struct_type(struct_type)?;
            }
            ResolvedDefinition::Function() => {}
            ResolvedDefinition::ExternalFunction() => {}
            ResolvedDefinition::ImplType(resolved_type, resolved_functions) => {
                match resolved_type {
                    ResolvedType::Struct(found_struct) => {
                        found_struct.borrow_mut().functions = resolved_functions;
                    }
                    _ => todo!(),
                }
            }
            ResolvedDefinition::FunctionDef(function_def) => match function_def {
                ResolvedFunction::Internal(internal_fn) => {
                    self.current_module
                        .namespace
                        .add_internal_function_ref(&internal_fn)?;
                }
                ResolvedFunction::External(resolved_external_function_def_ref) => {
                    self.current_module
                        .namespace
                        .add_external_function_declaration_ref(
                            resolved_external_function_def_ref,
                        )?;
                }
            },
            ResolvedDefinition::Alias(resolved_type) => match resolved_type {
                ResolvedType::Alias(name, resolved_type) => {
                    self.current_module
                        .namespace
                        .add_type_alias(&name.0.clone(), *resolved_type.clone())?;
                }
                _ => panic!("type should always be alias"),
            },
            ResolvedDefinition::Comment(_) => {}
        }
        Ok(())
    }

    pub fn resolve_definition(
        &mut self,
        ast_def: &Definition,
    ) -> Result<ResolvedDefinition, ResolveError> {
        let resolved_def = match ast_def {
            Definition::StructDef(ref ast_struct) => {
                ResolvedDefinition::StructType(self.resolve_struct_type_definition(ast_struct)?)
            }
            Definition::EnumDef(identifier, variants) => {
                let (parent, variants) = self.resolve_enum_type_definition(identifier, variants)?;
                ResolvedDefinition::EnumType(parent, variants)
            }
            Definition::FunctionDef(identifier, function) => {
                let mut inner_resolver = self.new_resolver();
                inner_resolver.resolve_function_definition(identifier, function)?
            }
            Definition::ImplDef(type_identifier, functions) => {
                let (attached_type_type, functions) =
                    self.resolve_impl_definition(type_identifier, functions)?;
                ResolvedDefinition::ImplType(attached_type_type, functions)
            }
            Definition::TypeAlias(identifier, target_type) => ResolvedDefinition::Alias(
                self.resolve_type_alias_definition(identifier, target_type)?,
            ),
            Definition::Comment(comment_ref) => ResolvedDefinition::Comment(comment_ref.clone()),
            Definition::Import(_) => todo!(), // TODO: Implement import resolution
        };

        Ok(resolved_def)
    }

    fn resolve_function_definition(
        &mut self,
        identifier: &LocalIdentifier,
        function: &Function,
    ) -> Result<ResolvedDefinition, ResolveError> {
        let resolved_function = match function {
            Function::Internal(function_data) => {
                let parameters = self.resolve_parameters(&function_data.signature.params)?;
                let return_type = self.resolve_type(&function_data.signature.return_type)?;

                // Set up scope for function body
                for param in &parameters {
                    self.set_variable_with_type(
                        &Variable::new(&param.name.clone(), param.is_mutable),
                        &param.resolved_type.clone(),
                    )?;
                }

                let statements = self.resolve_statements(&function_data.body)?;

                let internal = ResolvedInternalFunctionDefinition {
                    signature: ResolvedFunctionSignature {
                        parameters,
                        return_type,
                    },
                    statements,
                    name: identifier.clone(),
                };

                ResolvedFunction::Internal(Rc::new(internal))
            }
            Function::External(signature) => {
                let parameters = self.resolve_parameters(&signature.params)?;
                let return_type = self.resolve_type(&signature.return_type)?;
                let external_function_id = self.state.allocate_external_function_id();

                let external = ResolvedExternalFunctionDefinition {
                    signature: ResolvedFunctionSignature {
                        parameters,
                        return_type,
                    },
                    name: identifier.clone(),
                    id: external_function_id,
                };

                ResolvedFunction::External(Rc::new(external))
            }
        };

        Ok(ResolvedDefinition::FunctionDef(resolved_function))
    }

    fn resolve_impl_func(
        &mut self,
        function: &Function,
        found_struct: &ResolvedStructTypeRef,
    ) -> Result<ResolvedFunction, ResolveError> {
        let resolved_fn = match function {
            Function::Internal(function_data) => {
                let mut parameters = Vec::new();

                // Handle parameters, including self if present
                for param in &function_data.signature.params {
                    let resolved_type = if param.is_self {
                        ResolvedType::Struct(found_struct.clone())
                    } else {
                        self.resolve_type(&param.param_type)?
                    };

                    parameters.push(ResolvedParameter {
                        name: param.variable.name.clone(),
                        resolved_type,
                        ast_parameter: param.clone(),
                        is_mutable: param.is_mutable,
                    });
                }

                let return_type = self.resolve_type(&function_data.signature.return_type)?;

                for param in &parameters {
                    self.set_variable_with_type(
                        &Variable::new(&param.name, param.is_mutable),
                        &param.resolved_type,
                    )?;
                }

                let statements = self.resolve_statements(&function_data.body)?;

                let internal = ResolvedInternalFunctionDefinition {
                    signature: ResolvedFunctionSignature {
                        parameters,
                        return_type,
                    },
                    statements,
                    name: function_data.signature.name.clone(),
                };

                let internal_ref = Rc::new(internal);

                ResolvedFunction::Internal(internal_ref)
            }
            Function::External(signature) => {
                let mut parameters = Vec::new();

                // Handle parameters, including self if present
                for param in &signature.params {
                    let resolved_type = if param.is_self {
                        ResolvedType::Struct(found_struct.clone())
                    } else {
                        self.resolve_type(&param.param_type)?
                    };

                    parameters.push(ResolvedParameter {
                        name: param.variable.name.clone(),
                        resolved_type,
                        ast_parameter: param.clone(),
                        is_mutable: param.is_mutable,
                    });
                }

                let return_type = self.resolve_type(&signature.return_type)?;

                let external = ResolvedExternalFunctionDefinition {
                    name: signature.name.clone(),
                    signature: ResolvedFunctionSignature {
                        parameters,
                        return_type,
                    },
                    id: 0,
                };

                let external_ref = Rc::new(external);

                ResolvedFunction::External(external_ref)
            }
        };
        Ok(resolved_fn)
    }

    pub fn resolve_type_alias_definition(
        &mut self,
        name: &LocalTypeIdentifier,
        target_type: &Type,
    ) -> Result<ResolvedType, ResolveError> {
        let resolved_type = self.resolve_type(target_type)?;
        let alias_type =
            ResolvedType::Alias(LocalTypeName(name.text.clone()), Box::new(resolved_type));

        Ok(alias_type)
    }

    fn resolve_impl_definition(
        &mut self,
        attached_to_type: &LocalTypeIdentifier,
        functions: &SeqMap<IdentifierName, Function>,
    ) -> Result<(ResolvedType, SeqMap<IdentifierName, ResolvedFunctionRef>), ResolveError> {
        let found_struct = self.find_struct_type_local_mut(attached_to_type)?.clone();
        let mut resolved_functions = SeqMap::new();

        for (name, function) in functions {
            let mut inner_resolver = self.new_resolver();
            let resolved_function = inner_resolver.resolve_impl_func(&function, &found_struct)?;
            let resolved_function_ref = Rc::new(resolved_function);
            resolved_functions.insert(name.clone(), resolved_function_ref)?;
        }

        Ok((ResolvedType::Struct(found_struct), resolved_functions))
    }

    fn resolve_let_statement(
        &mut self,
        ast_pattern: &Pattern,
        ast_expression: &Expression,
    ) -> Result<ResolvedStatement, ResolveError> {
        let expr = self.resolve_expression(ast_expression)?;

        let resolved_let = match ast_pattern {
            Pattern::VariableAssignment(variable) => self.assign_variable(&variable, expr)?,
            Pattern::Tuple(_variables) => todo!(), // TODO: PBJ
            Pattern::Struct(_) => todo!(),         // TODO: PBJ
            Pattern::Literal(_) => todo!(),        // TODO: PBJ
            Pattern::EnumTuple(_, _) => todo!(),   // TODO: PBJ
            Pattern::EnumStruct(_, _) => todo!(),  // TODO: PBJ
            Pattern::EnumSimple(_) => todo!(),     // TODO: PBJ
            Pattern::Wildcard => todo!(),          // TODO: PBJ
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

    pub fn resolve_statements(
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

            Expression::IndexAccess(collection_expression, lookup) => {
                let resolved_collection_expression =
                    self.resolve_expression(&collection_expression)?;
                debug!(resolved_collection_expression=?resolved_collection_expression, "resolve_collection_access");
                let collection_resolution = resolution(&resolved_collection_expression);
                debug!(collection_resolution=?collection_resolution, "collection_resolution");
                match &collection_resolution {
                    ResolvedType::Array(array_type) => {
                        let int_expression = self.resolve_usize_index(lookup)?;
                        let array_item = ResolvedArrayItem {
                            array_type: collection_resolution.clone(),
                            item_type: array_type.item_type.clone(),
                            int_expression,
                            array_expression: resolved_collection_expression,
                        };

                        let array_item_ref = Rc::new(array_item); // TODO: why do I require ref?

                        ResolvedExpression::ArrayAccess(array_item_ref)
                    }
                    ResolvedType::Map(map_type_ref) => {
                        let resolved_lookup_expr = self.resolve_expression(lookup)?;
                        let index_type = resolution(&resolved_lookup_expr);

                        if !map_type_ref.key_type.same_type(&index_type) {
                            return Err(ResolveError::NotSameKeyTypeForMapIndex(
                                map_type_ref.key_type.clone(),
                                index_type,
                            ));
                        }
                        let map_lookup = ResolvedMapIndexLookup {
                            map_type: collection_resolution.clone(),
                            item_type: ResolvedType::Optional(Box::from(
                                map_type_ref.key_type.clone(),
                            )),
                            map_type_ref: map_type_ref.clone(),
                            index_expression: Box::from(resolved_lookup_expr),
                            map_expression: Box::from(resolved_collection_expression),
                        };

                        ResolvedExpression::MapIndexAccess(map_lookup)
                    }
                    _ => {
                        return Err(ResolveError::TypeIsNotAnIndexCollection(
                            collection_resolution,
                        ))
                    }
                }
            }

            // Assignments
            Expression::IndexAssignment(collection_expression, index_expression, source_expr) => {
                let resolved_collection_expression =
                    self.resolve_expression(&collection_expression)?;
                debug!(resolved_collection_expression=?resolved_collection_expression, "resolve_collection_access");

                let collection_resolution = resolution(&resolved_collection_expression);
                debug!(collection_resolution=?collection_resolution, "collection_resolution");

                let resolved_index_expression = self.resolve_expression(&index_expression)?;
                debug!(resolved_index_expression=?resolved_index_expression, "resolve_index_access");

                let index_resolution = resolution(&resolved_index_expression);
                debug!(index_resolution=?index_resolution, "index_resolution");

                let resolved_source_expression = self.resolve_expression(source_expr)?;
                let resolved_source_expression_type = resolution(&resolved_source_expression);

                info!("COLLECTION: {resolved_collection_expression}");

                match collection_resolution {
                    ResolvedType::Array(array_type_ref) => {
                        let mut_array = ResolvedMutArray {
                            expression: Box::from(resolved_collection_expression),
                            array_type_ref,
                        };

                        let index_type = ResolvedIndexType {
                            expression: Box::from(resolved_index_expression),
                            resolved_type: index_resolution,
                        };
                        ResolvedExpression::ArrayAssignment(
                            mut_array,
                            index_type,
                            Box::from(resolved_source_expression),
                        )
                    }
                    ResolvedType::Map(map_type_ref) => {
                        let mut_map = ResolvedMutMap {
                            expression: Box::from(resolved_collection_expression),
                            map_type_ref: map_type_ref.clone(),
                        };

                        if !index_resolution.same_type(&map_type_ref.key_type) {
                            return Err(ResolveError::MapKeyTypeMismatch {
                                expected: map_type_ref.key_type.clone(),
                                found: index_resolution,
                            });
                        }

                        if !resolved_source_expression_type.same_type(&map_type_ref.value_type) {
                            return Err(ResolveError::MapValueTypeMismatch {
                                expected: map_type_ref.value_type.clone(),
                                found: resolved_source_expression_type,
                            });
                        }

                        let index_type = ResolvedIndexType {
                            expression: Box::from(resolved_index_expression),
                            resolved_type: index_resolution,
                        };
                        ResolvedExpression::MapAssignment(
                            mut_map,
                            index_type,
                            Box::from(resolved_source_expression),
                        )
                    }
                    _ => return Err(ResolveError::UnknownIndexAwareCollection),
                }
            }

            Expression::VariableAssignment(variable_expression, source_expression) => {
                ResolvedExpression::VariableAssignment(
                    self.resolve_variable_assignment(variable_expression, source_expression)?,
                )
            }

            Expression::IndexCompoundAssignment(_target, _index, _operator, _source) => {
                // self.resolve_compound_assignment_index(source, index, operator, source)?
                todo!()
            }

            Expression::VariableCompoundAssignment(target, operator, source) => {
                self.resolve_compound_assignment_variable(target, operator, source)?
            }

            Expression::FieldCompoundAssignment(_target, _field, _operator, _source) => {
                //self.resolve_compound_assignment_field(target, field, operator, source)?
                todo!()
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

            // Postfix operators
            Expression::PostfixOp(operator, expression) => {
                ResolvedExpression::PostfixOp(self.resolve_postfix_op(operator, expression)?)
            }

            // Calls
            Expression::FunctionCall(function_expression, parameter_expressions) => {
                self.resolve_function_call(function_expression, parameter_expressions)?
            }
            Expression::StaticCall(type_name, function_name, arguments) => {
                ResolvedExpression::StaticCall(self.resolve_static_call(
                    type_name,
                    function_name,
                    arguments,
                )?)
            }

            Expression::StaticCallGeneric(type_name, function_name, arguments, generic_types) => {
                ResolvedExpression::StaticCallGeneric(
                    self.resolve_static_call_generic(type_name, function_name, generic_types, arguments)?
                )
            },

            Expression::MemberCall(ast_member_expression, ast_identifier, ast_arguments) => {
                let result = self.check_for_internal_member_call(
                    ast_member_expression,
                    ast_identifier,
                    ast_arguments,
                )?;

                if let Some(internal_expression) = result {
                    return Ok(internal_expression);
                }

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
                self.types.string_type.clone(),
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
                    self.types.exclusive_range_type.clone(),
                    Box::from(min_expression),
                    Box::from(max_expression),
                )
            }

            Expression::Literal(literal) => {
                swamp_script_semantic::ResolvedExpression::Literal(match literal {
                    Literal::Int(value) => {
                        ResolvedLiteral::IntLiteral(*value, self.types.int_type.clone())
                    }
                    Literal::Float(value) => {
                        ResolvedLiteral::FloatLiteral(*value, self.types.float_type.clone())
                    }
                    Literal::String(value) => ResolvedLiteral::StringLiteral(
                        value.clone(),
                        self.types.string_type.clone(),
                    ),
                    Literal::Unit => ResolvedLiteral::UnitLiteral(self.types.unit_type.clone()),
                    Literal::None => ResolvedLiteral::NoneLiteral,
                    Literal::Bool(value) => {
                        ResolvedLiteral::BoolLiteral(*value, self.types.bool_type.clone())
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

                    Literal::Map(entries) => self.resolve_map_literal(entries)?,
                })
            }

            // Comparison
            Expression::IfElse(condition, consequence, alternate) => {
                self.analyze_optional_condition(condition, consequence, alternate)?
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

    fn get_struct_types(
        &self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
    ) -> Result<(ResolvedType, ResolvedStructTypeRef), ResolveError> {
        let namespace = self.get_namespace(qualified_type_identifier)?;

        // If it's an alias, return both the alias and the underlying struct
        if let Some(alias_type) = namespace.get_type_alias(&qualified_type_identifier.name.text) {
            let unaliased = unalias_type(alias_type.clone());
            match unaliased {
                ResolvedType::Struct(struct_ref) => Ok((alias_type.clone(), struct_ref)),
                _ => Err(ResolveError::TypeAliasNotAStruct(
                    qualified_type_identifier.clone(),
                )),
            }
        } else {
            // If direct struct, return the struct type for both
            let struct_ref = namespace
                .get_struct(&qualified_type_identifier.name)
                .ok_or_else(|| {
                    ResolveError::UnknownStructTypeReference(qualified_type_identifier.clone())
                })?;
            Ok((ResolvedType::Struct(struct_ref.clone()), struct_ref.clone()))
        }
    }

    fn resolve_struct_instantiation(
        &mut self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
        ast_fields: &SeqMap<IdentifierName, Expression>,
    ) -> Result<ResolvedStructInstantiation, ResolveError> {
        let (display_type_ref, struct_to_instantiate) =
            self.get_struct_types(qualified_type_identifier)?;

        if ast_fields.len() != struct_to_instantiate.borrow().fields.len() {
            return Err(ResolveError::WrongFieldCountInStructInstantiation(
                struct_to_instantiate,
                ast_fields.clone(),
            ));
        }

        let mut expressions_in_order = Vec::new();

        for (field_name, field_type) in &struct_to_instantiate.borrow().fields {
            if let Some(found) = ast_fields.get(field_name) {
                let resolved_expression = self.resolve_expression(found)?;

                let upgraded_resolved_expression =
                    wrap_in_some_if_optional(field_type, resolved_expression);

                let expression_type = resolution(&upgraded_resolved_expression);
                info!("expression_type: target:{field_type}  source_type:{expression_type} source_expression:{upgraded_resolved_expression}");

                if !field_type.same_type(&expression_type) {
                    return Err(ResolveError::ExpressionIsOfWrongFieldType);
                }

                expressions_in_order.push(upgraded_resolved_expression);
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
            display_type_ref,
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
        self.set_variable_with_type(variable, expression_type)?;
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
                ResolvedPattern::EnumSimple(enum_variant_type_ref)
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
            if !argument_type.same_type(parameter_type) {
                error!("{argument_type}: {resolved_argument_expression}");
                return Err(ResolveError::IncompatibleArguments(
                    argument_type,
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
            self.resolve_and_verify_parameters(&fn_def.signature.parameters, arguments)?;

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
            self.resolve_and_verify_parameters(&fn_def.signature.parameters, arguments)?;

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
                // Check if this is a member function (has self parameter)
                if function_call
                    .signature
                    .parameters
                    .first()
                    .map_or(false, |p| p.ast_parameter.is_self)
                {
                    // For member functions, extract the target object from the function expression
                    match function_expression {
                        Expression::MemberCall(target, _, _) => {
                            // Use the target as the first argument (self)
                            let mut all_args = Vec::new();
                            all_args.push(*target.clone());
                            all_args.extend(arguments.iter().cloned());

                            self.resolve_internal_function_call(
                                function_call,
                                function_expr,
                                &all_args,
                            )
                        }
                        _ => Err(ResolveError::ExpectedMemberCall(
                            function_expression.clone(),
                        )),
                    }
                } else {
                    // Regular function call
                    self.resolve_internal_function_call(function_call, function_expr, arguments)
                }
            }
            ResolvedType::FunctionExternal(ref function_call) => {
                // Similar handling for external functions
                if function_call
                    .signature
                    .parameters
                    .first()
                    .map_or(false, |p| p.ast_parameter.is_self)
                {
                    match function_expression {
                        Expression::MemberCall(target, _, _) => {
                            let mut all_args = Vec::new();
                            all_args.push(*target.clone());
                            all_args.extend(arguments.iter().cloned());

                            self.resolve_external_function_call(
                                function_call,
                                function_expr,
                                &all_args,
                            )
                        }
                        _ => Err(ResolveError::ExpectedMemberCall(
                            function_expression.clone(),
                        )),
                    }
                } else {
                    self.resolve_external_function_call(function_call, function_expr, arguments)
                }
            }
            _ => Err(ResolveError::ExpectedFunctionExpression(
                function_expression.clone(),
            )),
        }
    }

    fn resolve_static_call(
        &mut self,
        type_name: &LocalTypeIdentifier,
        function_name: &LocalIdentifier,
        arguments: &Vec<Expression>,
    ) -> Result<ResolvedStaticCall, ResolveError> {
        // Resolve arguments first
        let resolved_arguments = self.resolve_expressions(arguments)?;

        // Get struct type just to look up the function
        let struct_type_ref = self.find_struct_type_local_mut(type_name)?;
        let struct_ref = struct_type_ref.borrow();

        if let Some(function_ref) = struct_ref
            .functions
            .get(&IdentifierName(function_name.text.clone()))
        {
            Ok(ResolvedStaticCall {
                function: function_ref.clone(),
                arguments: resolved_arguments,
                // Remove struct_type_ref since it's a static call
            })
        } else {
            Err(ResolveError::CouldNotFindStaticMember(
                function_name.clone(),
                type_name.clone(),
            ))
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

        let struct_ref = resolved_struct_type_ref.borrow();
        if let Some(function_ref) = struct_ref
            .functions
            .get(&IdentifierName(ast_member_function_name.text.clone()))
        {
            let (is_self_mutable, signature) = match &**function_ref {
                ResolvedFunction::Internal(function_data) => {
                    let first_param = function_data
                        .signature
                        .parameters
                        .first()
                        .ok_or_else(|| ResolveError::WrongNumberOfArguments(0, 1))?;
                    (first_param.is_mutable, &function_data.signature)
                }
                ResolvedFunction::External(external) => {
                    let first_param = external
                        .signature
                        .parameters
                        .first()
                        .ok_or_else(|| ResolveError::WrongNumberOfArguments(0, 1))?;
                    (first_param.is_mutable, &external.signature)
                }
            };

            // Validate argument count (subtract 1 for self parameter)
            if ast_arguments.len() != signature.parameters.len() - 1 {
                return Err(ResolveError::WrongNumberOfArguments(
                    ast_arguments.len(),
                    signature.parameters.len() - 1,
                ));
            }

            // Now resolve the arguments
            let resolved_arguments = self.resolve_expressions(ast_arguments)?;

            Ok(ResolvedMemberCall {
                function: function_ref.clone(),
                arguments: resolved_arguments,
                self_expression: Box::new(resolved_expression),
                struct_type_ref: resolved_struct_type_ref.clone(),
                self_is_mutable: is_self_mutable,
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

    fn resolve_postfix_op(
        &mut self,
        ast_op: &PostfixOperator,
        ast_left: &Box<Expression>,
    ) -> Result<ResolvedPostfixOperator, ResolveError> {
        let left = self.resolve_expression(ast_left)?;
        let resolved_type = resolution(&left);

        Ok(ResolvedPostfixOperator {
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
                    .get_internal_function(&variable.name)
                    .map_or_else(
                        || {
                            self.current_module
                                .namespace
                                .get_external_function_declaration(&variable.name)
                                .map_or_else(
                                    || {
                                        error!("unknown external function {}", variable.name);
                                        Err(ResolveError::UnknownVariable(variable.clone()))
                                    },
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
        for scope in self.block_scope_stack.iter().rev() {
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

    fn resolve_usize_index(
        &mut self,
        usize_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let lookup_expression = self.resolve_expression(usize_expression)?;
        debug!(lookup_expression=?lookup_expression, "lookup_expression");
        let lookup_resolution = resolution(&lookup_expression);
        debug!(lookup_resolution=?lookup_resolution, "lookup_resolution");

        match &lookup_resolution {
            ResolvedType::Int(_) => {}
            _ => Err(ResolveError::ArrayIndexMustBeInt(usize_expression.clone()))?,
        }

        Ok(lookup_expression)
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

    fn resolve_compound_assignment_variable(
        &mut self,
        target: &Variable,
        operator: &CompoundOperator,
        source: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_variable = self.find_variable(target)?;
        let resolved_source = self.resolve_expression(source)?;

        let target_type = &resolved_variable.resolved_type;

        match &target_type {
            ResolvedType::Array(array_type_ref) => {
                match operator {
                    CompoundOperator::Add => {
                        let source_type = resolution(&resolved_source);
                        match &source_type {
                            ResolvedType::Array(_source_array_type) => {
                                // Concatenating two arrays
                                if !target_type.same_type(&source_type) {
                                    return Err(ResolveError::IncompatibleTypes(
                                        source_type,
                                        target_type.clone(),
                                    ));
                                }
                                Ok(ResolvedExpression::ArrayExtend(
                                    resolved_variable,
                                    Box::new(resolved_source),
                                ))
                            }
                            _ => {
                                // Appending a single item
                                if !source_type.same_type(&array_type_ref.item_type) {
                                    return Err(ResolveError::IncompatibleTypes(
                                        source_type,
                                        target_type.clone(),
                                    ));
                                }
                                Ok(ResolvedExpression::ArrayPush(
                                    resolved_variable,
                                    Box::new(resolved_source),
                                ))
                            }
                        }
                    }
                    _ => Err(ResolveError::InvalidOperatorForArray(operator.clone())),
                }
            }
            _ => Err(ResolveError::ExpectedArray(target_type.clone())),
        }
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

    fn resolve_map_literal(
        &mut self,
        entries: &[(Expression, Expression)],
    ) -> Result<ResolvedLiteral, ResolveError> {
        if entries.is_empty() {
            return Err(ResolveError::EmptyMapLiteral);
        }

        // Resolve first entry to determine map types
        let (first_key, first_value) = &entries[0];
        let resolved_first_key = self.resolve_expression(first_key)?;
        let resolved_first_value = self.resolve_expression(first_value)?;
        let key_type = resolution(&resolved_first_key);
        let value_type = resolution(&resolved_first_value);

        // Check all entries match the types
        let mut resolved_entries = Vec::new();
        resolved_entries.push((resolved_first_key, resolved_first_value));

        for (key, value) in entries.iter().skip(1) {
            let resolved_key = self.resolve_expression(key)?;
            let resolved_value = self.resolve_expression(value)?;

            if !resolution(&resolved_key).same_type(&key_type) {
                return Err(ResolveError::MapKeyTypeMismatch {
                    expected: key_type.clone(),
                    found: resolution(&resolved_key),
                });
            }

            if !resolution(&resolved_value).same_type(&value_type) {
                return Err(ResolveError::MapValueTypeMismatch {
                    expected: value_type.clone(),
                    found: resolution(&resolved_value),
                });
            }

            resolved_entries.push((resolved_key, resolved_value));
        }

        let resolved_map_type = ResolvedMapType {
            key_type,
            value_type,
        };

        let resolved_map_type_ref = Rc::new(resolved_map_type);

        Ok(ResolvedLiteral::Map(
            resolved_map_type_ref,
            resolved_entries,
        ))
    }

    fn push_block_scope(&mut self) {
        info!("pushing scope stack");

        self.block_scope_stack.push(BlockScope {
            variables: SeqMap::default(),
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
        if let Some(existing_variable) = self.try_find_variable(variable) {
            if !&existing_variable.resolved_type.same_type(variable_type_ref) {
                return Err(ResolveError::OverwriteVariableWithAnotherType(
                    variable.clone(),
                ));
            }
            if !existing_variable.ast_variable.is_mutable {
                return Err(ResolveError::CanOnlyOverwriteVariableWithMut(
                    variable.clone(),
                ));
            }

            return Ok((existing_variable, true));
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
            Literal::Int(value) => ResolvedLiteral::IntLiteral(*value, self.types.int_type.clone()),
            Literal::Float(value) => {
                ResolvedLiteral::FloatLiteral(*value, self.types.float_type.clone())
            }
            Literal::String(value) => {
                ResolvedLiteral::StringLiteral(value.clone(), self.types.string_type.clone())
            }
            Literal::Bool(value) => {
                ResolvedLiteral::BoolLiteral(*value, self.types.bool_type.clone())
            }
            Literal::EnumVariant(_, _, _) => todo!(), // TODO: PBJ
            Literal::Tuple(_) => todo!(),             // TODO: PBJ
            Literal::Array(_) => todo!(),             // TODO: PBJ
            Literal::Map(_) => todo!(),               // TODO: PBJ
            Literal::Unit => todo!(),                 // TODO: PBJ
            Literal::None => todo!(),                 // TODO: PBJ
        };

        ResolvedPattern::Literal(resolved_literal)
    }

    /// Analyzes an expression that might contain an optional unwrap operator,
    /// creating a new scope with the unwrapped shadow variable if necessary.
    /// It is a bit hacky code in the analyzer, but it enables a very ergonomic syntax of
    /// `if a? { print('a is {a}') }
    fn analyze_optional_condition(
        &mut self,
        condition: &Expression,
        true_block: &Expression,
        false_block: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        match condition {
            Expression::PostfixOp(PostfixOperator::Unwrap, expr) => {
                if let Expression::VariableAccess(var) = &**expr {
                    self.handle_optional_unwrap(var, expr, true_block, false_block)
                } else {
                    Err(ResolveError::ExpectedVariable)
                }
            }

            Expression::VariableAssignment(var, expr) => {
                if let Expression::PostfixOp(PostfixOperator::Unwrap, inner_expr) = &**expr {
                    self.handle_optional_assign_unwrap(var, inner_expr, true_block, false_block)
                } else {
                    self.resolve_normal_if(condition, true_block, false_block)
                }
            }

            _ => self.resolve_normal_if(condition, true_block, false_block),
        }
    }

    fn handle_optional_unwrap(
        &mut self,
        var: &Variable,
        expr: &Expression,
        true_block: &Expression,
        false_block: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_var_expr = self.resolve_expression(expr)?;

        if let ResolvedType::Optional(inner_type) = resolution(&resolved_var_expr) {
            let (resolved_var_ref, resolved_true) =
                self.analyze_in_new_scope(var, &*inner_type, true_block)?;

            Ok(ResolvedExpression::IfElseOnlyVariable {
                variable: resolved_var_ref,
                optional_expr: Box::new(resolved_var_expr),
                true_block: Box::new(resolved_true),
                false_block: Box::new(self.resolve_expression(false_block)?),
            })
        } else {
            Err(ResolveError::ExpectedOptional)
        }
    }

    fn handle_optional_assign_unwrap(
        &mut self,
        var: &Variable,
        inner_expr: &Expression,
        true_block: &Expression,
        false_block: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_expr = self.resolve_expression(inner_expr)?;

        if let ResolvedType::Optional(inner_type) = resolution(&resolved_expr) {
            let (resolved_var_ref, resolved_true) =
                self.analyze_in_new_scope(var, &inner_type, true_block)?;

            Ok(ResolvedExpression::IfElseAssignExpression {
                variable: resolved_var_ref,
                optional_expr: Box::new(resolved_expr),
                true_block: Box::new(resolved_true),
                false_block: Box::new(self.resolve_expression(false_block)?),
            })
        } else {
            Err(ResolveError::ExpectedOptional)
        }
    }

    fn analyze_in_new_scope(
        &mut self,
        var: &Variable,
        type_: &ResolvedType,
        block: &Expression,
    ) -> Result<(ResolvedVariableRef, ResolvedExpression), ResolveError> {
        self.push_block_scope();
        let resolved_var_ref = self.set_variable_with_type(var, type_)?;
        let resolved_expr = self.resolve_expression(block)?;
        self.pop_block_scope();

        Ok((resolved_var_ref, resolved_expr))
    }

    fn analyze_block_with_scope(
        &mut self,
        block: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        self.push_block_scope();
        let resolved = self.resolve_expression(block)?;
        self.pop_block_scope();
        Ok(resolved)
    }

    fn resolve_normal_if(
        &mut self,
        condition: &Expression,
        true_block: &Expression,
        false_block: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_condition = self.resolve_expression(condition)?;
        let resolved_true = self.analyze_block_with_scope(true_block)?;
        let resolved_false = self.analyze_block_with_scope(false_block)?;

        Ok(ResolvedExpression::IfElse(
            Box::from(resolved_condition),
            Box::from(resolved_true),
            Box::from(resolved_false),
        ))
    }

    fn resolve_array_member_call(
        &mut self,
        var_ref: ResolvedVariableRef,
        ast_member_function_name: &LocalIdentifier,
        ast_arguments: &[Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        let expr = match ast_member_function_name.text.as_str() {
            "remove" => {
                if !var_ref.is_mutable() {
                    return Err(ResolveError::VariableIsNotMutable(
                        var_ref.ast_variable.clone(),
                    ));
                }

                if ast_arguments.len() != 1 {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 1));
                }
                let index_expr = self.resolve_usize_index(&ast_arguments[0])?;

                ResolvedExpression::ArrayRemoveIndex(var_ref, Box::new(index_expr))
            }

            "clear" => {
                if !var_ref.is_mutable() {
                    return Err(ResolveError::VariableIsNotMutable(
                        var_ref.ast_variable.clone(),
                    ));
                }

                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }

                ResolvedExpression::ArrayClear(var_ref)
            }
            _ => return Err(ResolveError::UnknownMemberFunction),
        };

        Ok(expr)
    }

    fn check_for_internal_member_call(
        &mut self,
        source: &Expression,
        ast_member_function_name: &LocalIdentifier,
        ast_arguments: &[Expression],
    ) -> Result<Option<ResolvedExpression>, ResolveError> {
        let resolved_expr = self.resolve_expression(source)?;
        if let ResolvedType::Array(_) = resolution(&resolved_expr) {
            if let ResolvedExpression::VariableAccess(var_ref) = resolved_expr {
                let resolved = self.resolve_array_member_call(
                    var_ref,
                    ast_member_function_name,
                    ast_arguments,
                )?;
                return Ok(Some(resolved));
            }
        }
        Ok(None)
    }

    fn resolve_static_call_generic(
        &mut self,
        type_name: &LocalTypeIdentifier,
        function_name: &LocalIdentifier,
        generic_types: &Vec<Type>,
        arguments: &Vec<Expression>,
    ) -> Result<ResolvedStaticCallGeneric, ResolveError> {
        let resolved_arguments = self.resolve_expressions(arguments)?;
        let resolved_generic_types = self.resolve_types(generic_types)?;
        
        let struct_type_ref = self.find_struct_type_local_mut(type_name)?;
        let struct_ref = struct_type_ref.borrow();

        if let Some(function_ref) = struct_ref.functions.get(&IdentifierName(function_name.text.clone())) {
            Ok(ResolvedStaticCallGeneric {
                function: function_ref.clone(),
                arguments: resolved_arguments,
                generic_types: resolved_generic_types,
            })
        } else {
            Err(ResolveError::CouldNotFindStaticMember(
                function_name.clone(),
                type_name.clone(),
            ))
        }
    }
}

fn wrap_in_some_if_optional(
    target_type: &ResolvedType,
    resolved_value: ResolvedExpression,
) -> ResolvedExpression {
    match target_type {
        ResolvedType::Optional(_) => match resolved_value {
            ResolvedExpression::Option(_) => resolved_value,
            _ => ResolvedExpression::Option(Some(Box::new(resolved_value))),
        },
        _ => resolved_value,
    }
}
