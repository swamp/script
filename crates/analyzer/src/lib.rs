/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod lookup;
pub mod prelude;

use crate::lookup::NameLookup;
use seq_map::{SeqMap, SeqMapError};
use std::cell::RefCell;
use std::collections::HashSet;
use std::mem::take;
use std::num::{ParseFloatError, ParseIntError};
use std::rc::Rc;
use swamp_script_ast::prelude::*;
use swamp_script_ast::{
    CompoundOperator, CompoundOperatorKind, EnumVariantLiteral, FieldExpression, ForPattern,
    Function, PatternElement, PostfixOperator, SpanWithoutFileId,
};
use swamp_script_semantic::modules::ResolvedModules;
use swamp_script_semantic::{
    create_rust_type, prelude::*, FileId, LocalTypeName, ResolvedAccess,
    ResolvedAnonymousStructFieldType, ResolvedAnonymousStructType, ResolvedBinaryOperatorKind,
    ResolvedBoolType, ResolvedCompoundOperator, ResolvedCompoundOperatorKind, ResolvedFieldName,
    ResolvedForPattern, ResolvedFormatSpecifier, ResolvedFormatSpecifierKind,
    ResolvedLocalIdentifier, ResolvedLocalTypeIdentifier, ResolvedPatternElement,
    ResolvedPostfixOperatorKind, ResolvedPrecisionType, ResolvedStaticCallGeneric,
    ResolvedTupleTypeRef, ResolvedUnaryOperatorKind, ResolvedVariableCompoundAssignment, Span,
    TypeNumber,
};
use swamp_script_semantic::{
    ResolvedDefinition, ResolvedEnumTypeRef, ResolvedFunction, ResolvedFunctionRef,
    ResolvedFunctionSignature, ResolvedMapTypeRef, ResolvedMutMap, ResolvedStaticCall,
};
use swamp_script_semantic::{ResolvedMapIndexLookup, ResolvedProgramTypes};
use swamp_script_semantic::{ResolvedMapType, ResolvedProgramState};
use swamp_script_semantic::{ResolvedModulePath, ResolvedPostfixOperator};
use swamp_script_source_map::SourceMap;
use tracing::{debug, error, info, warn};

#[derive(Debug)]
pub struct ResolvedProgram {
    pub types: ResolvedProgramTypes,
    pub state: ResolvedProgramState,
    pub modules: ResolvedModules,
}

impl ResolvedProgram {
    #[must_use]
    pub fn new() -> Self {
        Self {
            types: ResolvedProgramTypes::new(),
            state: ResolvedProgramState::new(),
            modules: ResolvedModules::new(),
        }
    }
}

#[derive(Debug)]
pub enum NamespaceError {}

impl From<NamespaceError> for ResolveError {
    fn from(error: NamespaceError) -> Self {
        Self::NamespaceError(error)
    }
}

#[must_use]
pub const fn convert_span(without: &SpanWithoutFileId, file_id: FileId) -> Span {
    Span {
        file_id,
        offset: without.offset,
        length: without.length,
    }
}

pub const SPARSE_TYPE_ID: TypeNumber = 999;

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
    let resolution_expression = match expression {
        ResolvedExpression::FieldAccess(_expr, struct_field_ref, _lookups) => {
            struct_field_ref.resolved_type.clone()
        }
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
        ResolvedExpression::InitializeVariable(variable_assignment) => {
            variable_assignment.variable_refs[0].resolved_type.clone()
        }
        ResolvedExpression::ReassignVariable(variable_assignments) => {
            variable_assignments.variable_refs[0].resolved_type.clone()
        }

        ResolvedExpression::ArrayAssignment(_, _, _) => todo!(),
        ResolvedExpression::MapAssignment(_, _, _) => todo!(),
        ResolvedExpression::StructFieldAssignment(_struct_field, _lookups, source_resolution) => {
            resolution(source_resolution)
        }
        ResolvedExpression::FieldCompoundAssignment(
            _resolved_expression,
            _access,
            _op_,
            source_resolution,
        ) => resolution(source_resolution),
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
        }
        ResolvedExpression::StaticCallGeneric(static_call_generic) => {
            signature(&static_call_generic.function).return_type.clone()
        }

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
            ResolvedLiteral::BoolLiteral(_value, _node, bool_type_ref) => {
                ResolvedType::Bool(bool_type_ref.clone())
            }
            ResolvedLiteral::FloatLiteral(_float_value, _node, float_type) => {
                ResolvedType::Float(float_type.clone())
            }
            ResolvedLiteral::IntLiteral(_int_value, _node, int_type) => {
                ResolvedType::Int(int_type.clone())
            }
            ResolvedLiteral::StringLiteral(_string_value, _node, string_type) => {
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
            ResolvedLiteral::NoneLiteral(_) => ResolvedType::Any,
        },
        ResolvedExpression::Option(inner_opt) => inner_opt.as_ref().map_or_else(
            || todo!("Handle None type inference"),
            |inner_expr| {
                let inner_type = resolution(inner_expr);
                ResolvedType::Optional(Box::new(inner_type))
            },
        ),
        ResolvedExpression::ArrayExtend(variable_ref, _) => variable_ref.resolved_type.clone(),
        ResolvedExpression::ArrayPush(variable_ref, _) => variable_ref.resolved_type.clone(),
        ResolvedExpression::ArrayRemoveIndex(variable_ref, _) => variable_ref.resolved_type.clone(),
        ResolvedExpression::ArrayClear(variable_ref) => variable_ref.resolved_type.clone(),
        ResolvedExpression::SparseAdd(_, _) => ResolvedType::Any, // TODO: return correct type
        ResolvedExpression::SparseRemove(_, _) => ResolvedType::Any, // TODO: return correct type
        ResolvedExpression::SparseNew(_rust_type_ref, resolved_type) => resolved_type.clone(),
        ResolvedExpression::CoerceOptionToBool(_) => ResolvedType::Bool(Rc::new(ResolvedBoolType)),
        ResolvedExpression::FloatFloor(_) => ResolvedType::Int(Rc::new(ResolvedIntType {})),
        ResolvedExpression::FloatRound(_) => ResolvedType::Int(Rc::new(ResolvedIntType {})),
        ResolvedExpression::FloatSign(_) => ResolvedType::Float(Rc::new(ResolvedFloatType {})),
        ResolvedExpression::FloatAbs(_) => ResolvedType::Float(Rc::new(ResolvedFloatType {})),
        ResolvedExpression::VariableCompoundAssignment(var_compound_assignment) => {
            var_compound_assignment.variable_ref.resolved_type.clone()
        }
    };

    resolution_expression
}

#[derive(Debug)]
pub enum ResolveError {
    NamespaceError(NamespaceError),
    CanNotFindModule(Vec<String>),
    UnknownStructTypeReference(ResolvedNode),
    UnknownLocalStructTypeReference(LocalTypeIdentifier),
    DuplicateFieldName(ResolvedNode),
    Unknown(String),
    UnknownImplTargetTypeReference(LocalTypeIdentifier),
    WrongFieldCountInStructInstantiation(ResolvedStructTypeRef, usize),
    MissingFieldInStructInstantiation(String, ResolvedStructTypeRef),
    ExpectedFunctionExpression,
    CouldNotFindMember(ResolvedNode, ResolvedNode),
    UnknownVariable(ResolvedNode),
    NotAnArray(Expression),
    ArrayIndexMustBeInt(ResolvedType),
    OverwriteVariableWithAnotherType(ResolvedNode),
    WrongNumberOfArguments(usize, usize),
    IncompatibleArguments(ResolvedType, ResolvedType),
    CanOnlyOverwriteVariableWithMut(ResolvedNode),
    OverwriteVariableNotAllowedHere(ResolvedNode),
    NotNamedStruct(ResolvedType),
    UnknownEnumVariantType(QualifiedTypeIdentifier),
    WasNotStructType(ResolvedNode),
    UnknownStructField(ResolvedNode),
    MustBeEnumType(Pattern),
    UnknownEnumVariantTypeInPattern(ResolvedNode),
    ExpectedEnumInPattern(ResolvedNode),
    WrongEnumVariantContainer(ResolvedEnumVariantTypeRef),
    VariableIsNotMutable(ResolvedNode),
    ArgumentIsNotMutable,
    WrongNumberOfTupleDeconstructVariables,
    UnknownTypeReference(ResolvedNode),
    SemanticError(SemanticError),
    SeqMapError(SeqMapError),
    ExpectedMemberCall(ResolvedNode),
    CouldNotFindStaticMember(ResolvedNode, ResolvedNode),
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
    InvalidOperatorForArray(ResolvedNode),
    IncompatibleTypes(Span, ResolvedType),
    ExpectedArray(ResolvedType),
    UnknownMemberFunction(ResolvedNode),
    WrongNumberOfTypeArguments(usize, i32),
    OnlyVariablesAllowedInEnumPattern,
    ExpressionsNotAllowedInLetPattern,
    UnknownField(ResolvedNode),
    EnumVariantHasNoFields(ResolvedNode),
    TooManyTupleFields {
        max: usize,
        got: usize,
    },
    NotInFunction,
    ExpectedBooleanExpression(Span),
    NotAnIterator(Span),
    UnsupportedIteratorPairs,
    NeedStructForFieldLookup,
    IntConversionError(ParseIntError),
    FloatConversionError(ParseFloatError),
    BoolConversionError,
    DuplicateFieldInStructInstantiation(String, Rc<RefCell<ResolvedStructType>>),
    InternalError(&'static str),
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

pub struct SharedState<'a> {
    pub types: &'a ResolvedProgramTypes,
    pub state: &'a mut ResolvedProgramState,
    pub lookup: &'a mut NameLookup<'a>,
    pub source_map: &'a SourceMap,
    pub file_id: FileId,
}

pub struct FunctionScopeState {
    pub block_scope_stack: Vec<BlockScope>,
    pub return_type: ResolvedType,
}

impl FunctionScopeState {
    pub fn new(return_type: ResolvedType) -> Self {
        Self {
            block_scope_stack: vec![BlockScope::new()],
            return_type,
        }
    }
}

pub struct Resolver<'a> {
    shared: SharedState<'a>,
    scope: FunctionScopeState,
    global: FunctionScopeState,
}

impl<'a> Resolver<'a> {
    pub fn new(
        types: &'a ResolvedProgramTypes,
        state: &'a mut ResolvedProgramState,
        lookup: &'a mut NameLookup<'a>,
        source_map: &'a SourceMap,
        file_id: FileId,
    ) -> Self {
        let shared = SharedState {
            types,
            state,
            lookup,
            source_map,
            file_id,
        };
        Self {
            scope: FunctionScopeState::new(ResolvedType::Any),
            global: FunctionScopeState::new(ResolvedType::Any),
            shared,
        }
    }

    fn start_function(&mut self, return_type: ResolvedType) {
        self.global.block_scope_stack = take(&mut self.scope.block_scope_stack);
        self.scope = FunctionScopeState::new(return_type);
    }

    fn stop_function(&mut self) {
        self.scope.block_scope_stack = take(&mut self.global.block_scope_stack);
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

        self.shared.state.array_types.push(rc_array.clone());

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

    fn handle_optional_unwrap_statement(
        &mut self,
        var: &Variable,
        expr: &Expression,
        statements: &Vec<Statement>,
        maybe_else_statements: &Option<Vec<Statement>>,
    ) -> Result<ResolvedStatement, ResolveError> {
        let resolved_var_expr = self.resolve_expression(expr)?;

        if let ResolvedType::Optional(inner_type) = resolution(&resolved_var_expr) {
            self.push_block_scope("if_unwrap");
            let resolved_var_ref =
                self.create_local_variable(&var.name, &var.is_mutable, &inner_type)?;
            let resolved_true = self.resolve_statements(statements)?;
            self.pop_block_scope("if_unwrap");

            let resolved_false = if let Some(else_statements) = maybe_else_statements {
                Some(self.resolve_statements(else_statements)?)
            } else {
                None
            };

            Ok(ResolvedStatement::IfOnlyVariable {
                variable: resolved_var_ref,
                optional_expr: Box::new(resolved_var_expr),
                true_block: resolved_true,
                false_block: resolved_false,
            })
        } else {
            Err(ResolveError::ExpectedOptional)
        }
    }

    fn handle_optional_assign_unwrap_statement(
        &mut self,
        var: &Variable,
        inner_expr: &Expression,
        statements: &Vec<Statement>,
        maybe_else_statements: &Option<Vec<Statement>>,
    ) -> Result<ResolvedStatement, ResolveError> {
        let resolved_expr = self.resolve_expression(inner_expr)?;

        if let ResolvedType::Optional(inner_type) = resolution(&resolved_expr) {
            self.push_block_scope("if_assign_unwrap");
            let resolved_var_ref =
                self.create_local_variable(&var.name, &var.is_mutable, &inner_type)?;
            let resolved_true = self.resolve_statements(statements)?;
            self.pop_block_scope("if_assign_unwrap");

            let resolved_false = if let Some(else_statements) = maybe_else_statements {
                Some(self.resolve_statements(else_statements)?)
            } else {
                None
            };

            Ok(ResolvedStatement::IfAssignExpression {
                variable: resolved_var_ref,
                optional_expr: Box::new(resolved_expr),
                true_block: resolved_true,
                false_block: resolved_false,
            })
        } else {
            Err(ResolveError::ExpectedOptional)
        }
    }

    fn resolve_normal_if_statement(
        &mut self,
        condition: &Expression,
        statements: &[Statement],
        maybe_else_statements: &Option<Vec<Statement>>,
    ) -> Result<ResolvedStatement, ResolveError> {
        let condition = self.resolve_bool_expression(condition)?;

        // For the true branch
        let mut resolved_statements = Vec::new();
        for (i, stmt) in statements.iter().enumerate() {
            let resolved_stmt = self.resolve_statement(stmt)?;
            /*
            if i == statements.len() - 1 {
                if let ResolvedStatement::Expression(expr) = resolved_stmt {
                    resolved_stmt = ResolvedStatement::Expression(ResolvedExpression::Option(
                        Some(Box::new(expr)),
                    ));
                }
            }

             */
            resolved_statements.push(resolved_stmt);
        }

        // For the else branch
        let else_statements = if let Some(else_statements) = maybe_else_statements {
            let mut resolved = Vec::new();
            for (_i, stmt) in else_statements.iter().enumerate() {
                let resolved_stmt = self.resolve_statement(stmt)?;
                /*
                if i == else_statements.len() - 1 {
                    if let ResolvedStatement::Expression(expr) = resolved_stmt {
                        resolved_stmt = ResolvedStatement::Expression(ResolvedExpression::Option(
                            Some(Box::new(expr)),
                        ));
                    }
                }*/
                resolved.push(resolved_stmt);
            }
            Some(resolved)
        } else {
            None
        };

        Ok(ResolvedStatement::If(
            condition,
            resolved_statements,
            else_statements,
        ))
    }

    pub fn resolve_type(&mut self, ast_type: &Type) -> Result<ResolvedType, ResolveError> {
        let resolved = match ast_type {
            Type::Any(_) => ResolvedType::Any,
            Type::Int(_) => ResolvedType::Int(self.shared.types.int_type.clone()),
            Type::Float(_) => ResolvedType::Float(self.shared.types.float_type.clone()),
            Type::String(_) => ResolvedType::String(self.shared.types.string_type.clone()),
            Type::Bool(_) => ResolvedType::Bool(self.shared.types.bool_type.clone()),
            Type::Unit(_) => ResolvedType::Unit(self.shared.types.unit_type.clone()),
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
            Type::Optional(inner_type_ast, _node) => {
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

    fn get_text(&self, ast_node: &Node) -> &str {
        let span = Span {
            file_id: self.shared.file_id,
            offset: ast_node.span.offset,
            length: ast_node.span.length,
        };
        self.shared.source_map.get_span_source(
            self.shared.file_id,
            span.offset as usize,
            span.length as usize,
        )
    }

    fn get_text_resolved(&self, resolved_node: &ResolvedNode) -> &str {
        let span = Span {
            file_id: self.shared.file_id,
            offset: resolved_node.span.offset,
            length: resolved_node.span.length,
        };
        self.shared.source_map.get_span_source(
            self.shared.file_id,
            span.offset as usize,
            span.length as usize,
        )
    }

    fn get_path(&self, ident: &QualifiedTypeIdentifier) -> (Vec<String>, String) {
        let path = ident
            .module_path
            .as_ref()
            .map_or_else(Vec::new, |found_path| {
                let mut v = Vec::new();
                for p in &found_path.0 {
                    v.push(self.get_text(&p.node).to_string());
                }
                v
            });
        (path, self.get_text(&ident.name.0).to_string())
    }

    fn find_type_reference(
        &self,
        type_name_to_find: &QualifiedTypeIdentifier,
    ) -> Result<ResolvedType, ResolveError> {
        let (path, text) = self.get_path(type_name_to_find);

        let resolved_type =
            if let Some(aliased_type) = self.shared.lookup.get_type_alias(&path, &text) {
                aliased_type
            } else if let Some(found) = self.shared.lookup.get_struct(&path, &text) {
                ResolvedType::Struct(found)
            } else if let Some(found) = self.shared.lookup.get_enum(&path, &text) {
                ResolvedType::Enum(found)
            } else if let Some(found) = self.shared.lookup.get_rust_type(&path, &text) {
                ResolvedType::RustType(found)
            } else {
                Err(ResolveError::UnknownTypeReference(
                    self.to_node(&type_name_to_find.name.0),
                ))?
            };

        Ok(resolved_type)
    }

    pub fn find_struct_type(
        &self,
        type_name: &QualifiedTypeIdentifier,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        let (path, name_string) = self.get_path(type_name);
        if let Some(aliased_type) = self.shared.lookup.get_type_alias(&path, &name_string) {
            let unaliased = unalias_type(aliased_type);
            return match unaliased {
                ResolvedType::Struct(struct_ref) => Ok(struct_ref),
                _ => Err(ResolveError::TypeAliasNotAStruct(type_name.clone())),
            };
        }

        self.shared
            .lookup
            .get_struct(&path, &name_string)
            .map_or_else(
                || {
                    let resolved_node = self.to_node(&type_name.name.0);
                    Err(ResolveError::UnknownStructTypeReference(resolved_node))
                },
                Ok,
            )
    }

    pub fn find_struct_type_local_mut(
        &self,
        type_name: &Node,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        self.find_struct_type(&QualifiedTypeIdentifier::new(
            LocalTypeIdentifier(type_name.clone()),
            vec![],
        ))
    }

    pub fn resolve_struct_type_definition(
        &mut self,
        ast_struct: &StructType,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        let mut resolved_fields = SeqMap::new();

        for field_name_and_type in &ast_struct.fields {
            let resolved_type = self.resolve_type(&field_name_and_type.field_type)?;
            let name_string = self.get_text(&field_name_and_type.field_name.0).to_string();

            let field_type = ResolvedAnonymousStructFieldType {
                identifier: ResolvedFieldName(self.to_node(&field_name_and_type.field_name.0)),
                field_type: resolved_type,
                index: 0,
            };

            resolved_fields
                .insert(name_string, field_type)
                .map_err(|_| {
                    ResolveError::DuplicateFieldName(
                        self.to_node(&field_name_and_type.field_name.0),
                    )
                })?;
        }

        let resolved_anon_struct = ResolvedAnonymousStructType {
            defined_fields: resolved_fields,
        };

        let struct_name_str = self.get_text(&ast_struct.identifier.0).to_string();

        let resolved_struct = ResolvedStructType::new(
            self.to_node(&ast_struct.identifier.0),
            &struct_name_str,
            resolved_anon_struct,
            self.shared.state.allocate_number(),
        );

        let resolved_struct_ref = self
            .shared
            .lookup
            .add_struct(&struct_name_str, resolved_struct)?;

        Ok(resolved_struct_ref)
    }

    fn resolve_enum_type_definition(
        &mut self,
        enum_type_name: &Node,
        ast_variants: &Vec<EnumVariantType>,
    ) -> Result<(ResolvedEnumTypeRef, Vec<ResolvedEnumVariantTypeRef>), ResolveError> {
        let mut resolved_variants = Vec::new();

        let parent_number = self.shared.state.allocate_number();

        let enum_parent = ResolvedEnumType {
            name: ResolvedLocalTypeIdentifier(self.to_node(enum_type_name)),
            assigned_name: self.get_text(&enum_type_name).to_string(),
            module_path: self.shared.lookup.get_path().to_vec(),
            number: parent_number,
        };

        let enum_type_str = self.get_text(enum_type_name).to_string();
        let parent_ref = self
            .shared
            .lookup
            .add_enum_type(&enum_type_str, enum_parent)?;

        for variant_type in ast_variants {
            let mut container_number: Option<TypeNumber> = None;

            let variant_name_node = match variant_type {
                EnumVariantType::Simple(name) => name,
                EnumVariantType::Tuple(name, _) => name,
                EnumVariantType::Struct(name, _) => name,
            };

            let container = match variant_type {
                EnumVariantType::Simple(_variant_name_node) => {
                    ResolvedEnumVariantContainerType::Nothing
                }
                EnumVariantType::Tuple(_variant_name_node, types) => {
                    let mut vec = Vec::new();
                    for tuple_type in types {
                        let resolved_type = self.resolve_type(tuple_type)?;
                        vec.push(resolved_type);
                    }

                    let number = self.shared.state.allocate_number();
                    container_number = Some(number);

                    let common = CommonEnumVariantType {
                        number,
                        module_path: ResolvedModulePath(vec![]), // TODO:
                        variant_name: ResolvedLocalTypeIdentifier(self.to_node(variant_name_node)),
                        assigned_name: self.get_text(&variant_name_node).to_string(),
                        enum_ref: parent_ref.clone(),
                    };

                    let resolved_tuple_type = ResolvedEnumVariantTupleType {
                        common,
                        fields_in_order: vec,
                    };
                    let resolved_tuple_type_ref = Rc::new(resolved_tuple_type);

                    ResolvedEnumVariantContainerType::Tuple(resolved_tuple_type_ref)
                }
                EnumVariantType::Struct(_variant_name_node, ast_struct_fields) => {
                    let mut fields = SeqMap::new();

                    for (index, field_with_type) in ast_struct_fields.fields.iter().enumerate() {
                        let resolved_type = self.resolve_type(&field_with_type.field_type)?;
                        let field_name_str =
                            self.get_text(&field_with_type.field_name.0).to_string();

                        let resolved_field = ResolvedAnonymousStructFieldType {
                            identifier: ResolvedFieldName(
                                self.to_node(&field_with_type.field_name.0),
                            ),
                            field_type: resolved_type,
                            index,
                        };

                        fields.insert(field_name_str, resolved_field).map_err(|_| {
                            ResolveError::DuplicateFieldName(
                                self.to_node(&field_with_type.field_name.0),
                            )
                        })?;
                    }

                    let number = self.shared.state.allocate_number();
                    container_number = Some(number);

                    let common = CommonEnumVariantType {
                        number,
                        module_path: ResolvedModulePath(vec![]), // TODO:
                        variant_name: ResolvedLocalTypeIdentifier(self.to_node(variant_name_node)),
                        assigned_name: self.get_text(&variant_name_node).to_string(),
                        enum_ref: parent_ref.clone(),
                    };

                    let enum_variant_struct_type = ResolvedEnumVariantStructType {
                        common,
                        anon_struct: ResolvedAnonymousStructType {
                            defined_fields: fields,
                        },
                    };

                    let enum_variant_struct_type_ref = Rc::new(enum_variant_struct_type);

                    ResolvedEnumVariantContainerType::Struct(enum_variant_struct_type_ref)
                }
            };

            let variant_name_str = self.get_text(variant_name_node).to_string();

            let variant_type = ResolvedEnumVariantType {
                owner: parent_ref.clone(),
                data: container,
                name: ResolvedLocalTypeIdentifier(self.to_node(variant_name_node)),
                assigned_name: self.get_text(&variant_name_node).to_string(),
                number: container_number.unwrap_or(0),
            };

            let variant_type_ref = self.shared.lookup.add_enum_variant(
                &enum_type_str,
                &variant_name_str,
                variant_type,
            )?;

            resolved_variants.push(variant_type_ref);
        }

        Ok((parent_ref, resolved_variants))
    }

    fn resolve_return_type(&mut self, function: &Function) -> Result<ResolvedType, ResolveError> {
        let ast_return_type = match function {
            Function::Internal(x) => &x.declaration.return_type,
            Function::External(x) => &x.return_type,
        };

        let resolved_return_type = match ast_return_type {
            None => self.shared.types.unit_type(),
            Some(x) => self.resolve_type(x)?,
        };

        Ok(resolved_return_type)
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
            Definition::FunctionDef(function) => {
                let resolved_return_type = self.resolve_return_type(function)?;
                self.start_function(resolved_return_type);
                let resolved_def = self.resolve_function_definition(function)?;
                self.stop_function();
                resolved_def
            }
            Definition::ImplDef(type_identifier, functions) => {
                let attached_type_type =
                    self.resolve_impl_definition(type_identifier, functions)?;
                ResolvedDefinition::ImplType(attached_type_type)
            }
            Definition::TypeAlias(identifier, target_type) => ResolvedDefinition::Alias(
                self.resolve_type_alias_definition(identifier, target_type)?,
            ),
            Definition::Comment(comment_ref) => {
                ResolvedDefinition::Comment(self.to_node(comment_ref))
            }
            Definition::Import(_) => todo!(), // TODO: Implement import resolution
        };

        Ok(resolved_def)
    }

    fn resolve_statements_in_function(
        &mut self,
        statements: &[Statement],
        return_type: &ResolvedType,
    ) -> Result<Vec<ResolvedStatement>, ResolveError> {
        let mut resolved_statements = Vec::new();

        for (i, statement) in statements.iter().enumerate() {
            let mut resolved_statement = self.resolve_statement(statement)?;

            // Handle last statement in function
            if i == statements.len() - 1 {
                if let ResolvedStatement::Expression(expr) = resolved_statement {
                    let wrapped_expr = Self::check_and_wrap_return_value(expr, return_type)?;
                    resolved_statement = ResolvedStatement::Expression(wrapped_expr);
                }
            }

            resolved_statements.push(resolved_statement);
        }

        Ok(resolved_statements)
    }

    fn resolve_function_definition(
        &mut self,
        function: &Function,
    ) -> Result<ResolvedDefinition, ResolveError> {
        let resolved_function = match function {
            Function::Internal(function_data) => {
                let parameters = self.resolve_parameters(&function_data.declaration.params)?;
                let return_type = if let Some(found) = &function_data.declaration.return_type {
                    self.resolve_type(found)?
                } else {
                    self.shared.types.unit_type()
                };

                self.scope.return_type = return_type.clone();

                // Set up scope for function body
                for param in &parameters {
                    self.create_local_variable_resolved(
                        &param.name,
                        &param.is_mutable,
                        &param.resolved_type.clone(),
                    )?;
                }

                let statements =
                    self.resolve_statements_in_function(&function_data.body, &return_type)?;

                self.scope.return_type = self.shared.types.unit_type();

                let internal = ResolvedInternalFunctionDefinition {
                    signature: ResolvedFunctionSignature {
                        first_parameter_is_self: function_data.declaration.self_parameter.is_some(),
                        parameters,
                        return_type,
                    },
                    statements,
                    name: ResolvedLocalIdentifier(self.to_node(&function_data.declaration.name)),
                };

                let function_name = self.get_text(&function_data.declaration.name).to_string();
                let function_ref = self
                    .shared
                    .lookup
                    .add_internal_function_ref(&function_name, internal)?;
                ResolvedFunction::Internal(function_ref)
            }
            Function::External(ast_signature) => {
                let parameters = self.resolve_parameters(&ast_signature.params)?;
                let external_return_type = if let Some(found) = &ast_signature.return_type {
                    self.resolve_type(found)?
                } else {
                    self.shared.types.unit_type()
                };

                let return_type = external_return_type;
                let external_function_id = self.shared.state.allocate_external_function_id();

                let external = ResolvedExternalFunctionDefinition {
                    signature: ResolvedFunctionSignature {
                        first_parameter_is_self: ast_signature.self_parameter.is_some(),
                        parameters,
                        return_type,
                    },
                    name: self.to_node(&ast_signature.name),
                    id: external_function_id,
                };

                ResolvedFunction::External(Rc::new(external))
            }
        };

        Ok(ResolvedDefinition::FunctionDef(resolved_function))
    }

    fn resolve_maybe_type(
        &mut self,
        maybe_type: &Option<Type>,
    ) -> Result<ResolvedType, ResolveError> {
        let found_type = match maybe_type {
            None => self.shared.types.unit_type(),
            Some(ast_type) => self.resolve_type(ast_type)?,
        };
        Ok(found_type)
    }

    fn resolve_impl_func(
        &mut self,
        function: &Function,
        found_struct: &ResolvedStructTypeRef,
    ) -> Result<ResolvedFunction, ResolveError> {
        let resolved_fn = match function {
            Function::Internal(function_data) => {
                let mut parameters = Vec::new();

                if let Some(found_self) = &function_data.declaration.self_parameter {
                    let resolved_type = ResolvedType::Struct(found_struct.clone());
                    parameters.push(ResolvedParameter {
                        name: self.to_node(&found_self.self_node),
                        resolved_type,
                        is_mutable: self.to_node_option(&found_self.is_mutable),
                    });
                }

                for param in &function_data.declaration.params {
                    let resolved_type = self.resolve_type(&param.param_type)?;

                    parameters.push(ResolvedParameter {
                        name: self.to_node(&param.variable.name),
                        resolved_type,
                        is_mutable: self.to_node_option(&param.variable.is_mutable),
                    });
                }

                let return_type =
                    self.resolve_maybe_type(&function_data.declaration.return_type)?;

                for param in &parameters {
                    self.create_local_variable_resolved(
                        &param.name,
                        &param.is_mutable,
                        &param.resolved_type,
                    )?;
                }

                let statements = self.resolve_statements(&function_data.body)?;

                let internal = ResolvedInternalFunctionDefinition {
                    signature: ResolvedFunctionSignature {
                        first_parameter_is_self: function_data.declaration.self_parameter.is_some(),
                        parameters,
                        return_type,
                    },
                    statements,
                    name: ResolvedLocalIdentifier(self.to_node(&function_data.declaration.name)),
                };

                let internal_ref = Rc::new(internal);

                ResolvedFunction::Internal(internal_ref)
            }
            Function::External(signature) => {
                let mut parameters = Vec::new();

                if let Some(found_self) = &signature.self_parameter {
                    let resolved_type = ResolvedType::Struct(found_struct.clone());
                    parameters.push(ResolvedParameter {
                        name: self.to_node(&found_self.self_node),
                        resolved_type,
                        is_mutable: self.to_node_option(&found_self.is_mutable),
                    });
                }

                // Handle parameters, including self if present
                for param in &signature.params {
                    let resolved_type = self.resolve_type(&param.param_type)?;

                    parameters.push(ResolvedParameter {
                        name: self.to_node(&param.variable.name),
                        resolved_type,
                        is_mutable: self.to_node_option(&param.variable.is_mutable),
                    });
                }

                let return_type = self.resolve_maybe_type(&signature.return_type)?;

                let external = ResolvedExternalFunctionDefinition {
                    name: self.to_node(&signature.name),
                    signature: ResolvedFunctionSignature {
                        first_parameter_is_self: signature.self_parameter.is_some(),
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
        name: &Node,
        target_type: &Type,
    ) -> Result<ResolvedType, ResolveError> {
        let resolved_type = self.resolve_type(target_type)?;
        let alias_type =
            ResolvedType::Alias(LocalTypeName(self.to_node(name)), Box::new(resolved_type));

        Ok(alias_type)
    }

    fn resolve_impl_definition(
        &mut self,
        attached_to_type: &Node,
        functions: &Vec<Function>,
    ) -> Result<ResolvedType, ResolveError> {
        let found_struct = self.find_struct_type_local_mut(attached_to_type)?;

        for function in functions {
            let new_return_type = self.resolve_return_type(function)?;
            self.start_function(new_return_type);

            let function_name = match function {
                Function::Internal(function_with_body) => &function_with_body.declaration,
                Function::External(external_declaration) => external_declaration,
            };

            let function_name_str = self.get_text(&function_name.name).to_string();

            let resolved_function = self.resolve_impl_func(function, &found_struct)?;
            let resolved_function_ref = Rc::new(resolved_function);

            found_struct
                .borrow_mut()
                .functions
                .insert(function_name_str, resolved_function_ref)?;
            self.stop_function();
        }

        Ok(ResolvedType::Struct(found_struct))
    }

    fn resolve_for_pattern(
        &mut self,
        pattern: &ForPattern,
        key_type: Option<&ResolvedType>,
        value_type: &ResolvedType,
    ) -> Result<ResolvedForPattern, ResolveError> {
        match pattern {
            ForPattern::Single(var) => {
                let variable_ref =
                    self.create_local_variable(&var.identifier, &var.is_mut, value_type)?;
                Ok(ResolvedForPattern::Single(variable_ref))
            }
            ForPattern::Pair(first, second) => {
                let found_key = key_type.unwrap();
                let first_var_ref =
                    self.create_local_variable(&first.identifier, &first.is_mut, found_key)?;
                let second_var_ref =
                    self.create_local_variable(&second.identifier, &second.is_mut, value_type)?;
                Ok(ResolvedForPattern::Pair(first_var_ref, second_var_ref))
            }
        }
    }

    fn check_and_wrap_return_value(
        expr: ResolvedExpression,
        return_type: &ResolvedType,
    ) -> Result<ResolvedExpression, ResolveError> {
        let expr_type = resolution(&expr);

        // If return type is Unit, ignore the expression's type and return Unit
        // TODO: In future versions, always have a return statement
        if let ResolvedType::Unit(_) = return_type {
            return Ok(expr);
        }

        if let ResolvedType::Optional(inner_type) = return_type {
            if return_type.same_type(&expr_type) {
                return Ok(expr);
            }

            if inner_type.same_type(&expr_type) {
                return Ok(ResolvedExpression::Option(Some(Box::new(expr))));
            }

            return Err(ResolveError::IncompatibleTypes(
                expr.span(),
                return_type.clone(),
            ));
        }

        if !return_type.same_type(&expr_type) {
            return Err(ResolveError::IncompatibleTypes(
                expr.span(),
                return_type.clone(),
            ));
        }
        Ok(expr)
    }

    fn current_function_return_type(&self) -> ResolvedType {
        self.scope.return_type.clone()
    }

    pub fn resolve_statement(
        &mut self,
        statement: &Statement,
    ) -> Result<ResolvedStatement, ResolveError> {
        let converted = match statement {
            Statement::ForLoop(pattern, iteratable_expression, statements) => {
                let resolved_iterator = self.resolve_iterator(
                    &iteratable_expression.expression,
                    &iteratable_expression.is_mut,
                )?;

                self.push_block_scope("for_loop");
                let pattern = self.resolve_for_pattern(
                    pattern,
                    resolved_iterator.key_type.as_ref(),
                    &resolved_iterator.value_type,
                )?;
                let resolved_statements = self.resolve_statements(statements)?;
                self.pop_block_scope("for_loop");

                ResolvedStatement::ForLoop(pattern, resolved_iterator, resolved_statements)
            }
            Statement::WhileLoop(expression, statements) => {
                let condition = self.resolve_bool_expression(expression)?;
                self.push_block_scope("while_loop");
                let resolved_statements = self.resolve_statements(statements)?;
                self.pop_block_scope("while_loop");

                ResolvedStatement::WhileLoop(condition, resolved_statements)
            }
            Statement::Return(expr) => {
                let resolved_expr = self.resolve_expression(expr)?;
                let return_type = self.current_function_return_type();
                let wrapped_expr = Self::check_and_wrap_return_value(resolved_expr, &return_type)?;
                ResolvedStatement::Return(wrapped_expr)
            }
            Statement::Break(node) => ResolvedStatement::Break(self.to_node(node)),
            Statement::Continue(node) => ResolvedStatement::Continue(self.to_node(node)),
            Statement::Expression(expression) => {
                ResolvedStatement::Expression(self.resolve_expression(expression)?)
            }
            Statement::Block(statements) => {
                ResolvedStatement::Block(self.resolve_statements(statements)?)
            }
            Statement::If(expression, statements, maybe_else_statements) => match expression {
                Expression::PostfixOp(PostfixOperator::Unwrap(_unwrap_node), expr) => {
                    if let Expression::VariableAccess(var) = &**expr {
                        self.handle_optional_unwrap_statement(
                            var,
                            expr,
                            statements,
                            maybe_else_statements,
                        )?
                    } else {
                        Err(ResolveError::ExpectedVariable)?
                    }
                }
                Expression::VariableAssignment(var, expr) => {
                    if let Expression::PostfixOp(
                        PostfixOperator::Unwrap(_unwrap_node),
                        inner_expr,
                    ) = &**expr
                    {
                        self.handle_optional_assign_unwrap_statement(
                            var,
                            inner_expr,
                            statements,
                            maybe_else_statements,
                        )?
                    } else {
                        self.resolve_normal_if_statement(
                            expression,
                            statements,
                            maybe_else_statements,
                        )?
                    }
                }
                _ => {
                    self.resolve_normal_if_statement(expression, statements, maybe_else_statements)?
                }
            },
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
            let debug_text = self.get_text(&parameter.variable.name);
            let param_type = self.resolve_type(&parameter.param_type)?;
            resolved_parameters.push(ResolvedParameter {
                name: self.to_node(&parameter.variable.name),
                resolved_type: param_type,
                is_mutable: self.to_node_option(&parameter.variable.is_mutable),
            });
        }
        Ok(resolved_parameters)
    }

    fn get_field_index(
        &self,
        base_type: &ResolvedType,
        name: &Node,
    ) -> Result<(ResolvedType, usize), ResolveError> {
        if let ResolvedType::Struct(struct_type) = base_type {
            let fields = &struct_type.borrow().anon_struct_type.defined_fields;
            let field_name = self.get_text(name).to_string();
            fields.get_index(&field_name).map_or_else(
                || Err(ResolveError::UnknownStructField(self.to_node(name))),
                |field_index| {
                    let field = fields.get(&field_name).expect("should find it again");
                    Ok((field.field_type.clone(), field_index))
                },
            )
        } else {
            Err(ResolveError::NeedStructForFieldLookup)
        }
    }

    fn collect_field_chain(
        &mut self,
        expr: &Expression,
        access_chain: &mut Vec<ResolvedAccess>,
    ) -> Result<(ResolvedType, ResolvedExpression), ResolveError> {
        match expr {
            Expression::FieldAccess(source, field) => {
                let (resolved_type, base_expr) = self.collect_field_chain(source, access_chain)?;

                let (field_type, field_index) = self.get_field_index(&resolved_type, field)?;
                access_chain.push(ResolvedAccess::FieldIndex(self.to_node(field), field_index));

                Ok((field_type, base_expr))
            }

            Expression::IndexAccess(source, index_expr) => {
                let (resolved_type, base_expr) = self.collect_field_chain(source, access_chain)?;
                let resolved_index_expr = self.resolve_expression(index_expr)?;
                match resolved_type {
                    ResolvedType::Array(array_type_ref) => {
                        access_chain.push(ResolvedAccess::ArrayIndex(resolved_index_expr));
                        Ok((array_type_ref.item_type.clone(), base_expr))
                    }
                    ResolvedType::Map(map_type_ref) => {
                        access_chain.push(ResolvedAccess::MapIndex(resolved_index_expr));
                        Ok((map_type_ref.value_type.clone(), base_expr))
                    }
                    _ => Err(ResolveError::ExpectedArray(resolved_type)),
                }
            }
            _ => {
                let resolved_expr = self.resolve_expression(expr)?;
                let resolved_type = resolution(&resolved_expr);
                Ok((resolved_type, resolved_expr))
            }
        }
    }

    fn resolve_expression(
        &mut self,
        ast_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let expression = match ast_expression {
            // Lookups
            Expression::FieldAccess(expression, field_name) => {
                let struct_field_ref =
                    self.resolve_into_struct_field_ref(expression.as_ref(), field_name)?;

                self.resolve_field_access(expression, &struct_field_ref, field_name)?
            }
            Expression::VariableAccess(variable) => {
                self.resolve_variable_or_function_access(&variable.name)?
            }
            Expression::MutRef(variable) => self.resolve_mut_ref(variable)?,
            Expression::IndexAccess(collection_expression, lookup) => {
                let resolved_collection_expression =
                    self.resolve_expression(collection_expression)?;
                let collection_resolution = resolution(&resolved_collection_expression);
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
                                map_type_ref.value_type.clone(),
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
                    self.resolve_expression(collection_expression)?;
                debug!(resolved_collection_expression=?resolved_collection_expression, "resolve_collection_access");

                let collection_resolution = resolution(&resolved_collection_expression);
                debug!(collection_resolution=?collection_resolution, "collection_resolution");

                let resolved_index_expression = self.resolve_expression(index_expression)?;
                debug!(resolved_index_expression=?resolved_index_expression, "resolve_index_access");

                let index_resolution = resolution(&resolved_index_expression);
                debug!(index_resolution=?index_resolution, "index_resolution");

                let resolved_source_expression = self.resolve_expression(source_expr)?;
                let resolved_source_expression_type = resolution(&resolved_source_expression);

                info!("COLLECTION: {resolved_collection_expression:?}");

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
                self.resolve_variable_assignment(variable_expression, source_expression)?
            }

            Expression::MultiVariableAssignment(variables, source_expression) => {
                self.resolve_multi_variable_assignment(variables, source_expression)?
            }

            Expression::IndexCompoundAssignment(_target, _index, _operator, _source) => {
                // self.resolve_compound_assignment_index(source, index, operator, source)?
                todo!()
            }

            Expression::VariableCompoundAssignment(target, operator, source) => {
                self.resolve_compound_assignment_variable(target, operator, source)?
            }

            Expression::FieldCompoundAssignment(target, field, operator, source) => {
                self.resolve_field_assignment_compound(target, field, operator, source)?
            }

            Expression::FieldAssignment(ast_struct_expr, ast_field_name, ast_expression) => {
                self.resolve_field_assignment(ast_struct_expr, ast_field_name, ast_expression)?
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
                if let Some(found) = self.check_for_internal_static_call(
                    type_name,
                    function_name,
                    generic_types,
                    arguments,
                )? {
                    found
                } else {
                    ResolvedExpression::StaticCallGeneric(self.resolve_static_call_generic(
                        type_name,
                        function_name,
                        generic_types,
                        arguments,
                    )?)
                }
            }

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
                self.shared.types.string_type.clone(),
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
                    self.shared.types.exclusive_range_type.clone(),
                    Box::from(min_expression),
                    Box::from(max_expression),
                )
            }

            Expression::Literal(literal) => {
                ResolvedExpression::Literal(self.resolve_literal(literal)?)
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

    fn resolve_literal(&mut self, ast_literal: &Literal) -> Result<ResolvedLiteral, ResolveError> {
        let resolved_literal = match ast_literal {
            Literal::Int(int_node) => {
                let integer_text = self.get_text(int_node);
                ResolvedLiteral::IntLiteral(
                    Self::str_to_int(integer_text).map_err(ResolveError::IntConversionError)?,
                    self.to_node(int_node),
                    self.shared.types.int_type.clone(),
                )
            }
            Literal::Float(float_node) => {
                let float_str = self.get_text(float_node);
                let float =
                    Self::str_to_float(float_str).map_err(ResolveError::FloatConversionError)?;
                ResolvedLiteral::FloatLiteral(
                    Fp::from(float),
                    self.to_node(float_node),
                    self.shared.types.float_type.clone(),
                )
            }
            Literal::String(string_node) => {
                let string_str = self.get_text(string_node);
                ResolvedLiteral::StringLiteral(
                    string_str[1..string_str.len() - 1].to_string(), // remove prefix and suffix quotes
                    self.to_node(string_node),
                    self.shared.types.string_type.clone(),
                )
            }
            Literal::Bool(bool_node) => {
                let bool_str = self.get_text(bool_node);
                let bool_val = if bool_str == "false" {
                    false
                } else if bool_str == "true" {
                    true
                } else {
                    return Err(ResolveError::BoolConversionError);
                };
                ResolvedLiteral::BoolLiteral(
                    bool_val,
                    self.to_node(bool_node),
                    self.shared.types.bool_type.clone(),
                )
            }
            Literal::EnumVariant(ref enum_literal) => {
                let (enum_name, variant_name) = match enum_literal {
                    EnumVariantLiteral::Simple(enum_name, variant_name) => {
                        (enum_name, variant_name)
                    }
                    EnumVariantLiteral::Tuple(enum_name, variant_name, _) => {
                        (enum_name, variant_name)
                    }
                    EnumVariantLiteral::Struct(enum_name, variant_name, _) => {
                        (enum_name, variant_name)
                    }
                };

                // Handle enum variant literals in patterns
                let variant_ref = self
                    .resolve_enum_variant_ref(enum_name, variant_name)
                    .expect("enum variant should exist");

                let resolved_data = match enum_literal {
                    EnumVariantLiteral::Simple(_, _) => ResolvedEnumLiteralData::Nothing,
                    EnumVariantLiteral::Tuple(_node, _variant, expressions) => {
                        let resolved = self
                            .resolve_expressions(expressions)
                            .expect("enum tuple expressions should resolve");
                        ResolvedEnumLiteralData::Tuple(resolved)
                    }
                    EnumVariantLiteral::Struct(
                        _node,
                        _variant,
                        anonym_struct_field_and_expressions,
                    ) => {
                        let mut resolved = Vec::new();
                        for field_and_expr in anonym_struct_field_and_expressions {
                            let resolved_expression = self
                                .resolve_expression(&field_and_expr.expression)
                                .expect("enum struct expressions should resolve");
                            resolved.push(resolved_expression);
                        }
                        ResolvedEnumLiteralData::Struct(resolved)
                    }
                };

                ResolvedLiteral::EnumVariantLiteral(variant_ref, resolved_data)
            }

            Literal::Array(items) => {
                let (array_type_ref, resolved_items) = self.resolve_array_type_helper(items)?;
                ResolvedLiteral::Array(array_type_ref, resolved_items)
            }

            Literal::Map(entries) => self.resolve_map_literal(entries)?,

            Literal::Tuple(expressions) => {
                let (tuple_type_ref, resolved_items) = self.resolve_tuple_literal(expressions)?;
                ResolvedLiteral::TupleLiteral(tuple_type_ref, resolved_items)
            }
            Literal::Unit => ResolvedLiteral::UnitLiteral(self.shared.types.unit_type.clone()),
            Literal::None(none_node) => ResolvedLiteral::NoneLiteral(self.to_node(none_node)),
        };

        Ok(resolved_literal)
    }

    fn resolve_into_struct_field_ref(
        &mut self,
        struct_expression: &Expression,
        name: &Node,
    ) -> Result<ResolvedStructTypeFieldRef, ResolveError> {
        let resolved_expr = self.resolve_expression(struct_expression)?;
        let resolved_type = resolution(&resolved_expr);
        let ResolvedType::Struct(struct_ref) = &resolved_type else {
            warn!(found_type=?resolved_type, expr=?resolved_expr, "NOT STRUCT TYPE");
            return Err(ResolveError::WasNotStructType(self.to_node(name)));
        };

        let borrowed_struct = struct_ref.borrow();

        let field_name = self.get_text(name).to_string();

        if let Some(field_index) = borrowed_struct
            .anon_struct_type
            .defined_fields
            .get_index(&field_name)
        {
            let field_resolved_type = borrowed_struct
                .anon_struct_type
                .defined_fields
                .get(&field_name)
                .expect("checked earlier");

            let field = ResolvedStructTypeField {
                struct_type_ref: struct_ref.clone(),
                index: field_index,
                resolved_type: field_resolved_type.field_type.clone(),
                field_name: ResolvedLocalIdentifier(self.to_node(name)),
                //struct_expression: Box::from(resolved_expr),
            };

            let field_ref = Rc::new(field);

            Ok(field_ref)
        } else {
            Err(ResolveError::UnknownStructField(self.to_node(name)))
        }
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
            _ => Err(ResolveError::NotNamedStruct(resolved_type.clone())),
        }
    }

    fn resolve_into_member_function(
        &mut self,
        expression: &Expression,
        ast_member_function_name: &Node,
    ) -> Result<(ResolvedFunctionRef, ResolvedExpression), ResolveError> {
        let resolved = self.resolve_expression(expression)?;

        let resolved_type = resolution(&resolved);
        match resolved_type {
            ResolvedType::Struct(resolved_struct_type_ref) => {
                let struct_ref = resolved_struct_type_ref.borrow();

                let ast_member_function_name_str =
                    self.get_text(ast_member_function_name).to_string();

                struct_ref
                    .functions
                    .get(&ast_member_function_name_str)
                    .map_or_else(
                        || {
                            Err(ResolveError::UnknownMemberFunction(
                                self.to_node(ast_member_function_name),
                            ))
                        },
                        |function_ref| Ok((function_ref.clone(), resolved)),
                    )
            }
            _ => Err(ResolveError::NotNamedStruct(resolved_type)),
        }
    }

    fn get_struct_types(
        &self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
    ) -> Result<(ResolvedType, ResolvedStructTypeRef), ResolveError> {
        //   let namespace = self.get_namespace(qualified_type_identifier)?;

        let (path, name) = self.get_path(qualified_type_identifier);
        // If it's an alias, return both the alias and the underlying struct
        if let Some(alias_type) = self.shared.lookup.get_type_alias(&path, &name) {
            let unaliased = unalias_type(alias_type.clone());
            match unaliased {
                ResolvedType::Struct(struct_ref) => Ok((alias_type, struct_ref)),
                _ => Err(ResolveError::TypeAliasNotAStruct(
                    qualified_type_identifier.clone(),
                )),
            }
        } else {
            // If direct struct, return the struct type for both
            let struct_ref = self.shared.lookup.get_struct(&path, &name).ok_or_else(|| {
                let type_reference_node = self.to_node(&qualified_type_identifier.name.0);
                ResolveError::UnknownStructTypeReference(type_reference_node)
            })?;
            Ok((ResolvedType::Struct(struct_ref.clone()), struct_ref))
        }
    }

    fn resolve_struct_instantiation(
        &mut self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
        ast_fields: &Vec<FieldExpression>,
    ) -> Result<ResolvedStructInstantiation, ResolveError> {
        let (display_type_ref, struct_to_instantiate) =
            self.get_struct_types(qualified_type_identifier)?;

        if ast_fields.len()
            != struct_to_instantiate
                .borrow()
                .anon_struct_type
                .defined_fields
                .len()
        {
            return Err(ResolveError::WrongFieldCountInStructInstantiation(
                struct_to_instantiate,
                ast_fields.len(),
            ));
        }

        let mut seen_fields = HashSet::new();
        let mut source_order_expressions = Vec::new();

        for field in ast_fields {
            let field_name = self.get_text(&field.field_name.0).to_string();

            // If we can not insert it, it is a duplicate
            if !seen_fields.insert(field_name.clone()) {
                return Err(ResolveError::DuplicateFieldInStructInstantiation(
                    field_name,
                    struct_to_instantiate,
                ));
            }

            let borrowed_struct = struct_to_instantiate.borrow();

            let maybe_looked_up_field = borrowed_struct
                .anon_struct_type
                .defined_fields
                .get(&field_name);

            if maybe_looked_up_field.is_none() {
                return Err(ResolveError::UnknownStructField(
                    self.to_node(&field.field_name.0),
                ));
            }

            let looked_up_field = maybe_looked_up_field.expect("checked earlier");

            let field_index_in_definition = struct_to_instantiate
                .borrow()
                .anon_struct_type
                .defined_fields
                .get_index(&field_name)
                .expect("field_name is checked earlier");

            let resolved_expression = self.resolve_expression(&field.expression)?;
            let upgraded_resolved_expression =
                wrap_in_some_if_optional(&looked_up_field.field_type, resolved_expression);

            let expression_type = resolution(&upgraded_resolved_expression);

            if !looked_up_field.field_type.same_type(&expression_type) {
                error!("types: {looked_up_field:?} expr: {expression_type:?}");
                return Err(ResolveError::ExpressionIsOfWrongFieldType);
            }

            source_order_expressions
                .push((field_index_in_definition, upgraded_resolved_expression));
        }

        Ok(ResolvedStructInstantiation {
            source_order_expressions,
            struct_type_ref: struct_to_instantiate,
            display_type_ref,
        })
    }

    fn resolve_bool_expression(
        &mut self,
        expression: &Expression,
    ) -> Result<ResolvedBooleanExpression, ResolveError> {
        let resolved_expression = self.resolve_expression(expression)?;
        let expr_type = resolution(&resolved_expression);

        let bool_expression = match expr_type {
            ResolvedType::Bool(_) => resolved_expression,
            ResolvedType::Optional(_) => {
                ResolvedExpression::CoerceOptionToBool(Box::new(resolved_expression))
            }
            _ => {
                return Err(ResolveError::ExpectedBooleanExpression(
                    resolved_expression.span(),
                ))
            }
        };

        Ok(ResolvedBooleanExpression {
            expression: bool_expression,
        })
    }

    fn find_variant_in_pattern(
        &self,
        expression_type: &ResolvedType,
        ast_name: &Node,
    ) -> Result<ResolvedEnumVariantTypeRef, ResolveError> {
        let enum_type_ref = match expression_type {
            ResolvedType::Enum(enum_type_ref) => enum_type_ref,
            _ => Err(ResolveError::ExpectedEnumInPattern(self.to_node(ast_name)))?,
        };

        let enum_name = &enum_type_ref.assigned_name;
        let variant_name = self.get_text(ast_name).to_string();

        self.shared
            .lookup
            .get_enum_variant_type(&enum_type_ref.module_path, &enum_name, &variant_name)
            .map_or_else(
                || {
                    Err(ResolveError::UnknownEnumVariantTypeInPattern(
                        self.to_node(ast_name),
                    ))
                },
                Ok,
            )
    }

    fn resolve_pattern(
        &mut self,
        ast_pattern: &Pattern,
        expression_type: &ResolvedType,
    ) -> Result<(ResolvedPattern, bool), ResolveError> {
        match ast_pattern {
            Pattern::PatternList(elements) => {
                let mut resolved_elements = Vec::new();
                let mut scope_is_pushed = false;
                for element in elements {
                    match element {
                        PatternElement::Variable(var) => {
                            if !scope_is_pushed {
                                self.push_block_scope("pattern_list one variable");
                                scope_is_pushed = true;
                            }
                            let variable_ref =
                                self.create_local_variable(var, &None, expression_type)?;
                            resolved_elements.push(ResolvedPatternElement::Variable(variable_ref));
                        }
                        PatternElement::Expression(_expr) => {
                            return Err(ResolveError::ExpressionsNotAllowedInLetPattern);
                        }
                        PatternElement::Wildcard(node) => {
                            resolved_elements
                                .push(ResolvedPatternElement::Wildcard(self.to_node(node)));
                        }
                    }
                }
                Ok((
                    ResolvedPattern::PatternList(resolved_elements),
                    scope_is_pushed,
                ))
            }

            Pattern::EnumPattern(variant_name, maybe_elements) => {
                let mut scope_was_pushed = false;
                let enum_variant_type_ref =
                    self.find_variant_in_pattern(expression_type, variant_name)?;

                if let Some(elements) = maybe_elements {
                    let mut resolved_elements = Vec::new();
                    match &enum_variant_type_ref.data {
                        ResolvedEnumVariantContainerType::Tuple(tuple_type) => {
                            // For tuples, elements must be in order but can be partial
                            if elements.len() > tuple_type.fields_in_order.len() {
                                return Err(ResolveError::TooManyTupleFields {
                                    max: tuple_type.fields_in_order.len(),
                                    got: elements.len(),
                                });
                            }

                            if !scope_was_pushed {
                                self.push_block_scope("enum tuple");
                                scope_was_pushed = true;
                            }

                            // Only zip with as many fields as we have elements
                            for (element, field_type) in
                                elements.iter().zip(&tuple_type.fields_in_order)
                            {
                                match element {
                                    PatternElement::Variable(var) => {
                                        info!(?var, "ENUM TUPLE found variable to handle");
                                        let variable_ref =
                                            self.create_local_variable(var, &None, field_type)?;
                                        resolved_elements
                                            .push(ResolvedPatternElement::Variable(variable_ref));
                                    }
                                    PatternElement::Wildcard(node) => {
                                        resolved_elements.push(ResolvedPatternElement::Wildcard(
                                            self.to_node(node),
                                        ));
                                    }
                                    PatternElement::Expression(_) => {
                                        return Err(
                                            ResolveError::ExpressionsNotAllowedInLetPattern,
                                        );
                                    }
                                }
                            }
                        }
                        ResolvedEnumVariantContainerType::Struct(struct_type) => {
                            if !scope_was_pushed {
                                self.push_block_scope("enum struct");
                                scope_was_pushed = true;
                            }
                            // For structs, can match any subset of fields in any order
                            for element in elements {
                                match element {
                                    PatternElement::Variable(var) => {
                                        let var_name_str = self.get_text(var).to_string();
                                        // Check if the field exists
                                        let field_index = struct_type
                                            .anon_struct
                                            .defined_fields
                                            .get_index(&var_name_str)
                                            .ok_or_else(|| {
                                                ResolveError::UnknownField(self.to_node(var))
                                            })?;

                                        let field_type = struct_type
                                            .anon_struct
                                            .defined_fields
                                            .get(&var_name_str)
                                            .ok_or_else(|| {
                                                ResolveError::UnknownField(self.to_node(var))
                                            })?;

                                        let variable_ref = self.create_local_variable(
                                            var,
                                            &None,
                                            &field_type.field_type,
                                        )?;

                                        resolved_elements.push(
                                            ResolvedPatternElement::VariableWithFieldIndex(
                                                variable_ref,
                                                field_index,
                                            ),
                                        );
                                    }
                                    PatternElement::Wildcard(node) => {
                                        resolved_elements.push(ResolvedPatternElement::Wildcard(
                                            self.to_node(node),
                                        ));
                                    }
                                    PatternElement::Expression(_) => {
                                        return Err(
                                            ResolveError::ExpressionsNotAllowedInLetPattern,
                                        );
                                    }
                                }
                            }
                        }
                        ResolvedEnumVariantContainerType::Nothing => {
                            if !elements.is_empty() {
                                return Err(ResolveError::EnumVariantHasNoFields(
                                    self.to_node(variant_name),
                                ));
                            }
                        }
                    }

                    Ok((
                        ResolvedPattern::EnumPattern(
                            enum_variant_type_ref,
                            Some(resolved_elements),
                        ),
                        scope_was_pushed,
                    ))
                } else {
                    Ok((
                        ResolvedPattern::EnumPattern(enum_variant_type_ref, None),
                        false,
                    ))
                }
            }

            Pattern::Literal(ast_literal) => {
                Ok((self.resolve_pattern_literal(ast_literal)?, false))
            }
        }
    }

    fn resolve_iterator(
        &mut self,
        expression: &Expression,
        is_mutable: &Option<Node>,
    ) -> Result<ResolvedIterator, ResolveError> {
        let resolved_expression = self.resolve_expression(expression)?;
        let resolved_type = resolution(&resolved_expression);
        let (key_type, value_type): (Option<ResolvedType>, ResolvedType) = match resolved_type {
            ResolvedType::Array(array_type) => (
                Some(self.shared.types.int_type()),
                array_type.item_type.clone(),
            ),
            ResolvedType::Map(map_type_ref) => (
                Some(map_type_ref.key_type.clone()),
                map_type_ref.value_type.clone(),
            ),
            ResolvedType::ExclusiveRange(_) => (None, self.shared.types.int_type()),
            ResolvedType::Generic(_base_type, params) => {
                // TODO: HACK: We assume it is a container that iterates over the type parameters
                // TODO: HACK: We assume that it is a sparse map
                // TODO: HACK: Remove hardcoded number
                let rust_type_ref_for_id = create_rust_type("SparseId", 998);
                let rust_id_type = ResolvedType::RustType(rust_type_ref_for_id);
                (Some(rust_id_type), params[0].clone())
                /*
                               ResolvedType::Tuple(ResolvedTupleTypeRef::from(ResolvedTupleType(vec![
                                   rust_id_type,
                                   params[0].clone(),
                               ])))
                               *
                */
            }
            _ => return Err(ResolveError::NotAnIterator(resolved_expression.span())),
        };

        Ok(ResolvedIterator {
            key_type,
            value_type,
            resolved_expression,
            mutable_node: self.to_node_option(is_mutable),
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
                error!("{argument_type:?}: {resolved_argument_expression:?}");
                return Err(ResolveError::IncompatibleArguments(
                    argument_type,
                    parameter_type.clone(),
                ));
            }

            if parameter.is_mutable.is_some()
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
                if function_call.signature.first_parameter_is_self {
                    // For member functions, extract the target object from the function expression
                    match function_expression {
                        Expression::MemberCall(ref target, _, _) => {
                            let mut all_args = Vec::new();

                            // extend_from_slice is using Clone for unknown reason,
                            // even though we have ownership of the Expression in &[Expression].
                            unsafe {
                                // Move target expression into vec
                                let target_expr = std::ptr::read(&**target);
                                all_args.push(target_expr);

                                let target_ptr = all_args.as_mut_ptr().add(1);
                                std::ptr::copy_nonoverlapping(
                                    arguments.as_ptr(),
                                    target_ptr,
                                    arguments.len(),
                                );
                                all_args.set_len(1 + arguments.len());
                            }

                            self.resolve_internal_function_call(
                                function_call,
                                function_expr,
                                &all_args,
                            )
                        }
                        _ => Err(ResolveError::ExpectedMemberCall(
                            function_call.name.0.clone(),
                        )),
                    }
                } else {
                    // Regular function call
                    self.resolve_internal_function_call(function_call, function_expr, arguments)
                }
            }
            ResolvedType::FunctionExternal(ref function_call) => {
                // Similar handling for external functions
                if function_call.signature.first_parameter_is_self {
                    match function_expression {
                        Expression::MemberCall(target, _, _) => {
                            let mut all_args = Vec::new();

                            // extend_from_slice is using Clone for unknown reason,
                            // even though we have ownership of the Expression in &[Expression].
                            unsafe {
                                // Move target expression into vec
                                let target_expr = std::ptr::read(&**target);
                                all_args.push(target_expr);

                                let target_ptr = all_args.as_mut_ptr().add(1);
                                std::ptr::copy_nonoverlapping(
                                    arguments.as_ptr(),
                                    target_ptr,
                                    arguments.len(),
                                );
                                all_args.set_len(1 + arguments.len());
                            }
                            self.resolve_external_function_call(
                                function_call,
                                function_expr,
                                &all_args,
                            )
                        }
                        _ => Err(ResolveError::ExpectedMemberCall(function_call.name.clone())),
                    }
                } else {
                    self.resolve_external_function_call(function_call, function_expr, arguments)
                }
            }
            _ => Err(ResolveError::ExpectedFunctionExpression),
        }
    }

    fn resolve_static_call(
        &mut self,
        type_name: &Node,
        function_name: &Node,
        arguments: &[Expression],
    ) -> Result<ResolvedStaticCall, ResolveError> {
        let resolved_arguments = self.resolve_expressions(arguments)?;

        let struct_type_ref = self.find_struct_type_local_mut(type_name)?;
        let struct_ref = struct_type_ref.borrow();

        let function_name_str = self.get_text(function_name).to_string();

        struct_ref.functions.get(&function_name_str).map_or_else(
            || {
                Err(ResolveError::CouldNotFindStaticMember(
                    self.to_node(type_name),
                    self.to_node(function_name),
                ))
            },
            |function_ref| {
                Ok(ResolvedStaticCall {
                    function: function_ref.clone(),
                    arguments: resolved_arguments,
                })
            },
        )
    }

    fn resolve_member_call(
        &mut self,
        ast_member_expression: &Expression,
        ast_member_function_name: &Node,
        ast_arguments: &[Expression],
    ) -> Result<ResolvedMemberCall, ResolveError> {
        let (function_ref, resolved_expression) =
            self.resolve_into_member_function(ast_member_expression, ast_member_function_name)?;

        let (is_self_mutable, signature) = match &*function_ref {
            ResolvedFunction::Internal(function_data) => {
                let first_param = function_data
                    .signature
                    .parameters
                    .first()
                    .ok_or_else(|| ResolveError::WrongNumberOfArguments(0, 1))?;
                (first_param.is_mutable.clone(), &function_data.signature)
            }
            ResolvedFunction::External(external) => {
                let first_param = external
                    .signature
                    .parameters
                    .first()
                    .ok_or_else(|| ResolveError::WrongNumberOfArguments(0, 1))?;
                (first_param.is_mutable.clone(), &external.signature)
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
            // struct_type_ref: resolved_struct_type_ref.clone(),
            self_is_mutable: is_self_mutable.is_some(),
        })
    }

    fn resolve_binary_op(
        &mut self,
        ast_left: &Expression,
        ast_op: &BinaryOperator,
        ast_right: &Expression,
    ) -> Result<ResolvedBinaryOperator, ResolveError> {
        let left = self.resolve_expression(ast_left)?;
        let left_type = resolution(&left);

        let right = self.resolve_expression(ast_right)?;
        let right_type = resolution(&right);

        let (resolved_node, kind) = self.convert_binary_operator_kind(ast_op);

        match (&kind, &left_type, &right_type) {
            // String concatenation - allow any type on the right
            (&ResolvedBinaryOperatorKind::Add, ResolvedType::String(_), _) => {
                Ok(ResolvedBinaryOperator {
                    left: Box::new(left),
                    right: Box::new(right),
                    kind,
                    node: resolved_node,
                    resolved_type: self.shared.types.string_type(),
                })
            }

            // Comparison operators
            (
                ResolvedBinaryOperatorKind::Equal
                | ResolvedBinaryOperatorKind::NotEqual
                | ResolvedBinaryOperatorKind::GreaterThan
                | ResolvedBinaryOperatorKind::GreaterEqual
                | ResolvedBinaryOperatorKind::LessThan
                | ResolvedBinaryOperatorKind::LessEqual,
                _,
                _,
            ) => {
                if !left_type.same_type(&right_type) {
                    debug!(?left_type, ?right_type, "type mismatch in comparison");
                    return Err(ResolveError::IncompatibleTypes(left.span(), right_type));
                }
                Ok(ResolvedBinaryOperator {
                    left: Box::new(left),
                    right: Box::new(right),
                    kind,
                    node: resolved_node,
                    resolved_type: self.shared.types.bool_type(),
                })
            }

            // All other operators require exact type matches
            _ => {
                if !left_type.same_type(&right_type) {
                    debug!(?left_type, ?right_type, "type mismatch in operation");
                    return Err(ResolveError::IncompatibleTypes(left.span(), right_type));
                }
                Ok(ResolvedBinaryOperator {
                    left: Box::new(left),
                    right: Box::new(right),
                    kind,
                    node: resolved_node,
                    resolved_type: left_type,
                })
            }
        }
    }

    fn resolve_unary_op(
        &mut self,
        ast_op: &UnaryOperator,
        ast_left: &Expression,
    ) -> Result<ResolvedUnaryOperator, ResolveError> {
        let left = self.resolve_expression(ast_left)?;
        let resolved_type = resolution(&left);

        let (node, kind) = match ast_op {
            UnaryOperator::Not(node) => (node, ResolvedUnaryOperatorKind::Not),
            UnaryOperator::Negate(node) => (node, ResolvedUnaryOperatorKind::Negate),
        };

        Ok(ResolvedUnaryOperator {
            left: Box::new(left),
            resolved_type,
            kind,
            node: self.to_node(node),
        })
    }

    fn resolve_postfix_op(
        &mut self,
        ast_op: &PostfixOperator,
        ast_left: &Expression,
    ) -> Result<ResolvedPostfixOperator, ResolveError> {
        let left = self.resolve_expression(ast_left)?;
        let resolved_type = resolution(&left);

        let (resolved_node, resolved_op_kind) = match ast_op {
            PostfixOperator::Unwrap(node) => {
                (self.to_node(node), ResolvedPostfixOperatorKind::Unwrap)
            }
        };

        Ok(ResolvedPostfixOperator {
            left: Box::new(left),
            kind: resolved_op_kind,
            resolved_type,
            node: resolved_node,
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
        string_parts: &[StringPart],
    ) -> Result<Vec<ResolvedStringPart>, ResolveError> {
        let mut resolved_parts = Vec::new();
        for part in string_parts {
            let resolved_string_part = match part {
                StringPart::Literal(string_node) => {
                    let string_str = self.get_text(string_node).to_string();
                    ResolvedStringPart::Literal(self.to_node(string_node), string_str)
                }
                StringPart::Interpolation(expression, format_specifier) => {
                    let expr = self.resolve_expression(expression)?;
                    let resolved_format_specifier = self.resolve_format_specifier(format_specifier);
                    ResolvedStringPart::Interpolation(expr, resolved_format_specifier)
                }
            };

            resolved_parts.push(resolved_string_part);
        }

        Ok(resolved_parts)
    }

    fn resolve_variable_or_function_access(
        &self,
        var_or_function_ref_node: &Node,
    ) -> Result<ResolvedExpression, ResolveError> {
        self.try_find_variable(var_or_function_ref_node)
            .map_or_else(
                || {
                    let name = self.get_text(var_or_function_ref_node).to_string();
                    self.shared
                        .lookup
                        .get_internal_function(&[], &name)
                        .map_or_else(
                            || {
                                self.shared
                                    .lookup
                                    .get_external_function_declaration(&[], &name)
                                    .map_or_else(
                                        || {
                                            error!("unknown external function {:?}", name);
                                            Err(ResolveError::UnknownVariable(
                                                self.to_node(var_or_function_ref_node),
                                            ))
                                        },
                                        |external_function_ref| {
                                            Ok(ResolvedExpression::ExternalFunctionAccess(
                                                external_function_ref,
                                            ))
                                        },
                                    )
                            },
                            |function_ref| {
                                Ok(ResolvedExpression::InternalFunctionAccess(function_ref))
                            },
                        )
                },
                |variable_ref| Ok(ResolvedExpression::VariableAccess(variable_ref)),
            )
    }

    #[allow(unused)]
    fn find_variable(&self, variable: &Variable) -> Result<ResolvedVariableRef, ResolveError> {
        self.try_find_variable(&variable.name).map_or_else(
            || Err(ResolveError::UnknownVariable(self.to_node(&variable.name))),
            Ok,
        )
    }

    fn find_variable_from_node(&self, node: &Node) -> Result<ResolvedVariableRef, ResolveError> {
        self.try_find_variable(node).map_or_else(
            || Err(ResolveError::UnknownVariable(self.to_node(node))),
            Ok,
        )
    }

    fn try_find_variable(&self, node: &Node) -> Option<ResolvedVariableRef> {
        let variable_text = self.get_text(node);
        for scope in self.scope.block_scope_stack.iter().rev() {
            if let Some(value) = scope.variables.get(&variable_text.to_string()) {
                return Some(value.clone());
            }
        }

        None
    }

    fn try_find_local_variable(&self, node: &ResolvedNode) -> Option<&ResolvedVariableRef> {
        let current_scope = self
            .scope
            .block_scope_stack
            .iter()
            .last()
            .expect("no scope stack available");

        let variable_text = self.get_text_resolved(node).to_string();

        current_scope.variables.get(&variable_text)
    }

    fn resolve_usize_index(
        &mut self,
        usize_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let lookup_expression = self.resolve_expression(usize_expression)?;
        let lookup_resolution = resolution(&lookup_expression);

        match &lookup_resolution {
            ResolvedType::Int(_) => {}
            _ => Err(ResolveError::ArrayIndexMustBeInt(lookup_resolution))?,
        }

        Ok(lookup_expression)
    }

    fn resolve_variable_assignment(
        &mut self,
        ast_variable: &Variable,
        ast_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let converted_expression = self.resolve_expression(ast_expression)?;
        let expression_type = resolution(&converted_expression);
        let (variable_ref, is_reassignment) =
            self.set_or_overwrite_variable_with_type(ast_variable, &expression_type)?;

        let assignment = ResolvedVariableAssignment {
            variable_refs: vec![variable_ref],
            expression: Box::from(converted_expression),
        };

        let debug_name = self.get_text_resolved(&assignment.variable_refs[0].name);

        if is_reassignment {
            Ok(ResolvedExpression::ReassignVariable(assignment))
        } else {
            Ok(ResolvedExpression::InitializeVariable(assignment))
        }
    }

    fn resolve_multi_variable_assignment(
        &mut self,
        ast_variables: &[Variable],
        ast_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let converted_expression = self.resolve_expression(ast_expression)?;
        let expression_type = resolution(&converted_expression);

        let mut variable_refs = Vec::new();
        let mut all_reassignment = true;

        for ast_variable in ast_variables {
            let (variable_ref, is_reassignment) =
                self.set_or_overwrite_variable_with_type(ast_variable, &expression_type)?;
            variable_refs.push(variable_ref);
            if !is_reassignment {
                all_reassignment = false;
            }
        }

        let assignment = ResolvedVariableAssignment {
            variable_refs,
            expression: Box::from(converted_expression),
        };

        if all_reassignment {
            Ok(ResolvedExpression::ReassignVariable(assignment))
        } else {
            Ok(ResolvedExpression::InitializeVariable(assignment))
        }
    }

    fn resolve_compound_assignment_variable(
        &mut self,
        target: &Node,
        operator: &CompoundOperator,
        source: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_variable = self.find_variable_from_node(target)?;
        let resolved_source = self.resolve_expression(source)?;

        let target_type = &resolved_variable.resolved_type;

        let resolved_compound_operator = self.resolve_compound_operator(operator);

        match &target_type {
            ResolvedType::Int(_) => {
                let compound_assignment = ResolvedVariableCompoundAssignment {
                    variable_ref: resolved_variable,
                    expression: Box::from(resolved_source),
                    compound_operator: resolved_compound_operator,
                };
                Ok(ResolvedExpression::VariableCompoundAssignment(
                    compound_assignment,
                ))
            }
            ResolvedType::Array(array_type_ref) => {
                match resolved_compound_operator.kind {
                    ResolvedCompoundOperatorKind::Add => {
                        let source_type = resolution(&resolved_source);
                        if let ResolvedType::Array(_source_array_type) = &source_type {
                            // Concatenating two arrays
                            if !target_type.same_type(&source_type) {
                                return Err(ResolveError::IncompatibleTypes(
                                    resolved_source.span(),
                                    target_type.clone(),
                                ));
                            }
                            Ok(ResolvedExpression::ArrayExtend(
                                resolved_variable,
                                Box::new(resolved_source),
                            ))
                        } else {
                            // Appending a single item
                            if !source_type.same_type(&array_type_ref.item_type) {
                                return Err(ResolveError::IncompatibleTypes(
                                    resolved_source.span(),
                                    target_type.clone(),
                                ));
                            }
                            Ok(ResolvedExpression::ArrayPush(
                                resolved_variable,
                                Box::new(resolved_source),
                            ))
                        }
                    }
                    _ => Err(ResolveError::InvalidOperatorForArray(
                        self.to_node(&operator.node),
                    )),
                }
            }
            _ => Err(ResolveError::ExpectedArray(target_type.clone())),
        }
    }

    fn resolve_array_type_helper(
        &mut self,
        items: &[Expression],
    ) -> Result<(ResolvedArrayTypeRef, Vec<ResolvedExpression>), ResolveError> {
        let expressions = self.resolve_expressions(items)?;
        let item_type = if expressions.is_empty() {
            ResolvedType::Any
        } else {
            resolution(&expressions[0])
        };

        let array_type = ResolvedArrayType { item_type };

        let array_type_ref = Rc::new(array_type);

        Ok((array_type_ref, expressions))
    }

    fn resolve_tuple_literal(
        &mut self,
        items: &[Expression],
    ) -> Result<(ResolvedTupleTypeRef, Vec<ResolvedExpression>), ResolveError> {
        let expressions = self.resolve_expressions(items)?;
        let mut tuple_types = Vec::new();
        for expr in &expressions {
            let item_type = resolution(expr);
            tuple_types.push(item_type);
        }

        let tuple_type = ResolvedTupleType(tuple_types);

        let tuple_type_ref = Rc::new(tuple_type);

        Ok((tuple_type_ref, expressions))
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
                    expected: key_type,
                    found: resolution(&resolved_key),
                });
            }

            if !resolution(&resolved_value).same_type(&value_type) {
                return Err(ResolveError::MapValueTypeMismatch {
                    expected: value_type,
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

    fn push_block_scope(&mut self, _debug_str: &str) {
        self.scope.block_scope_stack.push(BlockScope {
            variables: SeqMap::default(),
        });
    }

    fn pop_block_scope(&mut self, _debug_str: &str) {
        self.scope.block_scope_stack.pop();
    }

    fn set_or_overwrite_variable_with_type(
        &mut self,
        variable: &Variable,
        variable_type_ref: &ResolvedType,
    ) -> Result<(ResolvedVariableRef, bool), ResolveError> {
        if let Some(existing_variable) = self.try_find_variable(&variable.name) {
            // Check type compatibility
            if !&existing_variable.resolved_type.same_type(variable_type_ref) {
                return Err(ResolveError::OverwriteVariableWithAnotherType(
                    self.to_node(&variable.name),
                ));
            }

            let var_name = self.get_text_resolved(&existing_variable.name);

            // For reassignment, check if the EXISTING variable is mutable
            if !existing_variable.is_mutable() {
                return Err(ResolveError::CanOnlyOverwriteVariableWithMut(
                    self.to_node(&variable.name),
                ));
            }

            return Ok((existing_variable, true));
        }

        // For first assignment, create new variable with the mutability from the assignment
        let scope_index = self.scope.block_scope_stack.len() - 1;
        let name = self.to_node(&variable.name);
        let mutable_node = self.to_node_option(&variable.is_mutable);
        let variable_name_str = self.get_text_resolved(&name).to_string();

        let variables = &mut self
            .scope
            .block_scope_stack
            .last_mut()
            .expect("block scope should have at least one scope")
            .variables;
        let variable_index = variables.len();

        let resolved_variable = ResolvedVariable {
            name,
            resolved_type: variable_type_ref.clone(),
            mutable_node,
            scope_index,
            variable_index,
        };

        let variable_ref = Rc::new(resolved_variable);

        {
            variables
                .insert(variable_name_str, variable_ref.clone())
                .expect("should have checked earlier for variable");
        }

        Ok((variable_ref, false))
    }

    fn create_local_variable(
        &mut self,
        variable: &Node,
        is_mutable: &Option<Node>,
        variable_type_ref: &ResolvedType,
    ) -> Result<ResolvedVariableRef, ResolveError> {
        self.create_local_variable_resolved(
            &self.to_node(variable),
            &self.to_node_option(is_mutable),
            variable_type_ref,
        )
    }

    fn create_local_variable_resolved(
        &mut self,
        variable: &ResolvedNode,
        is_mutable: &Option<ResolvedNode>,
        variable_type_ref: &ResolvedType,
    ) -> Result<ResolvedVariableRef, ResolveError> {
        if let Some(_existing_variable) = self.try_find_local_variable(variable) {
            return Err(ResolveError::OverwriteVariableNotAllowedHere(
                variable.clone(),
            ));
        }
        let variable_str = self.get_text_resolved(variable).to_string();

        let scope_index = self.scope.block_scope_stack.len() - 1;

        let variables = &mut self
            .scope
            .block_scope_stack
            .last_mut()
            .expect("block scope should have at least one scope")
            .variables;

        let resolved_variable = ResolvedVariable {
            name: variable.clone(),
            resolved_type: variable_type_ref.clone(),
            mutable_node: is_mutable.clone(),
            scope_index,
            variable_index: variables.len(),
        };

        let variable_ref = Rc::new(resolved_variable);

        variables
            .insert(variable_str, variable_ref.clone())
            .expect("should have checked earlier for variable");

        Ok(variable_ref)
    }

    fn resolve_enum_variant_ref(
        &self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
        variant_name: &LocalTypeIdentifier,
    ) -> Result<ResolvedEnumVariantTypeRef, ResolveError> {
        let variant_name_string = self.get_text(&variant_name.0).to_string();
        self.get_enum_variant_type(qualified_type_identifier, &variant_name_string)
            .map_or_else(
                || {
                    Err(ResolveError::UnknownEnumVariantType(
                        qualified_type_identifier.clone(),
                    ))
                },
                Ok,
            )
    }

    #[allow(unused)]

    fn resolve_enum_variant_literal(
        &mut self,
        ast_variant: &EnumVariantLiteral,
    ) -> Result<ResolvedLiteral, ResolveError> {
        let (qualified_name, variant_name) = match ast_variant {
            EnumVariantLiteral::Simple(name, variant) => (name, variant),
            EnumVariantLiteral::Tuple(name, variant, _) => (name, variant),
            EnumVariantLiteral::Struct(name, variant, _) => (name, variant),
        };

        let variant_ref = self.resolve_enum_variant_ref(qualified_name, variant_name)?;

        let resolved_data = match ast_variant {
            EnumVariantLiteral::Simple(_qualified_name, _variant_name) => {
                ResolvedEnumLiteralData::Nothing
            }
            EnumVariantLiteral::Tuple(_qualified_name, _variant_name, expressions) => {
                ResolvedEnumLiteralData::Tuple(self.resolve_expressions(expressions)?)
            }
            EnumVariantLiteral::Struct(_qualified_name, _variant_name, field_expressions) => {
                let mut resolved_expressions = Vec::new();
                for field_expression in field_expressions {
                    let resolved_expression =
                        self.resolve_expression(&field_expression.expression)?;
                    resolved_expressions.push(resolved_expression);
                }
                ResolvedEnumLiteralData::Struct(resolved_expressions)
            }
        };

        Ok(ResolvedLiteral::EnumVariantLiteral(
            variant_ref,
            resolved_data,
        ))
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
        let (resolved_pattern, scope_was_pushed) =
            self.resolve_pattern(&arm.pattern, expression_type)?;

        let resolved_expression = self.resolve_expression(&arm.expression)?;
        if scope_was_pushed {
            self.pop_block_scope("resolve_arm");
        }

        let resolved_type = resolution(&resolved_expression);

        Ok(ResolvedMatchArm {
            pattern: resolved_pattern,
            expression: Box::from(resolved_expression),
            expression_type: resolved_type,
        })
    }

    fn str_to_int(text: &str) -> Result<i32, ParseIntError> {
        text.parse::<i32>()
    }

    fn str_to_float(text: &str) -> Result<f32, ParseFloatError> {
        text.parse::<f32>()
    }

    fn resolve_pattern_literal(
        &mut self,
        ast_literal: &Literal,
    ) -> Result<ResolvedPattern, ResolveError> {
        let resolved_literal = self.resolve_literal(ast_literal)?;

        Ok(ResolvedPattern::Literal(resolved_literal))
    }

    /// Analyzes an expression that might contain an optional unwrap operator,
    /// creating a new scope with the unwrapped shadow variable if necessary.
    /// It is a bit hacky code in the analyzer, but it enables a very ergonomic syntax of
    /// `if a? { print('a is {a}') }`
    fn analyze_optional_condition(
        &mut self,
        condition: &Expression,
        true_block: &Expression,
        false_block: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        match condition {
            Expression::PostfixOp(PostfixOperator::Unwrap(_), expr) => {
                if let Expression::VariableAccess(var) = &**expr {
                    self.handle_optional_unwrap(var, expr, true_block, false_block)
                } else {
                    Err(ResolveError::ExpectedVariable)
                }
            }

            Expression::VariableAssignment(var, expr) => {
                if let Expression::PostfixOp(PostfixOperator::Unwrap(_), inner_expr) = &**expr {
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
                self.analyze_in_new_scope(var, &inner_type, true_block)?;

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
        variable_type: &ResolvedType,
        block: &Expression,
    ) -> Result<(ResolvedVariableRef, ResolvedExpression), ResolveError> {
        self.push_block_scope("not sure, analyze_in_new_scope");
        let resolved_var_ref =
            self.create_local_variable(&var.name, &var.is_mutable, variable_type)?;
        let resolved_expr = self.resolve_expression(block)?;
        self.pop_block_scope("not sure, analyze_in_new_scope");

        Ok((resolved_var_ref, resolved_expr))
    }

    fn analyze_block_with_scope(
        &mut self,
        block: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        self.push_block_scope("analyze_block_with_scope");
        let resolved = self.resolve_expression(block)?;
        self.pop_block_scope("analyze_block_with_scope");
        Ok(resolved)
    }

    fn resolve_normal_if(
        &mut self,
        condition: &Expression,
        true_block: &Expression,
        false_block: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        debug!(
            ?condition,
            ?true_block,
            ?false_block,
            "resolve_normal_if input"
        );

        let resolved_condition = self.resolve_bool_expression(condition)?;
        debug!(?resolved_condition, "resolved condition");

        let resolved_true = self.analyze_block_with_scope(true_block)?;
        debug!(?resolved_true, "resolved true block");
        let true_type = resolution(&resolved_true);
        debug!(?true_type, "true block type");

        let resolved_false = self.analyze_block_with_scope(false_block)?;
        let false_type = resolution(&resolved_false);
        debug!(?false_type, "false block type");

        debug!(?self.scope.return_type, "current return type context");

        if !matches!(self.scope.return_type, ResolvedType::Unit(_)) {
            //let true_type = resolution(&resolved_true);
            //let false_type = resolution(&resolved_false);

            /*
            let wrapped_true = if matches!(true_type, ResolvedType::Optional(_)) {
                resolved_true
            } else {
                ResolvedExpression::Option(Some(Box::new(resolved_true)))
            };

            let wrapped_false = if matches!(false_type, ResolvedType::Optional(_)) {
                resolved_false
            } else {
                ResolvedExpression::Option(Some(Box::new(resolved_false)))
            };*/

            Ok(ResolvedExpression::IfElse(
                Box::from(resolved_condition),
                Box::from(resolved_true),
                Box::from(resolved_false),
            ))
        } else {
            Ok(ResolvedExpression::IfElse(
                Box::from(resolved_condition),
                Box::from(resolved_true),
                Box::from(resolved_false),
            ))
        }
    }

    fn resolve_array_member_call(
        &mut self,
        var_ref: ResolvedVariableRef,
        ast_member_function_name: &Node,
        ast_arguments: &[Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        let member_function_name_str = self.get_text(ast_member_function_name);
        let expr = match member_function_name_str {
            "remove" => {
                if !var_ref.is_mutable() {
                    return Err(ResolveError::VariableIsNotMutable(var_ref.name.clone()));
                }

                if ast_arguments.len() != 1 {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 1));
                }
                let index_expr = self.resolve_usize_index(&ast_arguments[0])?;

                ResolvedExpression::ArrayRemoveIndex(var_ref, Box::new(index_expr))
            }

            "clear" => {
                if !var_ref.is_mutable() {
                    return Err(ResolveError::VariableIsNotMutable(var_ref.name.clone()));
                }

                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }

                ResolvedExpression::ArrayClear(var_ref)
            }
            _ => {
                return Err(ResolveError::UnknownMemberFunction(
                    self.to_node(ast_member_function_name),
                ))
            }
        };

        Ok(expr)
    }

    fn resolve_float_member_call(
        &self,
        expr: ResolvedExpression,
        ast_member_function_name: &Node,
        ast_arguments: &[Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        let function_name_str = self.get_text(ast_member_function_name);
        match function_name_str {
            "round" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::FloatRound(Box::new(expr)))
            }
            "floor" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::FloatFloor(Box::new(expr)))
            }
            "sign" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::FloatSign(Box::new(expr)))
            }
            "abs" => {
                if !ast_arguments.is_empty() {
                    return Err(ResolveError::WrongNumberOfArguments(ast_arguments.len(), 0));
                }
                Ok(ResolvedExpression::FloatAbs(Box::new(expr)))
            }
            _ => Err(ResolveError::UnknownMemberFunction(
                self.to_node(ast_member_function_name),
            )),
        }
    }

    fn check_for_internal_member_call(
        &mut self,
        source: &Expression,
        ast_member_function_name: &Node,
        ast_arguments: &[Expression],
    ) -> Result<Option<ResolvedExpression>, ResolveError> {
        let resolved_expr = self.resolve_expression(source)?;

        match resolution(&resolved_expr) {
            ResolvedType::Array(_) => {
                if let ResolvedExpression::VariableAccess(var_ref) = resolved_expr {
                    let resolved = self.resolve_array_member_call(
                        var_ref,
                        ast_member_function_name,
                        ast_arguments,
                    )?;
                    return Ok(Some(resolved));
                }
            }
            ResolvedType::Float(_) => {
                let resolved = self.resolve_float_member_call(
                    resolved_expr,
                    ast_member_function_name,
                    ast_arguments,
                )?;
                return Ok(Some(resolved));
            }
            _ => {
                return self.check_for_internal_member_call_extra(
                    resolved_expr,
                    ast_member_function_name,
                    ast_arguments,
                );
            }
        }
        Ok(None)
    }

    fn check_for_internal_member_call_extra(
        &mut self,
        self_expression: ResolvedExpression,
        ast_member_function_name: &Node,
        ast_arguments: &[Expression],
    ) -> Result<Option<ResolvedExpression>, ResolveError> {
        // TODO: Early out
        if let ResolvedType::Generic(generic_type, _parameters) = resolution(&self_expression) {
            if let ResolvedType::RustType(rust_type_ref) = *generic_type {
                if rust_type_ref.as_ref().number == SPARSE_TYPE_ID {
                    let function_name_str = self.get_text(ast_member_function_name);
                    // TODO: Remove hack
                    match function_name_str {
                        "add" => {
                            if ast_arguments.len() != 1 {
                                return Err(ResolveError::WrongNumberOfArguments(
                                    ast_arguments.len(),
                                    1,
                                ));
                            }
                            let value = self.resolve_expression(&ast_arguments[0])?;
                            return Ok(Some(ResolvedExpression::SparseAdd(
                                Box::new(self_expression),
                                Box::new(value),
                            )));
                        }
                        "remove" => {
                            if ast_arguments.len() != 1 {
                                return Err(ResolveError::WrongNumberOfArguments(
                                    ast_arguments.len(),
                                    1,
                                ));
                            }
                            let sparse_slot_id_expression =
                                self.resolve_expression(&ast_arguments[0])?;
                            return Ok(Some(ResolvedExpression::SparseRemove(
                                Box::new(self_expression),
                                Box::new(sparse_slot_id_expression),
                            )));
                        }
                        _ => {}
                    }
                }
            }
        }

        Ok(None)
    }

    fn resolve_static_call_generic(
        &mut self,
        type_name: &Node,
        function_name: &Node,
        generic_types: &[Type],
        arguments: &[Expression],
    ) -> Result<ResolvedStaticCallGeneric, ResolveError> {
        let resolved_arguments = self.resolve_expressions(arguments)?;
        let resolved_generic_types = self.resolve_types(generic_types)?;

        let struct_type_ref = self.find_struct_type_local_mut(type_name)?;
        let struct_ref = struct_type_ref.borrow();

        let function_name_str = self.get_text(function_name).to_string();

        struct_ref.functions.get(&function_name_str).map_or_else(
            || {
                Err(ResolveError::CouldNotFindStaticMember(
                    self.to_node(type_name),
                    self.to_node(function_name),
                ))
            },
            |function_ref| {
                Ok(ResolvedStaticCallGeneric {
                    function: function_ref.clone(),
                    arguments: resolved_arguments,
                    generic_types: resolved_generic_types,
                })
            },
        )
    }

    fn check_for_internal_static_call(
        &mut self,
        type_name: &Node,
        function_name: &Node,
        generic_types: &[Type],
        arguments: &[Expression],
    ) -> Result<Option<ResolvedExpression>, ResolveError> {
        let (type_name_text, function_name_text) = {
            (
                self.get_text(type_name).to_string(),
                self.get_text(function_name),
            )
        };

        if type_name_text == "Sparse" && function_name_text == "new" {
            if !arguments.is_empty() {
                return Err(ResolveError::WrongNumberOfArguments(arguments.len(), 0));
            }
            let resolved_generic_types = self.resolve_types(generic_types)?;
            if resolved_generic_types.len() != 1 {
                return Err(ResolveError::WrongNumberOfTypeArguments(
                    resolved_generic_types.len(),
                    1,
                ));
            }

            let rust_type_ref = Rc::new(ResolvedRustType {
                type_name: type_name_text,
                number: SPARSE_TYPE_ID, // TODO: FIX hardcoded number
            });

            let rust_type_base = ResolvedType::RustType(rust_type_ref.clone());

            let resolved_type =
                ResolvedType::Generic(Box::from(rust_type_base), resolved_generic_types);

            return Ok(Some(ResolvedExpression::SparseNew(
                rust_type_ref,
                resolved_type,
            )));
        }

        Ok(None)
    }

    fn resolve_field_access(
        &mut self,
        base_expression: &Expression,
        struct_field_ref: &ResolvedStructTypeFieldRef,
        ast_field_name: &Node,
    ) -> Result<ResolvedExpression, ResolveError> {
        let mut access_chain = Vec::new();
        let (resolved_last_type, resolved_base_expression) =
            self.collect_field_chain(base_expression, &mut access_chain)?;

        // Add the last lookup that is part of the field lookup
        let (_field_type, field_index) =
            self.get_field_index(&resolved_last_type, ast_field_name)?;

        access_chain.push(ResolvedAccess::FieldIndex(
            self.to_node(ast_field_name),
            field_index,
        ));

        Ok(ResolvedExpression::FieldAccess(
            Box::from(resolved_base_expression),
            struct_field_ref.clone(),
            access_chain,
        ))
    }

    fn resolve_field_assignment(
        &mut self,
        ast_struct_field_expr: &Expression,
        ast_field_name: &Node,
        ast_source_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let mut chain = Vec::new();
        let (resolved_last_type, resolved_first_base_expression) =
            self.collect_field_chain(ast_struct_field_expr, &mut chain)?;

        let _ast_field_name_str = self.get_text(ast_field_name).to_string();
        // Add the last lookup that is part of the field_assignment itself
        let (_field_type, field_index) =
            self.get_field_index(&resolved_last_type, ast_field_name)?;

        chain.push(ResolvedAccess::FieldIndex(
            self.to_node(ast_field_name),
            field_index,
        ));

        let resolved_target_type = resolution(&resolved_first_base_expression);

        let source_expression = self.resolve_expression(ast_source_expression)?;
        let wrapped_expression = wrap_in_some_if_optional(&resolved_target_type, source_expression);

        Ok(ResolvedExpression::StructFieldAssignment(
            Box::new(resolved_first_base_expression),
            chain,
            Box::from(wrapped_expression),
        ))
    }

    fn resolve_field_assignment_compound(
        &mut self,
        ast_struct_field_expr: &Expression,
        ast_field_name: &Node,
        ast_operator: &CompoundOperator,
        ast_source_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let mut chain = Vec::new();
        let (resolved_last_type, resolved_first_base_expression) =
            self.collect_field_chain(ast_struct_field_expr, &mut chain)?;

        let resolved_operator = self.resolve_compound_operator(ast_operator);

        // Add the last lookup that is part of the field_assignment itself
        let (_field_type, field_index) =
            self.get_field_index(&resolved_last_type, ast_field_name)?;

        chain.push(ResolvedAccess::FieldIndex(
            self.to_node(ast_field_name),
            field_index,
        ));

        let resolved_target_type = resolution(&resolved_first_base_expression);

        let source_expression = self.resolve_expression(ast_source_expression)?;
        let wrapped_expression = wrap_in_some_if_optional(&resolved_target_type, source_expression);

        Ok(ResolvedExpression::FieldCompoundAssignment(
            Box::new(resolved_first_base_expression),
            chain,
            resolved_operator,
            Box::from(wrapped_expression),
        ))
    }

    const fn to_node(&self, node: &Node) -> ResolvedNode {
        ResolvedNode {
            span: Span {
                file_id: self.shared.file_id,
                offset: node.span.offset,
                length: node.span.length,
            },
        }
    }

    const fn convert_binary_operator_kind(
        &self,
        binary_operator: &BinaryOperator,
    ) -> (ResolvedNode, ResolvedBinaryOperatorKind) {
        match binary_operator {
            BinaryOperator::Add(node) => (self.to_node(node), ResolvedBinaryOperatorKind::Add),
            BinaryOperator::Subtract(node) => {
                (self.to_node(node), ResolvedBinaryOperatorKind::Subtract)
            }
            BinaryOperator::Multiply(node) => {
                (self.to_node(node), ResolvedBinaryOperatorKind::Multiply)
            }
            BinaryOperator::Divide(node) => {
                (self.to_node(node), ResolvedBinaryOperatorKind::Divide)
            }
            BinaryOperator::Modulo(node) => {
                (self.to_node(node), ResolvedBinaryOperatorKind::Modulo)
            }
            BinaryOperator::LogicalOr(node) => {
                (self.to_node(node), ResolvedBinaryOperatorKind::LogicalOr)
            }
            BinaryOperator::LogicalAnd(node) => {
                (self.to_node(node), ResolvedBinaryOperatorKind::LogicalAnd)
            }
            BinaryOperator::Equal(node) => (self.to_node(node), ResolvedBinaryOperatorKind::Equal),
            BinaryOperator::NotEqual(node) => {
                (self.to_node(node), ResolvedBinaryOperatorKind::NotEqual)
            }
            BinaryOperator::LessThan(node) => {
                (self.to_node(node), ResolvedBinaryOperatorKind::LessThan)
            }
            BinaryOperator::LessEqual(node) => {
                (self.to_node(node), ResolvedBinaryOperatorKind::LessEqual)
            }
            BinaryOperator::GreaterThan(node) => {
                (self.to_node(node), ResolvedBinaryOperatorKind::GreaterThan)
            }
            BinaryOperator::GreaterEqual(node) => {
                (self.to_node(node), ResolvedBinaryOperatorKind::GreaterEqual)
            }
            BinaryOperator::RangeExclusive(node) => (
                self.to_node(node),
                ResolvedBinaryOperatorKind::RangeExclusive,
            ),
        }
    }

    fn get_enum_variant_type(
        &self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
        variant_name: &str,
    ) -> Option<ResolvedEnumVariantTypeRef> {
        let path: Vec<String> =
            qualified_type_identifier
                .module_path
                .as_ref()
                .map_or_else(Vec::new, |found| {
                    let mut strings = Vec::new();
                    for path_item in &found.0 {
                        strings.push(self.get_text(&path_item.node).to_string());
                    }
                    strings
                });

        let enum_name = self.get_text(&qualified_type_identifier.name.0).to_string();

        self.shared
            .lookup
            .get_enum_variant_type(&path, &enum_name, variant_name)
    }

    const fn resolve_compound_operator(
        &self,
        ast_operator: &CompoundOperator,
    ) -> ResolvedCompoundOperator {
        let resolved_node = self.to_node(&ast_operator.node);
        let resolved_kind = match ast_operator.kind {
            CompoundOperatorKind::Add => ResolvedCompoundOperatorKind::Add,
            CompoundOperatorKind::Sub => ResolvedCompoundOperatorKind::Sub,
            CompoundOperatorKind::Mul => ResolvedCompoundOperatorKind::Mul,
            CompoundOperatorKind::Div => ResolvedCompoundOperatorKind::Div,
        };

        ResolvedCompoundOperator {
            node: resolved_node,
            kind: resolved_kind,
        }
    }

    const fn to_node_option(&self, maybe_node: &Option<Node>) -> Option<ResolvedNode> {
        match maybe_node {
            None => None,
            Some(node) => Some(self.to_node(node)),
        }
    }

    const fn resolve_format_specifier(
        &self,
        ast_format_specifier: &Option<FormatSpecifier>,
    ) -> Option<ResolvedFormatSpecifier> {
        let f = match ast_format_specifier {
            None => return None,
            Some(ast_format) => match ast_format {
                FormatSpecifier::LowerHex(node) => ResolvedFormatSpecifier {
                    node: self.to_node(node),
                    kind: ResolvedFormatSpecifierKind::LowerHex,
                },
                FormatSpecifier::UpperHex(node) => ResolvedFormatSpecifier {
                    node: self.to_node(node),
                    kind: ResolvedFormatSpecifierKind::UpperHex,
                },
                FormatSpecifier::Binary(node) => ResolvedFormatSpecifier {
                    node: self.to_node(node),
                    kind: ResolvedFormatSpecifierKind::Binary,
                },
                FormatSpecifier::Float(node) => ResolvedFormatSpecifier {
                    node: self.to_node(node),
                    kind: ResolvedFormatSpecifierKind::Float,
                },
                FormatSpecifier::Precision(value, node, x) => {
                    let (precision_type, precision_node) = match x {
                        PrecisionType::Float(node) => {
                            (ResolvedPrecisionType::Float, self.to_node(node))
                        }
                        PrecisionType::String(node) => {
                            (ResolvedPrecisionType::String, self.to_node(node))
                        }
                    };
                    ResolvedFormatSpecifier {
                        node: self.to_node(node),
                        kind: ResolvedFormatSpecifierKind::Precision(
                            *value,
                            precision_node,
                            precision_type,
                        ),
                    }
                }
            },
        };

        Some(f)
    }

    fn resolve_mut_ref(
        &self,
        variable: &MutVariableRef,
    ) -> Result<ResolvedExpression, ResolveError> {
        let var = self.find_variable(&variable.0)?;
        if !var.is_mutable() {
            Err(ResolveError::VariableIsNotMutable(
                self.to_node(&variable.0.name),
            ))?;
        }
        let mut_var = ResolvedMutVariable { variable_ref: var };
        Ok(ResolvedExpression::MutRef(Rc::new(mut_var)))
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
