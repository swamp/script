/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod access;
pub mod assign;
pub mod call;
pub mod constant;
pub mod def;
pub mod err;
pub mod internal;
pub mod literal;
pub mod lookup;
pub mod operator;
mod optional;
pub mod pattern;
pub mod prelude;
mod structure;
pub mod types;
pub mod variable;

use crate::err::ResolveError;
use crate::lookup::NameLookup;
use seq_map::SeqMap;
use std::mem::take;
use std::num::{ParseFloatError, ParseIntError};
use std::rc::Rc;
use swamp_script_ast::prelude::*;
use swamp_script_ast::{
    CompoundOperator, CompoundOperatorKind, EnumVariantLiteral, ForPattern, Function,
    LocationExpression, PostfixOperator, QualifiedIdentifier, RangeMode, SpanWithoutFileId,
};
use swamp_script_semantic::prelude::*;

use swamp_script_source_map::SourceMap;

use swamp_script_semantic::{ResolvedNormalPattern, ResolvedRangeMode};
use tracing::{error, info};

#[derive(Debug)]
pub struct ResolvedProgram {
    pub state: ResolvedProgramState,
    pub modules: ResolvedModules,
}

impl Default for ResolvedProgram {
    fn default() -> Self {
        Self::new()
    }
}

impl ResolvedProgram {
    #[must_use]
    pub fn new() -> Self {
        Self {
            state: ResolvedProgramState::new(),
            modules: ResolvedModules::new(),
        }
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

#[derive(Debug, Eq, PartialEq)]
pub enum BlockScopeMode {
    Open,
    Closed,
}

#[derive(Debug)]
pub struct BlockScope {
    mode: BlockScopeMode,
    variables: SeqMap<String, ResolvedVariableRef>,
    constants: SeqMap<String, ResolvedConstantRef>,
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
            mode: BlockScopeMode::Open,
            variables: SeqMap::new(),
            constants: SeqMap::new(),
        }
    }
}

pub struct SharedState<'a> {
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
    #[must_use]
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
        state: &'a mut ResolvedProgramState,
        lookup: &'a mut NameLookup<'a>,
        source_map: &'a SourceMap,
        file_id: FileId,
    ) -> Self {
        let shared = SharedState {
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

    fn resolve_normal_if_statement(
        &mut self,
        condition: &Expression,
        true_expression: &Expression,
        maybe_false_expression: &Option<Box<Expression>>,
    ) -> Result<ResolvedExpression, ResolveError> {
        let condition = self.resolve_bool_expression(condition)?;

        // For the true branch
        let resolved_true = Box::new(self.resolve_expression(true_expression)?);

        // For the else branch
        let else_statements = if let Some(false_expression) = maybe_false_expression {
            Some(Box::new(self.resolve_expression(false_expression)?))
        } else {
            None
        };

        Ok(ResolvedExpression::If(
            condition,
            resolved_true,
            else_statements,
        ))
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
                    v.push(self.get_text(p).to_string());
                }
                v
            });
        (path, self.get_text(&ident.name.0).to_string())
    }

    fn resolve_return_type(&mut self, function: &Function) -> Result<ResolvedType, ResolveError> {
        let ast_return_type = match function {
            Function::Internal(x) => &x.declaration.return_type,
            Function::External(x) => &x.return_type,
        };

        let resolved_return_type = match ast_return_type {
            None => ResolvedType::Unit,
            Some(x) => self.resolve_type(x)?,
        };

        Ok(resolved_return_type)
    }

    fn resolve_statements_in_function(
        &mut self,
        expression: &Expression,
        return_type: &ResolvedType,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_statement = self.resolve_expression(expression)?;
        let wrapped_expr = Self::check_and_wrap_return_value(resolved_statement, return_type)?;

        Ok(wrapped_expr)
    }

    fn resolve_maybe_type(
        &mut self,
        maybe_type: &Option<Type>,
    ) -> Result<ResolvedType, ResolveError> {
        let found_type = match maybe_type {
            None => ResolvedType::Unit,
            Some(ast_type) => self.resolve_type(ast_type)?,
        };
        Ok(found_type)
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
        let expr_type = expr.resolution();

        // If return type is Unit, ignore the expression's type and return Unit
        // TODO: In future versions, always have a return statement
        if matches!(return_type, ResolvedType::Unit) {
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

    fn resolve_parameters(
        &mut self,
        parameters: &Vec<Parameter>,
    ) -> Result<Vec<ResolvedTypeForParameter>, ResolveError> {
        let mut resolved_parameters = Vec::new();
        for parameter in parameters {
            let param_type = self.resolve_type(&parameter.param_type)?;
            resolved_parameters.push(ResolvedTypeForParameter {
                name: self.get_text(&parameter.variable.name).to_string(),
                resolved_type: param_type,
                is_mutable: parameter.variable.is_mutable.is_some(),
                node: Some(ResolvedParameterNode {
                    is_mutable: self.to_node_option(&parameter.variable.is_mutable),
                    name: self.to_node(&parameter.variable.name),
                }),
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
            Err(ResolveError::NeedStructForFieldLookup(
                self.to_node(name).span,
            ))
        }
    }

    #[must_use]
    pub fn is_empty_array_literal(ast_expression: &Expression) -> bool {
        matches!(ast_expression, Expression::Literal(Literal::Array(items)) if items.is_empty())
    }

    /// # Errors
    ///
    pub fn resolve_expression_maybe_expecting_type(
        &mut self,
        ast_expression: &Expression,
        expected_type: Option<ResolvedType>,
        allow_some: bool,
    ) -> Result<ResolvedExpression, ResolveError> {
        if let Some(found_type) = expected_type {
            self.resolve_expression_expecting_type(ast_expression, &found_type, allow_some)
        } else {
            self.resolve_expression(ast_expression)
        }
    }

    /// # Errors
    ///
    pub fn resolve_expression_expecting_type(
        &mut self,
        ast_expression: &Expression,
        expected_type: &ResolvedType,
        allow_some_wrap: bool,
    ) -> Result<ResolvedExpression, ResolveError> {
        if Self::is_empty_array_literal(ast_expression) {
            match expected_type {
                ResolvedType::Map(map_type_ref) => Ok(ResolvedExpression::Literal(
                    ResolvedLiteral::Map(map_type_ref.clone(), vec![]),
                )),
                ResolvedType::Array(array_type_ref) => Ok(ResolvedExpression::Literal(
                    ResolvedLiteral::Array(array_type_ref.clone(), vec![]),
                )),
                _ => Err(ResolveError::EmptyArrayCanOnlyBeMapOrArray),
            }
        } else {
            let mut resolved_expr = self.resolve_expression(ast_expression)?;

            if allow_some_wrap {
                resolved_expr = wrap_in_some_if_optional(expected_type, resolved_expr);
            }
            let resolved_type = resolved_expr.resolution();

            if resolved_type.same_type(expected_type) {
                Ok(resolved_expr)
            } else {
                error!(?resolved_expr, ?expected_type, "incompatible types");
                Err(ResolveError::IncompatibleTypes(
                    resolved_expr.span(),
                    expected_type.clone(),
                ))
            }
        }
    }

    /// # Errors
    ///
    #[allow(clippy::too_many_lines)]
    pub fn resolve_expression(
        &mut self,
        ast_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let expression = match ast_expression {
            // Lookups
            Expression::FieldOrMemberAccess(expression, field_name) => {
                self.resolve_field_or_member_access(expression, field_name)?
            }

            Expression::IndexAccess(expression, index_expr) => {
                self.resolve_index_access(expression, index_expr)?
            }

            Expression::RangeAccess(expression, min_expr, max_expr, mode) => {
                self.resolve_range_access(expression, min_expr, max_expr, mode)?
            }
            Expression::VariableAccess(variable) => self.resolve_variable_like(&variable.name)?,

            Expression::StaticMemberFunctionReference(type_identifier, member_name) => {
                self.resolve_static_member_access(type_identifier, member_name)?
            }

            Expression::ConstantAccess(constant_identifier) => {
                self.resolve_constant_access(constant_identifier)?
            }

            Expression::FunctionAccess(qualified_identifier) => {
                self.resolve_function_access(qualified_identifier)?
            }

            Expression::MutRef(location_expression) => self.resolve_mut_ref(location_expression)?,

            // Assignments
            Expression::IndexAssignment(collection_expression, index_expression, source_expr) => {
                let resolved_collection_expression =
                    self.resolve_expression(collection_expression)?;

                let collection_resolution = resolved_collection_expression.resolution();

                let resolved_index_expression = self.resolve_expression(index_expression)?;

                let index_resolution = resolved_index_expression.resolution();

                let resolved_source_expression = self.resolve_expression(source_expr)?;
                let resolved_source_expression_type = resolved_source_expression.resolution();

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

            Expression::RangeAssignment(
                collection_expression,
                start_expression,
                end_expression,
                mode,
                source_expr,
            ) => {
                let resolved_collection_expression =
                    self.resolve_expression(collection_expression)?;

                let collection_resolution = resolved_collection_expression.resolution();

                let resolved_range_mode = match mode {
                    RangeMode::Inclusive => ResolvedRangeMode::Inclusive,
                    RangeMode::Exclusive => ResolvedRangeMode::Exclusive,
                };

                let resolved_start_expression = self.resolve_expression_expecting_type(
                    start_expression,
                    &ResolvedType::Int,
                    false,
                )?;
                let resolved_end_expression = self.resolve_expression_expecting_type(
                    end_expression,
                    &ResolvedType::Int,
                    false,
                )?;

                let resolved_source_expression = self.resolve_expression(source_expr)?;
                let resolved_source_expression_type = resolved_source_expression.resolution();

                match &collection_resolution {
                    ResolvedType::Array(target_array_type_ref) => {
                        if let ResolvedType::Array(found_source_array) =
                            resolved_source_expression_type
                        {
                            if !target_array_type_ref
                                .item_type
                                .same_type(&found_source_array.item_type)
                            {
                                return Err(ResolveError::IncompatibleTypes(
                                    resolved_source_expression.span(),
                                    collection_resolution.clone(),
                                ));
                            }
                            ResolvedExpression::AssignArrayRange(
                                Box::from(resolved_collection_expression),
                                target_array_type_ref.clone(),
                                Box::from(resolved_start_expression),
                                Box::from(resolved_end_expression),
                                resolved_range_mode,
                                Box::from(resolved_source_expression),
                            )
                        } else {
                            return Err(ResolveError::NotAnArray(
                                resolved_collection_expression.span(),
                            ));
                        }
                    }
                    ResolvedType::String => {
                        if !resolved_source_expression_type.same_type(&ResolvedType::String) {
                            return Err(ResolveError::IncompatibleTypes(
                                resolved_source_expression.span(),
                                collection_resolution.clone(),
                            ));
                        }
                        ResolvedExpression::AssignStringRange(
                            Box::from(resolved_collection_expression),
                            Box::from(resolved_start_expression),
                            Box::from(resolved_end_expression),
                            resolved_range_mode,
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

            Expression::NoneCoalesceOperator(base_expr, default_expr) => {
                let (base_expr, default_expr) =
                    self.resolve_none_coalesce_operator(base_expr, default_expr)?;
                ResolvedExpression::NoneCoalesceOperator(
                    Box::from(base_expr),
                    Box::from(default_expr),
                )
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

            Expression::StaticCallGeneric(type_name, function_name, arguments) => {
                if let Some(found) =
                    self.check_for_internal_static_call(type_name, function_name, arguments)?
                {
                    found
                } else {
                    ResolvedExpression::StaticCallGeneric(self.resolve_static_call_generic(
                        type_name,
                        function_name,
                        arguments,
                    )?)
                }
            }

            Expression::MemberOrFieldCall(ast_member_expression, ast_identifier, ast_arguments) => {
                self.member_or_field_call(ast_member_expression, ast_identifier, ast_arguments)?
            }
            Expression::Block(expressions) => {
                ResolvedExpression::Block(self.resolve_expressions(expressions)?)
            }

            Expression::With(variable_bindings, expression) => {
                self.resolve_with_expr(variable_bindings, expression)?
            }

            Expression::InterpolatedString(string_parts) => ResolvedExpression::InterpolatedString(
                self.resolve_interpolated_string(string_parts)?,
            ),

            // Creation
            Expression::StructInstantiation(struct_identifier, fields, has_rest) => {
                self.resolve_struct_instantiation(struct_identifier, fields, *has_rest)?
            }
            Expression::ExclusiveRange(min_value, max_value) => {
                let min_expression =
                    self.resolve_expression_expecting_type(min_value, &ResolvedType::Int, false)?;
                let max_expression =
                    self.resolve_expression_expecting_type(max_value, &ResolvedType::Int, false)?;
                ResolvedExpression::ExclusiveRange(
                    Box::from(min_expression),
                    Box::from(max_expression),
                )
            }

            Expression::InclusiveRange(min_value, max_value) => {
                let min_expression =
                    self.resolve_expression_expecting_type(min_value, &ResolvedType::Int, false)?;
                let max_expression =
                    self.resolve_expression_expecting_type(max_value, &ResolvedType::Int, false)?;
                ResolvedExpression::InclusiveRange(
                    Box::from(min_expression),
                    Box::from(max_expression),
                )
            }

            Expression::Literal(literal) => {
                ResolvedExpression::Literal(self.resolve_literal(literal)?)
            }

            Expression::ForLoop(pattern, iteratable_expression, statements) => {
                let resolved_iterator = self.resolve_iterator(&iteratable_expression.expression)?;

                self.push_block_scope("for_loop");
                let pattern = self.resolve_for_pattern(
                    pattern,
                    resolved_iterator.key_type.as_ref(),
                    &resolved_iterator.value_type,
                )?;
                let resolved_statements = self.resolve_expression(statements)?;
                self.pop_block_scope("for_loop");

                ResolvedExpression::ForLoop(
                    pattern,
                    resolved_iterator,
                    Box::from(resolved_statements),
                )
            }
            Expression::WhileLoop(expression, statements) => {
                let condition = self.resolve_bool_expression(expression)?;
                self.push_block_scope("while_loop");
                let resolved_statements = self.resolve_expression(statements)?;
                self.pop_block_scope("while_loop");

                ResolvedExpression::WhileLoop(condition, Box::from(resolved_statements))
            }
            Expression::Return(expr) => {
                let wrapped_expr = if let Some(found_expr) = expr {
                    let resolved_expr = self.resolve_expression(found_expr)?;
                    let return_type = self.current_function_return_type();
                    Some(Box::new(Self::check_and_wrap_return_value(
                        resolved_expr,
                        &return_type,
                    )?))
                } else {
                    None
                };
                ResolvedExpression::Return(wrapped_expr)
            }
            Expression::Break(node) => ResolvedExpression::Break(self.to_node(node)),
            Expression::Continue(node) => ResolvedExpression::Continue(self.to_node(node)),

            Expression::If(expression, true_expression, maybe_false_expression) => {
                match &**expression {
                    Expression::PostfixOp(PostfixOperator::Unwrap(unwrap_node), expr) => {
                        info!(?expr, "unwrap");
                        if let Expression::VariableAccess(var) = &**expr {
                            self.handle_optional_unwrap_statement(
                                var,
                                expr,
                                true_expression,
                                maybe_false_expression,
                            )?
                        } else {
                            Err(ResolveError::ExpectedVariable(self.to_node(unwrap_node)))?
                        }
                    }
                    Expression::VariableAssignment(target_variable, expr) => {
                        if let Expression::PostfixOp(
                            PostfixOperator::Unwrap(_unwrap_node),
                            inner_expr,
                        ) = &**expr
                        {
                            self.handle_optional_assign_unwrap_statement(
                                target_variable,
                                inner_expr,
                                true_expression,
                                maybe_false_expression,
                            )?
                        } else {
                            self.resolve_normal_if_statement(
                                expression,
                                true_expression,
                                maybe_false_expression,
                            )?
                        }
                    }
                    _ => self.resolve_normal_if_statement(
                        expression,
                        true_expression,
                        maybe_false_expression,
                    )?,
                }
            }

            Expression::Match(expression, arms) => {
                ResolvedExpression::Match(self.resolve_match(expression, arms)?)
            }
            Expression::Guard(guard_expressions, wildcard) => {
                self.resolve_guard(guard_expressions, wildcard)?
            }
        };

        Ok(expression)
    }

    #[allow(unused)]
    fn resolve_into_named_struct_ref(
        &mut self,
        struct_expression: &Expression,
    ) -> Result<(ResolvedStructTypeRef, ResolvedExpression), ResolveError> {
        let resolved = self.resolve_expression(struct_expression)?;

        let resolved_type = resolved.resolution();
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

        let resolved_type = resolved.resolution();
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

    fn get_struct_type(
        &self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        //   let namespace = self.get_namespace(qualified_type_identifier)?;

        let (path, name) = self.get_path(qualified_type_identifier);

        let struct_ref = self.shared.lookup.get_struct(&path, &name).ok_or_else(|| {
            let type_reference_node = self.to_node(&qualified_type_identifier.name.0);
            ResolveError::UnknownStructTypeReference(type_reference_node)
        })?;

        Ok(struct_ref)
    }

    fn create_default_value_for_type(
        field_type: &ResolvedType,
    ) -> Result<ResolvedExpression, ResolveError> {
        let expr = match field_type {
            ResolvedType::Bool => ResolvedExpression::Literal(ResolvedLiteral::BoolLiteral(
                false,
                ResolvedNode::new_unknown(),
            )),
            ResolvedType::Int => ResolvedExpression::Literal(ResolvedLiteral::IntLiteral(
                0,
                ResolvedNode::new_unknown(),
            )),
            ResolvedType::Float => ResolvedExpression::Literal(ResolvedLiteral::FloatLiteral(
                Fp::zero(),
                ResolvedNode::new_unknown(),
            )),
            ResolvedType::String => ResolvedExpression::Literal(ResolvedLiteral::StringLiteral(
                String::new(),
                ResolvedNode::new_unknown(),
            )),
            ResolvedType::Array(array_type_ref) => {
                ResolvedExpression::Literal(ResolvedLiteral::Array(array_type_ref.clone(), vec![]))
            }
            ResolvedType::Tuple(tuple_type_ref) => {
                let mut expressions = Vec::new();
                for resolved_type in &tuple_type_ref.0 {
                    let expr = Self::create_default_value_for_type(resolved_type)?;
                    expressions.push(expr);
                }
                ResolvedExpression::Literal(ResolvedLiteral::TupleLiteral(
                    tuple_type_ref.clone(),
                    expressions,
                ))
            }
            ResolvedType::Map(map_type_ref) => {
                ResolvedExpression::Literal(ResolvedLiteral::Map(map_type_ref.clone(), vec![]))
            }
            ResolvedType::Optional(_optional_type) => ResolvedExpression::Literal(
                ResolvedLiteral::NoneLiteral(ResolvedNode::new_unknown()),
            ),

            ResolvedType::Struct(struct_ref) => {
                let struct_ref_borrow = struct_ref.borrow();
                if let Some(function) = struct_ref_borrow.functions.get(&"default".to_string()) {
                    ResolvedExpression::StaticCall(ResolvedStaticCall {
                        function: function.clone(),
                        arguments: vec![],
                    })
                } else {
                    return Err(ResolveError::NoDefaultImplementedForStruct(
                        struct_ref.clone(),
                    ));
                }
            }
            _ => return Err(ResolveError::NoDefaultImplemented(field_type.clone())),
        };
        Ok(expr)
    }

    fn resolve_bool_expression(
        &mut self,
        expression: &Expression,
    ) -> Result<ResolvedBooleanExpression, ResolveError> {
        let resolved_expression = self.resolve_expression(expression)?;
        let expr_type = resolved_expression.resolution();

        let bool_expression = match expr_type {
            ResolvedType::Bool => resolved_expression,
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
            expression: Box::from(bool_expression),
        })
    }

    fn resolve_iterator(
        &mut self,
        expression: &Expression,
    ) -> Result<ResolvedIterator, ResolveError> {
        let resolved_expression = self.resolve_expression(expression)?;
        let resolved_type = resolved_expression.resolution();
        let (key_type, value_type): (Option<ResolvedType>, ResolvedType) = match resolved_type {
            ResolvedType::Array(array_type) => {
                (Some(ResolvedType::Int), array_type.item_type.clone())
            }
            ResolvedType::Map(map_type_ref) => (
                Some(map_type_ref.key_type.clone()),
                map_type_ref.value_type.clone(),
            ),
            ResolvedType::String => (Some(ResolvedType::Int), ResolvedType::String),
            ResolvedType::Iterable(item_type) => (None, *item_type),
            ResolvedType::Generic(_base_type, params) => {
                // TODO: HACK: We assume it is a container that iterates over the type parameters
                // TODO: HACK: We assume that it is a sparse map
                // TODO: HACK: Remove hardcoded number
                let rust_type_ref_for_id = self
                    .shared
                    .lookup
                    .get_rust_type(&["std".to_string()], "SparseId")
                    .expect("SparseId was missing");
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
            resolved_expression: Box::new(resolved_expression),
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
                StringPart::Literal(string_node, processed_string) => ResolvedStringPart::Literal(
                    self.to_node(string_node),
                    processed_string.to_string(),
                ),
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

    fn resolve_function_access(
        &self,
        function_ref_node: &QualifiedIdentifier,
    ) -> Result<ResolvedExpression, ResolveError> {
        let path = self.get_module_path(&function_ref_node.module_path);
        let name = self.get_text(&function_ref_node.name).to_string();
        self.shared
            .lookup
            .get_internal_function(&path, &name)
            .map_or_else(
                || {
                    self.shared
                        .lookup
                        .get_external_function_declaration(&path, &name)
                        .map_or_else(
                            || {
                                error!("unknown function {path:?} {name:?}");
                                Err(ResolveError::UnknownFunction(
                                    self.to_node(&function_ref_node.name),
                                ))
                            },
                            |external_function_ref| {
                                Ok(ResolvedExpression::ExternalFunctionAccess(
                                    external_function_ref,
                                ))
                            },
                        )
                },
                |function_ref| Ok(ResolvedExpression::InternalFunctionAccess(function_ref)),
            )
    }

    // The ast assumes it is something similar to a variable, but it can be a function reference as well.
    fn resolve_variable_like(&self, var_node: &Node) -> Result<ResolvedExpression, ResolveError> {
        let text = self.get_text(var_node);
        self.shared
            .lookup
            .get_internal_function(&[], text)
            .map_or_else(
                || {
                    self.shared
                        .lookup
                        .get_external_function_declaration(&[], text)
                        .map_or_else(
                            || {
                                self.try_find_variable(var_node).map_or(
                                    Err(ResolveError::UnknownVariable(self.to_node(var_node))),
                                    |variable_ref| {
                                        Ok(ResolvedExpression::VariableAccess(variable_ref))
                                    },
                                )
                            },
                            |found_external_function| {
                                Ok(ResolvedExpression::ExternalFunctionAccess(
                                    found_external_function,
                                ))
                            },
                        )
                },
                |found_internal_function| {
                    Ok(ResolvedExpression::InternalFunctionAccess(
                        found_internal_function,
                    ))
                },
            )
    }

    fn resolve_usize_index(
        &mut self,
        usize_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let lookup_expression =
            self.resolve_expression_expecting_type(usize_expression, &ResolvedType::Int, false)?;
        let lookup_resolution = lookup_expression.resolution();

        match &lookup_resolution {
            ResolvedType::Int => {}
            _ => Err(ResolveError::ArrayIndexMustBeInt(lookup_resolution))?,
        }

        Ok(lookup_expression)
    }

    fn resolve_array_type_helper(
        &mut self,
        items: &[Expression],
    ) -> Result<(ResolvedArrayTypeRef, Vec<ResolvedExpression>), ResolveError> {
        let expressions = self.resolve_expressions(items)?;
        let item_type = if expressions.is_empty() {
            ResolvedType::Any
        } else {
            expressions[0].resolution()
        };

        let array_type = ResolvedArrayType { item_type };

        let array_type_ref = Rc::new(array_type);

        Ok((array_type_ref, expressions))
    }

    fn push_block_scope(&mut self, _debug_str: &str) {
        self.scope.block_scope_stack.push(BlockScope {
            mode: BlockScopeMode::Open,
            variables: SeqMap::default(),
            constants: SeqMap::default(),
        });
    }

    fn pop_block_scope(&mut self, _debug_str: &str) {
        self.scope.block_scope_stack.pop();
    }

    fn push_closed_block_scope(&mut self) {
        self.scope.block_scope_stack.push(BlockScope {
            mode: BlockScopeMode::Closed,
            variables: SeqMap::default(),
            constants: SeqMap::default(),
        });
    }

    fn pop_closed_block_scope(&mut self) {
        self.scope.block_scope_stack.pop();
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
                        self.to_node(&qualified_type_identifier.name.0),
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
            EnumVariantLiteral::Struct(_qualified_name, variant_name, field_expressions) => {
                if let ResolvedEnumVariantContainerType::Struct(struct_ref) = &variant_ref.data {
                    let resolved = self.resolve_anon_struct_instantiation(
                        &variant_name.0.clone(),
                        &struct_ref.anon_struct,
                        field_expressions,
                        false,
                    )?;
                    ResolvedEnumLiteralData::Struct(resolved)
                } else {
                    return Err(ResolveError::WrongEnumVariantContainer(variant_ref.clone()));
                }
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
        let resolved_type = resolved_expression.resolution();

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
        expected_condition_type: &ResolvedType,
    ) -> Result<ResolvedMatchArm, ResolveError> {
        let (resolved_pattern, scope_was_pushed) =
            self.resolve_pattern(&arm.pattern, expected_condition_type)?;

        let resolved_expression = self.resolve_expression(&arm.expression)?;
        if scope_was_pushed {
            self.pop_block_scope("resolve_arm");
        }

        let resolved_type = resolved_expression.resolution();

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
        expected_condition_type: &ResolvedType,
    ) -> Result<ResolvedNormalPattern, ResolveError> {
        let resolved_literal = self.resolve_literal(ast_literal)?;

        let resolved_literal_copy = self.resolve_literal(ast_literal)?;
        let resolved_literal_expr = ResolvedExpression::Literal(resolved_literal_copy);
        let span = resolved_literal_expr.span();
        let literal_type = resolved_literal_expr.resolution();
        if !literal_type.same_type(expected_condition_type) {
            return Err(ResolveError::IncompatibleTypes(
                span,
                expected_condition_type.clone(),
            ));
        }
        Ok(ResolvedNormalPattern::Literal(resolved_literal))
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

    fn get_module_path(&self, module_path: &Option<ModulePath>) -> Vec<String> {
        module_path.as_ref().map_or_else(Vec::new, |found| {
            let mut strings = Vec::new();
            for path_item in &found.0 {
                strings.push(self.get_text(path_item).to_string());
            }
            strings
        })
    }

    fn get_enum_variant_type(
        &self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
        variant_name: &str,
    ) -> Option<ResolvedEnumVariantTypeRef> {
        let path = self.get_module_path(&qualified_type_identifier.module_path);

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
            CompoundOperatorKind::Modulo => ResolvedCompoundOperatorKind::Modulo,
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
        &mut self,
        location_expression: &LocationExpression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let mut_expr = match location_expression {
            LocationExpression::Variable(variable) => {
                let var = self.find_variable(variable)?;
                if !var.is_mutable() {
                    Err(ResolveError::VariableIsNotMutable(
                        self.to_node(&variable.name),
                    ))?;
                }
                let mut_var = ResolvedMutVariable { variable_ref: var };
                ResolvedExpression::MutVariableRef(Rc::new(mut_var))
            }
            LocationExpression::IndexAccess(expression, index_expr) => {
                let resolved_base = self.resolve_expression(expression)?;
                let resolved_type = resolved_base.resolution();
                match resolved_type {
                    ResolvedType::Array(_) => {
                        let (base_repr, access_chain, last_type) =
                            self.resolve_array_index_access_helper(expression, index_expr)?;
                        ResolvedExpression::MutArrayIndexRef(
                            base_repr.into(),
                            last_type,
                            access_chain,
                        )
                    }

                    ResolvedType::Map(map_type_ref) => {
                        let resolved_index_expr = self.resolve_expression(index_expr)?;
                        ResolvedExpression::MutMapIndexRef(
                            Box::from(resolved_base),
                            map_type_ref,
                            Box::from(resolved_index_expr),
                        )
                    }

                    _ => return Err(ResolveError::ExpectedMutableLocation(Span::dummy())),
                }
            }
            LocationExpression::FieldAccess(expression, node) => {
                let (base_repr, access_chain, field_ref) =
                    self.resolve_field_access_helper(expression, node)?;
                ResolvedExpression::MutStructFieldRef(base_repr.into(), field_ref, access_chain)
            }
        };

        Ok(mut_expr)
    }

    fn resolve_with_expr(
        &mut self,
        variables: &[VariableBinding],
        expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let mut variable_expressions = Vec::new();
        for variable in variables {
            let var = self.resolve_expression(&variable.expression)?;
            variable_expressions.push(var);
        }

        self.push_closed_block_scope();
        let mut expressions = Vec::new();
        for (variable_binding, resolved_expression) in variables.iter().zip(variable_expressions) {
            let initialize_variable_expression = self.resolve_variable_assignment_resolved(
                &variable_binding.variable,
                resolved_expression,
            )?;
            expressions.push(initialize_variable_expression);
        }

        let resolved_expression = self.resolve_expression(expression)?;
        expressions.push(resolved_expression);

        let block_expression = ResolvedExpression::Block(expressions);
        self.pop_closed_block_scope();

        Ok(block_expression)
    }

    fn resolve_guard(
        &mut self,
        guard_expressions: &Vec<GuardExpr>,
        wildcard: &Option<Box<Expression>>,
    ) -> Result<ResolvedExpression, ResolveError> {
        let mut expecting_type = None;
        let mut guards = Vec::new();
        for guard in guard_expressions {
            let resolved_condition = self.resolve_bool_expression(&guard.condition)?;
            let resolved_result = self.resolve_expression_maybe_expecting_type(
                &guard.result,
                expecting_type.clone(),
                true,
            )?;
            if expecting_type.is_none() {
                expecting_type = Some(resolved_result.resolution().clone());
            }

            guards.push(ResolvedGuard {
                condition: resolved_condition,
                result: resolved_result,
            });
        }

        let resolved_wildcard = if let Some(found_wildcard) = wildcard {
            Some(Box::new(self.resolve_expression_maybe_expecting_type(
                found_wildcard,
                expecting_type,
                true,
            )?))
        } else {
            None
        };

        Ok(ResolvedExpression::Guard(guards, resolved_wildcard))
    }

    fn resolve_none_coalesce_operator(
        &mut self,
        base_expr: &Expression,
        default_expr: &Expression,
    ) -> Result<(ResolvedExpression, ResolvedExpression), ResolveError> {
        let expr = self.resolve_expression(base_expr)?;
        let resolved_type = expr.resolution();

        if let ResolvedType::Optional(found_type) = resolved_type {
            let resolved_default_expr =
                self.resolve_expression_expecting_type(default_expr, &found_type, true)?;
            Ok((expr, resolved_default_expr))
        } else {
            Err(ResolveError::NoneCoalesceNeedsOptionalType(expr.span()))
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
            _ => {
                if let ResolvedExpression::Literal(ResolvedLiteral::NoneLiteral(_)) = resolved_value
                {
                    resolved_value
                } else {
                    ResolvedExpression::Option(Some(Box::new(resolved_value)))
                }
            }
        },
        _ => resolved_value,
    }
}
