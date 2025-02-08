/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod access;
pub mod call;
pub mod constant;
pub mod def;
pub mod err;
pub mod internal;
pub mod literal;
pub mod lookup;
pub mod operator;
pub mod pattern;
pub mod prelude;
mod structure;
pub mod types;
pub mod variable;

use crate::err::{ResolveError, ResolveErrorKind};
use crate::lookup::NameLookup;
use seq_map::SeqMap;
use std::mem::take;
use std::num::{ParseFloatError, ParseIntError};
use std::rc::Rc;
use swamp_script_ast::prelude::*;
use swamp_script_ast::{
    CompoundOperator, CompoundOperatorKind, EnumVariantLiteral, ExpressionKind, ForPattern,
    Function, LiteralKind, MutableOrImmutableExpression, Postfix, PostfixChain,
    QualifiedIdentifier, RangeMode, SpanWithoutFileId, WhenBinding,
};
use swamp_script_semantic::prelude::*;
use swamp_script_semantic::{
    ResolvedArgumentExpressionOrLocation, ResolvedLocationAccess, ResolvedLocationAccessKind,
    ResolvedMutOrImmutableExpression, ResolvedNormalPattern, ResolvedPostfix, ResolvedPostfixKind,
    ResolvedRangeMode, ResolvedSingleLocationExpression, ResolvedSingleLocationExpressionKind,
    ResolvedSingleMutLocationExpression, ResolvedTypeWithMut, ResolvedWhenBinding,
};
use swamp_script_source_map::SourceMap;
use tracing::error;
use tracing::info;

pub fn convert_range_mode(range_mode: &RangeMode) -> ResolvedRangeMode {
    match range_mode {
        RangeMode::Inclusive => ResolvedRangeMode::Inclusive,
        RangeMode::Exclusive => ResolvedRangeMode::Exclusive,
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum LocationSide {
    Lhs,
    Rhs,
}

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
pub const SPARSE_ID_TYPE_ID: TypeNumber = 998;

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
            scope: FunctionScopeState::new(ResolvedType::Unit),
            global: FunctionScopeState::new(ResolvedType::Unit),
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
        expected_type: Option<&ResolvedType>,
        true_expression: &Expression,
        maybe_false_expression: &Option<Box<Expression>>,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_condition = self.resolve_bool_expression(condition)?;

        let true_expr = self.resolve_expression(true_expression, expected_type)?;
        let if_result_type = true_expr.ty.clone();
        // For the true branch
        let resolved_true = Box::new(true_expr);

        // For the else branch
        let else_statements = if let Some(false_expression) = maybe_false_expression {
            Some(Box::new(self.resolve_expression(
                false_expression,
                Some(&if_result_type),
            )?))
        } else {
            None
        };

        Ok(self.create_expr(
            ResolvedExpressionKind::If(resolved_condition, resolved_true, else_statements),
            if_result_type,
            &condition.node,
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
        let resolved_statement = self.resolve_expression(expression, Some(return_type))?;
        //let wrapped_expr = Self::check_and_wrap_return_value(resolved_statement, return_type)?;

        Ok(resolved_statement)
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
                resolved_type: Some(param_type),
                is_mutable: parameter.variable.is_mutable.is_some(),
                node: Some(ResolvedParameterNode {
                    is_mutable: self.to_node_option(&parameter.variable.is_mutable),
                    name: self.to_node(&parameter.variable.name),
                }),
            });
        }
        Ok(resolved_parameters)
    }

    #[must_use]
    pub fn is_empty_array_literal(ast_expression: &Expression) -> bool {
        matches!(&ast_expression.kind, ExpressionKind::Literal(LiteralKind::Array(items)) if items.is_empty())
    }

    pub fn resolve_immutable(
        &mut self,
        ast_expression: &Expression,
        expected_type: &ResolvedType,
    ) -> Result<ResolvedExpression, ResolveError> {
        self.resolve_expression(ast_expression, Some(expected_type))
    }

    pub fn resolve_expression_get_mutability(
        &mut self,
        ast_expression: &Expression,
        expected_type: Option<&ResolvedType>,
    ) -> Result<(ResolvedExpression, bool), ResolveError> {
        let resolved = self.resolve_expression(ast_expression, expected_type)?;
        let mutability = match resolved.kind {
            ResolvedExpressionKind::VariableAccess(ref resolved_variable) => {
                resolved_variable.is_mutable()
            }
            _ => false,
        };

        Ok((resolved, mutability))
    }

    /// # Errors
    ///
    #[allow(clippy::too_many_lines)]
    pub fn resolve_expression(
        &mut self,
        ast_expression: &Expression,
        expected_type: Option<&ResolvedType>,
    ) -> Result<ResolvedExpression, ResolveError> {
        let expr = self.resolve_expression_internal(ast_expression, expected_type)?;
        //  info!(ty=?expr.ty, kind=?expr.kind, "resolved to expr");
        let encountered_type = expr.ty.clone();
        if let Some(found_expected_type) = expected_type {
            if found_expected_type.same_type(&encountered_type) {
                return Ok(expr);
            } else if !matches!(encountered_type, ResolvedType::Optional(_)) {
                // If an optional is expected, we can wrap it
                if let ResolvedType::Optional(expected_inner_type) = found_expected_type {
                    if encountered_type.same_type(expected_inner_type) {
                        let wrapped = self.create_expr(
                            ResolvedExpressionKind::Option(Option::from(Box::new(expr))),
                            found_expected_type.clone(),
                            &ast_expression.node,
                        );
                        return Ok(wrapped);
                    }
                }
            } else if matches!(found_expected_type, &ResolvedType::Bool) {
                if let ResolvedType::Optional(_inner_type) = encountered_type {
                    let wrapped = self.create_expr(
                        ResolvedExpressionKind::CoerceOptionToBool(Box::from(expr)),
                        ResolvedType::Bool,
                        &ast_expression.node,
                    );
                    return Ok(wrapped);
                }
            }
            error!(?expr, ?ast_expression, "expr");
            error!(
                ?found_expected_type,
                ?encountered_type,
                "incompatible types"
            );
            return Err(self.create_err(
                ResolveErrorKind::IncompatibleTypes(found_expected_type.clone(), encountered_type),
                &ast_expression.node,
            ));
        }

        Ok(expr)
    }

    /// # Errors
    ///
    #[allow(clippy::too_many_lines)]
    pub fn resolve_expression_internal(
        &mut self,
        ast_expression: &Expression,
        expected_type: Option<&ResolvedType>,
    ) -> Result<ResolvedExpression, ResolveError> {
        //info!(?ast_expression, "resolving");
        let expression = match &ast_expression.kind {
            // Lookups
            ExpressionKind::PostfixChain(postfix_chain) => {
                self.resolve_postfix_chain(postfix_chain)?
            }

            ExpressionKind::IdentifierReference(variable) => {
                self.resolve_identifier_reference(&variable.name)?
            }
            ExpressionKind::VariableDefinition(variable, coerce_type, source_expression) => {
                self.resolve_create_variable(variable, coerce_type, source_expression)?
            }
            ExpressionKind::VariableAssignment(variable, source_expression) => {
                self.resolve_variable_assignment(variable, source_expression)?
            }
            ExpressionKind::DestructuringAssignment(variables, expression) => {
                self.resolve_destructuring(&ast_expression.node, variables, expression)?
            }

            ExpressionKind::StaticMemberFunctionReference(type_identifier, member_name) => {
                self.resolve_static_member_access(type_identifier, member_name)?
            }

            ExpressionKind::ConstantReference(constant_identifier) => {
                self.resolve_constant_access(constant_identifier)?
            }

            ExpressionKind::FunctionReference(qualified_identifier) => {
                self.resolve_function_access(qualified_identifier)?
            }

            ExpressionKind::Assignment(location, source) => {
                self.resolve_assignment(location, source)?
            }
            ExpressionKind::CompoundAssignment(target, op, source) => {
                self.resolve_assignment_compound(target, op, source)?
            }

            // Operator
            ExpressionKind::BinaryOp(resolved_a, operator, resolved_b) => {
                let (resolved_op, result_type) =
                    self.resolve_binary_op(resolved_a, operator, resolved_b)?;

                self.create_expr(
                    ResolvedExpressionKind::BinaryOp(resolved_op),
                    result_type,
                    &ast_expression.node,
                )
            }
            ExpressionKind::UnaryOp(operator, expression) => {
                let (resolved_op, result_type) = self.resolve_unary_op(operator, expression)?;
                self.create_expr(
                    ResolvedExpressionKind::UnaryOp(resolved_op),
                    result_type,
                    &ast_expression.node,
                )
            }

            ExpressionKind::Block(expressions) => {
                let (block, resulting_type) =
                    self.resolve_block(&ast_expression.node, expected_type, expressions)?;
                self.create_expr(
                    ResolvedExpressionKind::Block(block),
                    resulting_type.clone(),
                    &ast_expression.node,
                )
            }

            ExpressionKind::With(variable_bindings, expression) => {
                self.resolve_with_expr(expected_type, variable_bindings, expression)?
            }

            ExpressionKind::When(variable_bindings, true_expr, else_expr) => {
                self.resolve_when_expr(expected_type, variable_bindings, true_expr, else_expr)?
            }

            ExpressionKind::InterpolatedString(string_parts) => {
                let kind = ResolvedExpressionKind::InterpolatedString(
                    self.resolve_interpolated_string(string_parts)?,
                );

                self.create_expr(kind, ResolvedType::String, &ast_expression.node)
            }

            // Creation
            ExpressionKind::StructLiteral(struct_identifier, fields, has_rest) => {
                self.resolve_struct_instantiation(struct_identifier, fields, *has_rest)?
            }

            ExpressionKind::Range(min_value, max_value, range_mode) => {
                let range = self.resolve_range(min_value, max_value, range_mode)?;
                self.create_expr(
                    ResolvedExpressionKind::Range(
                        Box::from(range.min),
                        Box::from(range.max),
                        range.mode,
                    ),
                    ResolvedType::Iterable(Box::from(ResolvedType::Int)),
                    &ast_expression.node,
                )
            }

            ExpressionKind::Literal(literal) => {
                let (literal, resolved_type) =
                    self.resolve_literal(&ast_expression.node, literal, expected_type)?;
                self.create_expr(
                    ResolvedExpressionKind::Literal(literal),
                    resolved_type,
                    &ast_expression.node,
                )
            }

            ExpressionKind::ForLoop(pattern, iteratable_expression, statements) => {
                let resolved_iterator =
                    self.resolve_iterable(pattern.any_mut(), &iteratable_expression.expression)?;

                self.push_block_scope("for_loop");
                let pattern = self.resolve_for_pattern(
                    pattern,
                    resolved_iterator.key_type.as_ref(),
                    &resolved_iterator.value_type,
                )?;
                let resolved_statements = self.resolve_expression(statements, expected_type)?;
                self.pop_block_scope("for_loop");
                let resolved_type = resolved_statements.ty.clone();
                self.create_expr(
                    ResolvedExpressionKind::ForLoop(
                        pattern,
                        resolved_iterator,
                        Box::from(resolved_statements),
                    ),
                    resolved_type,
                    &ast_expression.node,
                )
            }
            ExpressionKind::WhileLoop(expression, statements) => {
                let condition = self.resolve_bool_expression(expression)?;
                self.push_block_scope("while_loop");
                let resolved_statements = self.resolve_expression(statements, expected_type)?;
                let resolved_type = resolved_statements.ty.clone();
                self.pop_block_scope("while_loop");

                self.create_expr(
                    ResolvedExpressionKind::WhileLoop(condition, Box::from(resolved_statements)),
                    resolved_type,
                    &ast_expression.node,
                )
            }
            ExpressionKind::Return(expr) => {
                let (wrapped_expr, return_type) = if let Some(found_expr) = expr {
                    let return_type = self.current_function_return_type();
                    let resolved_expr = self.resolve_expression(found_expr, Some(&return_type))?;
                    (Some(Box::new(resolved_expr)), return_type)
                } else {
                    (None, ResolvedType::Unit)
                };
                self.create_expr(
                    ResolvedExpressionKind::Return(wrapped_expr),
                    return_type,
                    &ast_expression.node,
                )
            }
            ExpressionKind::Break => self.create_expr(
                ResolvedExpressionKind::Break,
                ResolvedType::Unit,
                &ast_expression.node,
            ),
            ExpressionKind::Continue => self.create_expr(
                ResolvedExpressionKind::Continue,
                ResolvedType::Unit,
                &ast_expression.node,
            ),

            ExpressionKind::If(expression, true_expression, maybe_false_expression) => self
                .resolve_normal_if_statement(
                    expression,
                    expected_type,
                    true_expression,
                    maybe_false_expression,
                )?,

            ExpressionKind::Match(expression, arms) => {
                let (match_expr, return_type) =
                    self.resolve_match(expression, expected_type, arms)?;
                self.create_expr(
                    ResolvedExpressionKind::Match(match_expr),
                    return_type,
                    &ast_expression.node,
                )
            }
            ExpressionKind::Guard(guard_expressions) => {
                self.resolve_guard(&ast_expression.node, expected_type, guard_expressions)?
            }
        };

        //info!(ty=%expression.ty, kind=?expression.kind,  "resolved expression");

        Ok(expression)
    }

    #[allow(unused)]
    fn resolve_into_named_struct_ref(
        &mut self,
        struct_expression: &Expression,
    ) -> Result<(ResolvedStructTypeRef, ResolvedExpression), ResolveError> {
        let resolved = self.resolve_expression(struct_expression, None)?;

        let resolved_type = resolved.ty.clone();
        match resolved_type {
            ResolvedType::Struct(named_struct) => Ok((named_struct, resolved)),
            _ => Err(self.create_err(
                ResolveErrorKind::NotNamedStruct(resolved_type.clone()),
                &struct_expression.node,
            )),
        }
    }

    fn get_struct_type(
        &self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        //   let namespace = self.get_namespace(qualified_type_identifier)?;

        let (path, name) = self.get_path(qualified_type_identifier);

        let struct_ref = self.shared.lookup.get_struct(&path, &name).ok_or_else(|| {
            self.create_err(
                ResolveErrorKind::UnknownStructTypeReference,
                &qualified_type_identifier.name.0,
            )
        })?;

        Ok(struct_ref)
    }

    fn create_default_value_for_type(
        &mut self,
        node: &Node,
        field_type: &ResolvedType,
    ) -> Result<ResolvedExpression, ResolveError> {
        let kind = match field_type {
            ResolvedType::Bool => {
                ResolvedExpressionKind::Literal(ResolvedLiteral::BoolLiteral(false))
            }
            ResolvedType::Int => ResolvedExpressionKind::Literal(ResolvedLiteral::IntLiteral(0)),
            ResolvedType::Float => {
                ResolvedExpressionKind::Literal(ResolvedLiteral::FloatLiteral(Fp::zero()))
            }
            ResolvedType::String => {
                ResolvedExpressionKind::Literal(ResolvedLiteral::StringLiteral(String::new()))
            }
            ResolvedType::Array(array_type_ref) => ResolvedExpressionKind::Literal(
                ResolvedLiteral::Array(array_type_ref.clone(), vec![]),
            ),
            ResolvedType::Tuple(tuple_type_ref) => {
                let mut expressions = Vec::new();
                for resolved_type in &tuple_type_ref.0 {
                    let expr = self.create_default_value_for_type(node, resolved_type)?;
                    expressions.push(expr);
                }
                ResolvedExpressionKind::Literal(ResolvedLiteral::TupleLiteral(
                    tuple_type_ref.clone(),
                    expressions,
                ))
            }
            ResolvedType::Map(map_type_ref) => {
                ResolvedExpressionKind::Literal(ResolvedLiteral::Map(map_type_ref.clone(), vec![]))
            }
            ResolvedType::Optional(_optional_type) => {
                ResolvedExpressionKind::Literal(ResolvedLiteral::NoneLiteral)
            }

            ResolvedType::Struct(struct_ref) => {
                self.create_default_static_call(node, struct_ref)?
            }
            _ => {
                return Err(self.create_err(
                    ResolveErrorKind::NoDefaultImplemented(field_type.clone()),
                    node,
                ))
            }
        };

        let expr = self.create_expr(kind, field_type.clone(), node);
        Ok(expr)
    }

    fn create_default_static_call(
        &mut self,
        node: &Node,
        struct_ref_borrow: &ResolvedStructTypeRef,
    ) -> Result<ResolvedExpressionKind, ResolveError> {
        if let Some(function) = struct_ref_borrow
            .borrow()
            .functions
            .get(&"default".to_string())
        {
            let kind = match &**function {
                ResolvedFunction::Internal(internal_function) => {
                    ResolvedExpressionKind::InternalFunctionAccess(internal_function.clone())
                }
                ResolvedFunction::External(external_function) => {
                    ResolvedExpressionKind::ExternalFunctionAccess(external_function.clone())
                }
            };

            let base_expr = self.create_expr(
                kind,
                ResolvedType::Function(function.signature().clone()),
                node,
            );

            let empty_call_postfix = ResolvedPostfix {
                node: self.to_node(node),
                ty: *function.signature().return_type.clone(),
                kind: ResolvedPostfixKind::FunctionCall(vec![]),
            };

            let kind =
                ResolvedExpressionKind::PostfixChain(Box::new(base_expr), vec![empty_call_postfix]);

            Ok(kind)
        } else {
            Err(self.create_err(
                ResolveErrorKind::NoDefaultImplementedForStruct(struct_ref_borrow.clone()),
                node,
            ))
        }
    }

    fn add_postfix(
        &mut self,
        vec: &mut Vec<ResolvedPostfix>,
        kind: ResolvedPostfixKind,
        ty: ResolvedType,
        node: &Node,
    ) {
        let resolved_node = self.to_node(node);
        let postfix = ResolvedPostfix {
            node: resolved_node,
            ty,
            kind,
        };

        vec.push(postfix)
    }

    pub fn resolve_struct_field(
        &mut self,
        field_name: &Node,
        tv: ResolvedType,
    ) -> Result<(ResolvedStructTypeRef, usize, ResolvedType), ResolveError> {
        let field_name_str = self.get_text(&field_name).to_string();

        if let ResolvedType::Struct(struct_type) = &tv {
            if let Some(found_field) = struct_type
                .borrow()
                .anon_struct_type
                .defined_fields
                .get(&field_name_str)
            {
                let index = struct_type
                    .borrow()
                    .anon_struct_type
                    .defined_fields
                    .get_index(&field_name_str)
                    .expect("checked earlier");

                return Ok((struct_type.clone(), index, found_field.field_type.clone()));
            }
        }

        Err(self.create_err(ResolveErrorKind::UnknownStructField, field_name))
    }

    #[allow(clippy::too_many_lines)]
    fn resolve_postfix_chain(
        &mut self,
        chain: &PostfixChain,
    ) -> Result<ResolvedExpression, ResolveError> {
        if let ExpressionKind::StaticMemberFunctionReference(
            qualified_type_reference,
            member_name,
        ) = &chain.base.kind
        {
            if let Some(found_expr) =
                self.check_for_internal_static_call(qualified_type_reference, member_name, &vec![])?
            {
                return Ok(found_expr);
            }
        }

        let (start, is_mutable) = self.resolve_expression_get_mutability(&chain.base, None)?;

        let mut tv = ResolvedTypeWithMut {
            resolved_type: start.ty.clone(),
            is_mutable,
        };

        let mut uncertain = false;

        let mut suffixes = Vec::new();

        for item in &chain.postfixes {
            match item {
                Postfix::FieldAccess(field_name) => {
                    let (struct_type_ref, index, return_type) =
                        self.resolve_struct_field(&field_name.clone(), tv.resolved_type)?;
                    self.add_postfix(
                        &mut suffixes,
                        ResolvedPostfixKind::StructField(struct_type_ref.clone(), index),
                        return_type.clone(),
                        field_name,
                    );

                    tv.resolved_type = return_type.clone();
                    // keep previous `is_mutable`
                }
                Postfix::MemberCall(member_name, ast_arguments) => {
                    let dereference = ast_arguments
                        .iter()
                        .map(|x| &x.expression)
                        .collect::<Vec<_>>();
                    if let Some(found_internal) = self.check_for_internal_member_call(
                        &tv.resolved_type,
                        tv.is_mutable,
                        member_name,
                        &dereference,
                    )? {
                        tv.resolved_type = found_internal.ty.clone();
                        tv.is_mutable = false;
                        suffixes.push(found_internal);
                    } else if let ResolvedType::Struct(struct_type) = &tv.resolved_type.clone() {
                        let return_type = self.resolve_postfix_member_call(
                            struct_type,
                            tv.is_mutable,
                            member_name,
                            ast_arguments,
                            &mut suffixes,
                        )?;

                        //self.add_postfix(&mut suffixes, kind, return_type.clone(), member_name);
                        tv.resolved_type = return_type.clone();
                        tv.is_mutable = false;
                    } else {
                        return Err(self.create_err(
                            ResolveErrorKind::NotValidLocationStartingPoint,
                            member_name,
                        ));
                    }
                }
                Postfix::FunctionCall(node, arguments) => {
                    if let ResolvedType::Function(signature) = &tv.resolved_type {
                        let resolved_node = self.to_node(node);
                        let resolved_arguments = self.resolve_and_verify_parameters(
                            &resolved_node,
                            &signature.parameters,
                            arguments,
                        )?;
                        self.add_postfix(
                            &mut suffixes,
                            ResolvedPostfixKind::FunctionCall(resolved_arguments),
                            *signature.return_type.clone(),
                            &node,
                        );

                        tv.resolved_type = *signature.return_type.clone();
                        tv.is_mutable = false;
                    };
                }

                Postfix::Subscript(index_expr) => {
                    let collection_type = tv.resolved_type.clone();
                    match &collection_type {
                        ResolvedType::String => {
                            if let ExpressionKind::Range(min, max, mode) = &index_expr.kind {
                                let range = self.resolve_range(min, max, mode)?;

                                self.add_postfix(
                                    &mut suffixes,
                                    ResolvedPostfixKind::StringRangeIndex(range),
                                    collection_type.clone(),
                                    &index_expr.node,
                                );

                                tv.resolved_type = ResolvedType::String;
                            } else {
                                let resolved_index_expr =
                                    self.resolve_expression(index_expr, Some(&ResolvedType::Int))?;
                                self.add_postfix(
                                    &mut suffixes,
                                    ResolvedPostfixKind::StringIndex(resolved_index_expr),
                                    ResolvedType::String,
                                    &index_expr.node,
                                );

                                tv.resolved_type = ResolvedType::String;
                            }

                            tv.is_mutable = false;
                        }

                        ResolvedType::Array(array_type_ref) => {
                            if let ExpressionKind::Range(min_expr, max_expr, mode) =
                                &index_expr.kind
                            {
                                let range = self.resolve_range(min_expr, max_expr, mode)?;

                                self.add_postfix(
                                    &mut suffixes,
                                    ResolvedPostfixKind::ArrayRangeIndex(
                                        array_type_ref.clone(),
                                        range,
                                    ),
                                    collection_type.clone(),
                                    &index_expr.node,
                                );

                                tv.resolved_type = collection_type.clone();
                            } else {
                                let resolved_index_expr =
                                    self.resolve_expression(index_expr, Some(&ResolvedType::Int))?;
                                self.add_postfix(
                                    &mut suffixes,
                                    ResolvedPostfixKind::ArrayIndex(
                                        array_type_ref.clone(),
                                        resolved_index_expr,
                                    ),
                                    array_type_ref.item_type.clone(),
                                    &index_expr.node,
                                );

                                tv.resolved_type = array_type_ref.item_type.clone();
                            }

                            tv.is_mutable = false;
                        }

                        ResolvedType::Map(map_type_ref) => {
                            let resolved_key_expr =
                                self.resolve_expression(index_expr, Some(&map_type_ref.key_type))?;
                            let return_type =
                                ResolvedType::Optional(Box::from(map_type_ref.value_type.clone()));
                            self.add_postfix(
                                &mut suffixes,
                                ResolvedPostfixKind::MapIndex(
                                    map_type_ref.clone(),
                                    resolved_key_expr,
                                ),
                                return_type.clone(),
                                &index_expr.node,
                            );

                            tv.resolved_type = return_type;
                            tv.is_mutable = false;
                        }

                        ResolvedType::Generic(base, generic_type_parameters) => match &**base {
                            ResolvedType::RustType(found_rust_type) => {
                                if found_rust_type.number == SPARSE_TYPE_ID {
                                    let sparse_id = self
                                        .shared
                                        .lookup
                                        .get_rust_type(&["std".to_string()], "SparseId")
                                        .expect("SparseId is missing");
                                    let contained_type = &generic_type_parameters[0];
                                    let resolved_key = self.resolve_expression(
                                        index_expr,
                                        Some(&ResolvedType::RustType(sparse_id)),
                                    )?;

                                    let return_type =
                                        ResolvedType::Optional(Box::new(contained_type.clone()));

                                    self.add_postfix(
                                        &mut suffixes,
                                        ResolvedPostfixKind::RustTypeIndexRef(
                                            found_rust_type.clone(),
                                            resolved_key,
                                        ),
                                        return_type.clone(),
                                        &index_expr.node,
                                    );

                                    tv.resolved_type = return_type;
                                    tv.is_mutable = false;
                                } else {
                                    panic!("unknown generic type lookup")
                                }
                            }
                            _ => panic!("not supported"),
                        },
                        _ => {
                            return Err(self.create_err(
                                ResolveErrorKind::ExpectedArray(collection_type),
                                &index_expr.node,
                            ));
                        }
                    }
                }

                Postfix::NoneCoalesce(default_expr) => {
                    let unwrapped_type = if let ResolvedType::Optional(unwrapped_type) =
                        &tv.resolved_type
                    {
                        unwrapped_type
                    } else if uncertain {
                        &tv.resolved_type
                    } else {
                        return Err(self
                            .create_err(ResolveErrorKind::CanNotNoneCoalesce, &default_expr.node));
                    };

                    let resolved_default_expr =
                        self.resolve_expression(default_expr, Some(unwrapped_type))?;
                    self.add_postfix(
                        &mut suffixes,
                        ResolvedPostfixKind::NoneCoalesce(resolved_default_expr),
                        unwrapped_type.clone(),
                        &default_expr.node,
                    );
                    tv.resolved_type = unwrapped_type.clone();
                    uncertain = false; // the chain is safe, because this will always solve None
                }

                Postfix::OptionUnwrap(option_node) => {
                    if let ResolvedType::Optional(unwrapped_type) = &tv.resolved_type {
                        uncertain = true;
                        self.add_postfix(
                            &mut suffixes,
                            ResolvedPostfixKind::OptionUnwrap,
                            *unwrapped_type.clone(),
                            option_node,
                        );
                        tv.resolved_type = *unwrapped_type.clone();
                    } else {
                        return Err(
                            self.create_err(ResolveErrorKind::ExpectedOptional, option_node)
                        );
                    }
                }
            }
        }

        if uncertain {
            if let ResolvedType::Optional(_) = tv.resolved_type {
            } else {
                tv.resolved_type = ResolvedType::Optional(Box::from(tv.resolved_type.clone()));
            }
        }

        Ok(self.create_expr(
            ResolvedExpressionKind::PostfixChain(Box::new(start), suffixes),
            tv.resolved_type,
            &chain.base.node,
        ))
    }

    fn resolve_bool_expression(
        &mut self,
        expression: &Expression,
    ) -> Result<ResolvedBooleanExpression, ResolveError> {
        let resolved_expression = self.resolve_expression(expression, Some(&ResolvedType::Bool))?;
        let expr_type = resolved_expression.ty.clone();

        let bool_expression = match expr_type {
            ResolvedType::Bool => resolved_expression,
            ResolvedType::Optional(_) => self.create_expr(
                ResolvedExpressionKind::CoerceOptionToBool(Box::new(resolved_expression)),
                ResolvedType::Bool,
                &expression.node,
            ),
            _ => {
                return Err(self.create_err(
                    ResolveErrorKind::ExpectedBooleanExpression,
                    &expression.node,
                ))
            }
        };

        Ok(ResolvedBooleanExpression {
            expression: Box::from(bool_expression),
        })
    }

    fn resolve_iterable(
        &mut self,
        force_mut: Option<Node>,
        expression: &MutableOrImmutableExpression,
    ) -> Result<ResolvedIterable, ResolveError> {
        let resolved_expression: ResolvedMutOrImmutableExpression = if force_mut.is_some() {
            let resolved_node = self.to_node(&force_mut.unwrap());
            ResolvedMutOrImmutableExpression {
                expression_or_location: ResolvedArgumentExpressionOrLocation::Location(
                    self.resolve_to_location(&expression.expression, None, LocationSide::Rhs)?,
                ),
                is_mutable: Some(resolved_node),
            }
        } else {
            self.resolve_mut_or_immutable_expression(expression, None, LocationSide::Rhs)?
        };

        let resolved_type = &resolved_expression.ty().clone();
        let (key_type, value_type): (Option<ResolvedType>, ResolvedType) = match resolved_type {
            ResolvedType::Array(array_type) => {
                (Some(ResolvedType::Int), array_type.item_type.clone())
            }
            ResolvedType::Map(map_type_ref) => (
                Some(map_type_ref.key_type.clone()),
                map_type_ref.value_type.clone(),
            ),
            ResolvedType::String => (Some(ResolvedType::Int), ResolvedType::String),
            ResolvedType::Iterable(item_type) => (None, *item_type.clone()),
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
            }
            _ => {
                return Err(
                    self.create_err(ResolveErrorKind::NotAnIterator, &expression.expression.node)
                )
            }
        };

        Ok(ResolvedIterable {
            key_type,
            value_type,
            resolved_expression: Box::new(resolved_expression),
        })
    }

    fn resolve_expressions(
        &mut self,
        expected_type: Option<&ResolvedType>,
        ast_expressions: &[Expression],
    ) -> Result<Vec<ResolvedExpression>, ResolveError> {
        let mut resolved_expressions = Vec::new();
        for expression in ast_expressions {
            resolved_expressions.push(self.resolve_expression(expression, expected_type)?);
        }
        Ok(resolved_expressions)
    }

    fn resolve_block(
        &mut self,
        node: &Node,
        expected_type_for_last: Option<&ResolvedType>,
        ast_expressions: &[Expression],
    ) -> Result<(Vec<ResolvedExpression>, ResolvedType), ResolveError> {
        if ast_expressions.is_empty() {
            if expected_type_for_last == Some(&ResolvedType::Unit) {
                return Ok((vec![], ResolvedType::Unit));
            } else {
                return Err(self.create_err(ResolveErrorKind::EmptyBlockWrongType, node));
            }
        }
        let mut resolved_expressions = Vec::new();

        // need special handling for the last expression
        let (last, all_but_last) = ast_expressions.split_last().unwrap();

        for expression in all_but_last {
            resolved_expressions.push(self.resolve_expression(expression, None)?);
        }
        let last_expr = self.resolve_expression(last, expected_type_for_last)?;
        let last_type = last_expr.ty.clone();
        resolved_expressions.push(last_expr);
        Ok((resolved_expressions, last_type))
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
                    let expr = self.resolve_expression(expression, None)?;
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
                                Err(self.create_err(
                                    ResolveErrorKind::UnknownFunction,
                                    &function_ref_node.name,
                                ))
                            },
                            |external_function_ref| {
                                Ok(self.create_expr(
                                    ResolvedExpressionKind::ExternalFunctionAccess(
                                        external_function_ref.clone(),
                                    ),
                                    *external_function_ref.signature.return_type.clone(),
                                    &function_ref_node.name,
                                ))
                            },
                        )
                },
                |function_ref| {
                    Ok(self.create_expr(
                        ResolvedExpressionKind::InternalFunctionAccess(function_ref.clone()),
                        *function_ref.signature.return_type.clone(),
                        &function_ref_node.name,
                    ))
                },
            )
    }

    // The ast assumes it is something similar to a variable, but it can be a function reference as well.
    fn resolve_identifier_reference(
        &self,
        var_node: &Node,
    ) -> Result<ResolvedExpression, ResolveError> {
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
                                self.try_find_variable(var_node).map_or_else(
                                    || {
                                        Err(self.create_err(
                                            ResolveErrorKind::UnknownVariable,
                                            var_node,
                                        ))
                                    },
                                    |variable_ref| {
                                        let deref = self.create_expr(
                                            ResolvedExpressionKind::VariableAccess(
                                                variable_ref.clone(),
                                            ),
                                            variable_ref.resolved_type.clone(),
                                            var_node,
                                        );
                                        Ok(deref)
                                    },
                                )
                            },
                            |found_external_function| {
                                Ok(self.create_expr(
                                    ResolvedExpressionKind::ExternalFunctionAccess(
                                        found_external_function.clone(),
                                    ),
                                    ResolvedType::Function(
                                        found_external_function.signature.clone(),
                                    ),
                                    &var_node,
                                ))
                            },
                        )
                },
                |found_internal_function| {
                    Ok(self.create_expr(
                        ResolvedExpressionKind::InternalFunctionAccess(
                            found_internal_function.clone(),
                        ),
                        ResolvedType::Function(found_internal_function.signature.clone()),
                        &var_node,
                    ))
                },
            )
    }

    fn resolve_usize_index(
        &mut self,
        usize_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let lookup_expression =
            self.resolve_expression(usize_expression, Some(&ResolvedType::Int))?;
        let lookup_resolution = lookup_expression.ty.clone();

        match &lookup_resolution {
            ResolvedType::Int => {}
            _ => Err(self.create_err(
                ResolveErrorKind::ArrayIndexMustBeInt(lookup_resolution),
                &usize_expression.node,
            ))?,
        }

        Ok(lookup_expression)
    }

    fn resolve_array_type_helper(
        &mut self,
        node: &Node,
        items: &[Expression],
        expected_type: Option<&ResolvedType>,
    ) -> Result<(ResolvedArrayTypeRef, Vec<ResolvedExpression>), ResolveError> {
        let expressions = self.resolve_expressions(None, items)?;
        let item_type = if expressions.is_empty() {
            if let Some(found_expected_type) = expected_type {
                info!(?found_expected_type, "found array type");
                if let ResolvedType::Array(found) = found_expected_type {
                    found.item_type.clone()
                } else {
                    return Err(self.create_err(ResolveErrorKind::NotAnArray, node));
                }
            } else {
                return Err(self.create_err(ResolveErrorKind::NotAnArray, node));
            }
        } else {
            expressions[0].ty.clone()
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
                    Err(self.create_err(
                        ResolveErrorKind::UnknownEnumVariantType,
                        &qualified_type_identifier.name.0,
                    ))
                },
                Ok,
            )
    }

    fn resolve_enum_ref(
        &self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
    ) -> Result<ResolvedEnumTypeRef, ResolveError> {
        //let variant_name_string = self.get_text(&qualified_type_identifier.name.0).to_string();
        self.get_enum_type(qualified_type_identifier).map_or_else(
            || {
                Err(self.create_err(
                    ResolveErrorKind::UnknownEnumVariantType,
                    &qualified_type_identifier.name.0,
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
                ResolvedEnumLiteralData::Tuple(self.resolve_expressions(None, expressions)?)
            }
            EnumVariantLiteral::Struct(_qualified_name, variant_name, field_expressions) => {
                if let ResolvedEnumVariantType::Struct(struct_ref) = &*variant_ref {
                    let resolved = self.resolve_anon_struct_instantiation(
                        &variant_name.0.clone(),
                        &struct_ref.anon_struct,
                        field_expressions,
                        false,
                    )?;
                    ResolvedEnumLiteralData::Struct(resolved)
                } else {
                    return Err(self.create_err(
                        ResolveErrorKind::WrongEnumVariantContainer(variant_ref.clone()),
                        &variant_name.0,
                    ));
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
        condition_expression: &Expression,
        expected_type: Option<&ResolvedType>,
        arms: &Vec<MatchArm>,
    ) -> Result<(ResolvedMatch, ResolvedType), ResolveError> {
        let resolved_expression = self.resolve_expression(condition_expression, None)?;
        let resolved_type = resolved_expression.ty.clone();

        let mut resolved_arms = Vec::new();

        let mut detected_expected_type: Option<ResolvedType> = expected_type.cloned();

        for arm in arms {
            let resolved_arm = self.resolve_arm(
                arm,
                &resolved_expression,
                detected_expected_type.as_ref(),
                &resolved_type,
            )?;
            detected_expected_type = Some(resolved_arm.expression.ty.clone());
            resolved_arms.push(resolved_arm);
        }

        Ok((
            ResolvedMatch {
                expression: Box::new(resolved_expression),
                arms: resolved_arms,
            },
            detected_expected_type.expect("match arm failed"),
        ))
    }

    fn resolve_arm(
        &mut self,
        arm: &MatchArm,
        _expression: &ResolvedExpression,
        expected_return_type: Option<&ResolvedType>,
        expected_condition_type: &ResolvedType,
    ) -> Result<ResolvedMatchArm, ResolveError> {
        let (resolved_pattern, scope_was_pushed) =
            self.resolve_pattern(&arm.pattern, expected_condition_type)?;

        let resolved_expression = self.resolve_expression(&arm.expression, expected_return_type)?;
        if scope_was_pushed {
            self.pop_block_scope("resolve_arm");
        }

        let resolved_type = resolved_expression.ty.clone();

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
        node: &Node,
        ast_literal: &LiteralKind,
        expected_condition_type: &ResolvedType,
    ) -> Result<ResolvedNormalPattern, ResolveError> {
        let (resolved_literal, literal_type) =
            self.resolve_literal(node, ast_literal, Some(expected_condition_type))?;

        if !literal_type.same_type(expected_condition_type) {
            return Err(self.create_err(
                ResolveErrorKind::IncompatibleTypes(literal_type, expected_condition_type.clone()),
                node,
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
            markdown_doc: None,
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

    fn get_enum_type(
        &self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
    ) -> Option<ResolvedEnumTypeRef> {
        let path = self.get_module_path(&qualified_type_identifier.module_path);

        let enum_name = self.get_text(&qualified_type_identifier.name.0).to_string();

        self.shared.lookup.get_enum(&path, &enum_name)
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

    fn resolve_format_specifier(
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

    fn resolve_with_expr(
        &mut self,
        expected_type: Option<&ResolvedType>,
        variables: &[VariableBinding],
        expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let mut variable_expressions = Vec::new();

        for variable in variables {
            let var = self.resolve_mut_or_immutable_expression(
                &variable.expression,
                None,
                LocationSide::Rhs,
            )?;
            variable_expressions.push(var);
        }

        self.push_closed_block_scope();
        let mut expressions = Vec::new();
        for (variable_binding, resolved_expression) in variables.iter().zip(variable_expressions) {
            let initialize_variable_expression = self.create_variable_binding_for_with(
                &variable_binding.variable,
                resolved_expression,
            )?;
            expressions.push(initialize_variable_expression);
        }

        let resolved_expression = self.resolve_expression(expression, expected_type)?;
        let block_type = resolved_expression.ty.clone();
        expressions.push(resolved_expression);

        let block_expression_kind = ResolvedExpressionKind::Block(expressions);
        self.pop_closed_block_scope();

        let block_expr = self.create_expr(block_expression_kind, block_type, &expression.node);
        Ok(block_expr)
    }

    fn resolve_when_expr(
        &mut self,
        expected_type: Option<&ResolvedType>,
        variables: &[WhenBinding],
        true_expr: &Expression,
        else_expr: &Option<Box<Expression>>,
    ) -> Result<ResolvedExpression, ResolveError> {
        self.push_block_scope("when");
        let mut bindings = Vec::new();
        for variable_binding in variables {
            let mut_expr = if let Some(found_expr) = &variable_binding.expression {
                self.resolve_mut_or_immutable_expression(found_expr, None, LocationSide::Rhs)?
            } else {
                let same_var = self.find_variable(&variable_binding.variable)?;

                let is_mutable = same_var.mutable_node.clone();
                let argument_expression = if same_var.is_mutable() {
                    let loc = ResolvedSingleLocationExpression {
                        kind: ResolvedSingleLocationExpressionKind::MutVariableRef,
                        node: self.to_node(&variable_binding.variable.name),
                        ty: same_var.resolved_type.clone(),
                        starting_variable: same_var,
                        access_chain: vec![],
                    };
                    ResolvedArgumentExpressionOrLocation::Location(loc)
                } else {
                    let generated_expr_kind =
                        ResolvedExpressionKind::VariableAccess(same_var.clone());
                    let generated_expression = self.create_expr(
                        generated_expr_kind,
                        same_var.resolved_type.clone(),
                        &variable_binding.variable.name,
                    );
                    ResolvedArgumentExpressionOrLocation::Expression(generated_expression)
                };

                ResolvedMutOrImmutableExpression {
                    expression_or_location: argument_expression,
                    is_mutable,
                }
            };

            let ty = mut_expr.ty();

            if let ResolvedType::Optional(found_ty) = ty {
                let variable_ref = self.create_variable(&variable_binding.variable, found_ty)?;

                let binding = ResolvedWhenBinding {
                    variable: variable_ref,
                    expr: mut_expr,
                };
                bindings.push(binding);
            } else {
                return Err(self.create_err(ResolveErrorKind::ExpectedOptional, &true_expr.node));
            }
        }

        let resolved_true = self.resolve_expression(true_expr, expected_type)?;
        let block_type = resolved_true.ty.clone();

        self.pop_block_scope("when");

        let maybe_resolved_else = if let Some(found_else) = else_expr {
            Some(Box::new(
                self.resolve_expression(found_else, Some(&block_type))?,
            ))
        } else {
            None
        };

        let when_kind =
            ResolvedExpressionKind::When(bindings, Box::from(resolved_true), maybe_resolved_else);

        let block_expr = self.create_expr(when_kind, block_type, &true_expr.node);
        Ok(block_expr)
    }

    fn resolve_guard(
        &mut self,
        node: &Node,
        expected_type: Option<&ResolvedType>,
        guard_expressions: &Vec<GuardExpr>,
    ) -> Result<ResolvedExpression, ResolveError> {
        let mut expecting_type = expected_type.cloned();
        let mut guards = Vec::new();
        let mut found_wildcard = None;

        for guard in guard_expressions {
            let resolved_condition = match &guard.clause {
                GuardClause::Wildcard(x) => {
                    if found_wildcard.is_some() {
                        return Err(self.create_err(
                            ResolveErrorKind::GuardCanNotHaveMultipleWildcards,
                            &node,
                        ));
                    }
                    found_wildcard = Some(x);
                    None
                }
                GuardClause::Expression(clause_expr) => {
                    if found_wildcard.is_some() {
                        return Err(
                            self.create_err(ResolveErrorKind::WildcardMustBeLastInGuard, &node)
                        );
                    }
                    Some(self.resolve_bool_expression(&clause_expr)?)
                }
            };

            let resolved_result =
                self.resolve_expression(&guard.result, expecting_type.as_ref())?;
            let ty = resolved_result.ty.clone();
            if expecting_type == None {
                expecting_type = Some(ty);
            }

            guards.push(ResolvedGuard {
                condition: resolved_condition,
                result: resolved_result,
            });
        }

        if found_wildcard.is_none() {
            return Err(self.create_err(ResolveErrorKind::GuardMustHaveWildcard, &node));
        }

        let kind = ResolvedExpressionKind::Guard(guards);

        if let Some(found_expecting_type) = expecting_type {
            let expr = self.create_expr(kind, found_expecting_type.clone(), node);
            Ok(expr)
        } else {
            Err(self.create_err(ResolveErrorKind::GuardHasNoType, node))
        }
    }

    pub fn resolve_variable_assignment(
        &mut self,
        variable: &Variable,
        source_expression: &MutableOrImmutableExpression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let source_expr =
            self.resolve_mut_or_immutable_expression(source_expression, None, LocationSide::Rhs)?;
        let ty = source_expr.ty().clone();

        let maybe_found_variable = self.try_find_variable(&variable.name);

        let kind: ResolvedExpressionKind = if let Some(found_var) = maybe_found_variable {
            if !found_var.is_mutable() {
                return Err(self.create_err(ResolveErrorKind::VariableIsNotMutable, &variable.name));
            }
            if !found_var.resolved_type.assignable_type(&ty) {
                return Err(self.create_err(
                    ResolveErrorKind::IncompatibleTypes(
                        found_var.resolved_type.clone(),
                        ty.clone(),
                    ),
                    &variable.name,
                ));
            }
            ResolvedExpressionKind::VariableReassignment(found_var, Box::from(source_expr))
        } else {
            let new_var = self.create_variable(&variable, &ty)?;
            ResolvedExpressionKind::VariableDefinition(new_var, Box::from(source_expr))
        };

        Ok(self.create_expr(kind, ResolvedType::Unit, &variable.name))
    }

    fn resolve_create_variable(
        &mut self,
        var: &Variable,
        coerce_type: &Option<Type>,
        source_expression: &MutableOrImmutableExpression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let ty = if let Some(found_ast_type) = coerce_type {
            Some(self.resolve_type(found_ast_type)?)
        } else {
            None
        };

        let resolved_source = self.resolve_mut_or_immutable_expression(
            source_expression,
            ty.as_ref(),
            LocationSide::Rhs,
        )?;

        let var_ref =
            self.create_local_variable(&var.name, &var.is_mutable, &resolved_source.ty())?;

        let resolved_type = resolved_source.ty().clone();
        assert_ne!(resolved_type, ResolvedType::Unit);
        let kind = ResolvedExpressionKind::VariableDefinition(var_ref, Box::from(resolved_source));

        let resolved_expr = self.create_expr(kind, resolved_type, &var.name);

        Ok(resolved_expr)
    }

    fn add_location_item(
        &mut self,
        vec: &mut Vec<ResolvedLocationAccess>,
        kind: ResolvedLocationAccessKind,
        ty: ResolvedType,
        ast_node: &Node,
    ) {
        let resolved_node = self.to_node(ast_node);
        let postfix = ResolvedLocationAccess {
            node: resolved_node.clone(),
            ty,
            kind,
        };

        vec.push(postfix)
    }

    #[allow(clippy::too_many_lines)]
    fn resolve_chain_to_location(
        &mut self,
        chain: &PostfixChain,
        expected_type: Option<ResolvedType>,
        location_side: LocationSide,
    ) -> Result<ResolvedSingleLocationExpression, ResolveError> {
        let mut items = Vec::new();

        let base_expr = self.resolve_expression(&chain.base, None)?;
        let ResolvedExpressionKind::VariableAccess(start_variable) = base_expr.kind else {
            return Err(self.create_err(
                ResolveErrorKind::NotValidLocationStartingPoint,
                &chain.base.node,
            ));
        };

        let mut ty = start_variable.resolved_type.clone();
        for (i, item) in chain.postfixes.iter().enumerate() {
            match &item {
                Postfix::FieldAccess(field_name_node) => {
                    //let field_name_resolved = self.to_node(field_name_node)
                    let (struct_type_ref, index, return_type) =
                        self.resolve_struct_field(field_name_node, ty)?;
                    self.add_location_item(
                        &mut items,
                        ResolvedLocationAccessKind::FieldIndex(struct_type_ref.clone(), index),
                        return_type.clone(),
                        field_name_node,
                    );

                    ty = return_type.clone();
                }
                Postfix::Subscript(lookup_expr) => {
                    let is_range = if let ExpressionKind::Range(min, max, mode) = &lookup_expr.kind
                    {
                        Some(self.resolve_range(min, max, mode)?)
                    } else {
                        None
                    };
                    match &ty {
                        ResolvedType::String => {
                            if let Some(range) = is_range {
                                self.add_location_item(
                                    &mut items,
                                    ResolvedLocationAccessKind::StringRange(range),
                                    ResolvedType::String,
                                    &lookup_expr.node,
                                );
                                ty = ResolvedType::String;
                            } else {
                                let index_expr =
                                    self.resolve_expression(lookup_expr, Some(&ResolvedType::Int))?; // TODO: Support slice (range)
                                self.add_location_item(
                                    &mut items,
                                    ResolvedLocationAccessKind::StringIndex(index_expr),
                                    ResolvedType::String,
                                    &lookup_expr.node,
                                );
                                ty = ResolvedType::String;
                            }
                        }

                        ResolvedType::Array(array_type) => {
                            let index_expr =
                                self.resolve_expression(lookup_expr, Some(&ResolvedType::Int))?; // TODO: Support slice (range)
                            self.add_location_item(
                                &mut items,
                                ResolvedLocationAccessKind::ArrayIndex(
                                    array_type.clone(),
                                    index_expr,
                                ),
                                array_type.item_type.clone(),
                                &lookup_expr.node,
                            );
                            ty = array_type.item_type.clone();
                        }

                        ResolvedType::Map(map_type) => {
                            let key_expr = self.resolve_expression(
                                lookup_expr,
                                Some(&map_type.key_type.clone()),
                            )?;
                            let is_last = i == chain.postfixes.len() - 1;
                            let allow_auto_insert = is_last && location_side == LocationSide::Lhs;
                            let (kind, lookup_type) = if allow_auto_insert {
                                // If this is the last postfix in the chain, then it is a "bare" access and auto-insert is allowed
                                // the type is `value_type` since this lookup is safe. we can create a memory location if there wasn't one
                                (
                                    ResolvedLocationAccessKind::MapIndexInsertIfNonExisting(
                                        map_type.clone(),
                                        key_expr,
                                    ),
                                    map_type.value_type.clone(),
                                )
                            } else {
                                let optional_value_type =
                                    ResolvedType::Optional(Box::from(map_type.value_type.clone()));
                                (
                                    ResolvedLocationAccessKind::MapIndex(
                                        map_type.clone(),
                                        key_expr,
                                    ),
                                    optional_value_type,
                                )
                            };

                            self.add_location_item(
                                &mut items,
                                kind,
                                lookup_type.clone(),
                                &lookup_expr.node,
                            );
                            ty = lookup_type;
                        }

                        ResolvedType::Generic(collection_type, generic_params) => {
                            if let ResolvedType::RustType(rust_type) = &**collection_type {
                                let val_type = generic_params[0].clone();
                                if rust_type.number == SPARSE_TYPE_ID {
                                    let sparse_id_type = self
                                        .shared
                                        .lookup
                                        .get_rust_type(&["std".to_string()], "SparseId")
                                        .expect("should have SparseId");

                                    let key_type = ResolvedType::RustType(sparse_id_type);

                                    let key_expr = self
                                        .resolve_expression(lookup_expr, Some(&key_type.clone()))?;

                                    self.add_location_item(
                                        &mut items,
                                        ResolvedLocationAccessKind::RustTypeIndex(
                                            rust_type.clone(),
                                            key_expr,
                                        ),
                                        key_type.clone(),
                                        &lookup_expr.node,
                                    );

                                    ty = ResolvedType::Optional(Box::from(val_type.clone()));
                                }
                            }
                        }

                        _ => {
                            return Err(self.create_err(
                                ResolveErrorKind::IllegalIndexInChain,
                                &lookup_expr.node,
                            ));
                        }
                    }
                }

                Postfix::MemberCall(node, _) => {
                    return Err(self.create_err(ResolveErrorKind::CallsCanNotBePartOfChain, &node));
                }

                Postfix::FunctionCall(node, _) => {
                    return Err(self.create_err(ResolveErrorKind::CallsCanNotBePartOfChain, &node));
                }
                Postfix::OptionUnwrap(node) => {
                    return Err(self.create_err(ResolveErrorKind::UnwrapCanNotBePartOfChain, &node));
                }
                Postfix::NoneCoalesce(expr) => {
                    return Err(self.create_err(
                        ResolveErrorKind::NoneCoalesceCanNotBePartOfChain,
                        &expr.node,
                    ));
                }
            }
        }

        if let Some(found_expected_type) = expected_type {
            if !ty.same_type(&found_expected_type) {
                return Err(self.create_err(
                    ResolveErrorKind::IncompatibleTypes(ty, found_expected_type),
                    &chain.base.node,
                ));
            }
        }

        let location = ResolvedSingleLocationExpression {
            kind: ResolvedSingleLocationExpressionKind::MutVariableRef,
            node: self.to_node(&chain.base.node),
            ty: ty.clone(),
            starting_variable: start_variable,
            access_chain: items,
        };
        Ok(location)
    }

    fn resolve_to_location(
        &mut self,
        expr: &Expression,
        expected_type: Option<ResolvedType>,
        location_type: LocationSide,
    ) -> Result<ResolvedSingleLocationExpression, ResolveError> {
        //let resolved_expr = self.resolve_expression(expr, Some(&expected_type))?;
        match &expr.kind {
            ExpressionKind::PostfixChain(chain) => {
                self.resolve_chain_to_location(chain, expected_type, location_type)
            }
            ExpressionKind::IdentifierReference(variable) => {
                let var = self.find_variable(variable)?;
                if var.is_mutable() {
                    Ok(ResolvedSingleLocationExpression {
                        kind: ResolvedSingleLocationExpressionKind::MutVariableRef,
                        node: self.to_node(&variable.name),
                        ty: var.resolved_type.clone(),
                        starting_variable: var,
                        access_chain: vec![],
                    })
                } else {
                    Err(self.create_err(ResolveErrorKind::VariableIsNotMutable, &expr.node))
                }
            }
            _ => Err(self.create_err(ResolveErrorKind::NotValidLocationStartingPoint, &expr.node)),
        }
    }

    #[allow(clippy::single_match)]
    fn check_special_assignment_compound(
        &mut self,
        target_expression: &Expression,
        target_type: &ResolvedType,
        op: &ResolvedCompoundOperatorKind,
        source: &Expression,
        source_type: &ResolvedType,
    ) -> Result<Option<ResolvedExpressionKind>, ResolveError> {
        match &target_type {
            ResolvedType::Array(array_type) => {
                if *op == ResolvedCompoundOperatorKind::Add
                    && source_type.same_type(&array_type.item_type)
                {
                    // Handle ArrayPush
                    let target_location =
                        ResolvedSingleMutLocationExpression(self.resolve_to_location(
                            target_expression,
                            Some(target_type.clone()),
                            LocationSide::Rhs,
                        )?);
                    let resolved_source = self.resolve_expression(source, Some(source_type))?;
                    return Ok(Option::from(ResolvedExpressionKind::ArrayPush(
                        target_location,
                        Box::from(resolved_source),
                    )));
                } else if *op == ResolvedCompoundOperatorKind::Add
                    && source_type.same_type(target_type)
                {
                    // Handle ArrayExtend
                    let target_location =
                        ResolvedSingleMutLocationExpression(self.resolve_to_location(
                            target_expression,
                            Some(target_type.clone()),
                            LocationSide::Rhs,
                        )?);
                    let resolved_source = self.resolve_expression(source, Some(source_type))?;
                    return Ok(Option::from(ResolvedExpressionKind::ArrayExtend(
                        target_location,
                        Box::from(resolved_source),
                    )));
                }
            }
            _ => {}
        }

        Ok(None)
    }

    fn resolve_assignment_compound(
        &mut self,
        target_expression: &Expression,
        ast_op: &CompoundOperator,
        ast_source_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_op = self.resolve_compound_operator(ast_op);
        let source_expr = self.resolve_expression(ast_source_expression, None)?;

        let resolved_location = ResolvedSingleMutLocationExpression(self.resolve_to_location(
            target_expression,
            Some(source_expr.ty.clone()),
            LocationSide::Rhs,
        )?);

        let kind = if let Some(found_special) = self.check_special_assignment_compound(
            target_expression,
            &resolved_location.0.ty,
            &resolved_op.kind,
            ast_source_expression,
            &source_expr.ty,
        )? {
            found_special
        } else {
            ResolvedExpressionKind::CompoundAssignment(
                resolved_location,
                resolved_op.kind,
                Box::from(source_expr),
            )
        };

        let expr = self.create_expr(kind, ResolvedType::Unit, &target_expression.node);

        Ok(expr)
    }

    fn resolve_assignment(
        &mut self,
        target_location: &Expression,
        ast_source_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_location =
            self.resolve_to_location(target_location, None, LocationSide::Lhs)?;

        let ty = resolved_location.ty.clone();
        if ty == ResolvedType::Unit {
            error!(?ast_source_expression, "unit problem");
        }
        let source_expr = self.resolve_expression(ast_source_expression, Some(&ty))?;

        let mut_location = ResolvedSingleMutLocationExpression(resolved_location);

        let kind =
            ResolvedExpressionKind::Assignment(Box::from(mut_location), Box::from(source_expr));

        let expr = self.create_expr(kind, ResolvedType::Unit, &target_location.node); // Assignments are always of type Unit

        Ok(expr)
    }

    pub fn create_mut_single_location_expr(
        &self,
        kind: ResolvedSingleLocationExpressionKind,
        ty: ResolvedType,
        ast_node: &Node,
    ) -> ResolvedSingleMutLocationExpression {
        ResolvedSingleMutLocationExpression(ResolvedSingleLocationExpression {
            kind,
            ty,
            starting_variable: Rc::new(ResolvedVariable {
                name: Default::default(),
                resolved_type: ResolvedType::Int,
                mutable_node: None,
                scope_index: 0,
                variable_index: 0,
            }),
            node: self.to_node(ast_node),
            access_chain: vec![],
        })
    }

    pub fn create_single_location_expr(
        &self,
        kind: ResolvedSingleLocationExpressionKind,
        ty: ResolvedType,
        ast_node: &Node,
    ) -> ResolvedSingleLocationExpression {
        ResolvedSingleLocationExpression {
            kind,
            ty,
            starting_variable: Rc::new(ResolvedVariable {
                name: Default::default(),
                resolved_type: ResolvedType::Int,
                mutable_node: None,
                scope_index: 0,
                variable_index: 0,
            }),
            node: self.to_node(ast_node),
            access_chain: vec![],
        }
    }

    pub fn create_single_location_expr_resolved(
        &self,
        kind: ResolvedSingleLocationExpressionKind,
        ty: ResolvedType,
        node: &ResolvedNode,
    ) -> ResolvedSingleLocationExpression {
        ResolvedSingleLocationExpression {
            kind,
            ty,
            starting_variable: Rc::new(ResolvedVariable {
                name: Default::default(),
                resolved_type: ResolvedType::Int,
                mutable_node: None,
                scope_index: 0,
                variable_index: 0,
            }),
            node: node.clone(),
            access_chain: vec![],
        }
    }
    pub fn create_mut_single_location_expr_resolved(
        &self,
        kind: ResolvedSingleLocationExpressionKind,
        ty: ResolvedType,
        node: &ResolvedNode,
    ) -> ResolvedSingleMutLocationExpression {
        ResolvedSingleMutLocationExpression(ResolvedSingleLocationExpression {
            kind,
            ty,
            starting_variable: Rc::new(ResolvedVariable {
                name: Default::default(),
                resolved_type: ResolvedType::Int,
                mutable_node: None,
                scope_index: 0,
                variable_index: 0,
            }),
            node: node.clone(),
            access_chain: vec![],
        })
    }

    pub fn create_expr(
        &self,
        kind: ResolvedExpressionKind,
        ty: ResolvedType,
        ast_node: &Node,
    ) -> ResolvedExpression {
        //info!(%ty, ?kind, "create_expr()");
        ResolvedExpression {
            kind,
            ty,
            node: self.to_node(ast_node),
        }
    }

    pub fn create_expr_resolved(
        &self,
        kind: ResolvedExpressionKind,
        ty: ResolvedType,
        ast_node: &ResolvedNode,
    ) -> ResolvedExpression {
        ResolvedExpression {
            kind,
            ty,
            node: ast_node.clone(),
        }
    }

    fn resolve_destructuring(
        &mut self,
        node: &Node,
        target_ast_variables: &[Variable],
        tuple_expression: &Expression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let tuple_resolved = self.resolve_expression(tuple_expression, None)?;
        let tuple_expr_type = &tuple_resolved.ty;

        let mut variable_refs = Vec::new();
        if let ResolvedType::Tuple(tuple) = tuple_expr_type.clone() {
            if target_ast_variables.len() > tuple.0.len() {
                return Err(self.create_err(ResolveErrorKind::TooManyDestructureVariables, &node));
            }
            for (variable_ref, tuple_type) in target_ast_variables.iter().zip(tuple.0.clone()) {
                let (variable_ref, _is_reassignment) =
                    self.set_or_overwrite_variable_with_type(variable_ref, &tuple_type)?;
                variable_refs.push(variable_ref);
            }
            let expr_kind = ResolvedExpressionKind::TupleDestructuring(
                variable_refs,
                tuple,
                Box::from(tuple_resolved),
            );

            Ok(self.create_expr(expr_kind, ResolvedType::Unit, &node))
        } else {
            Err(self.create_err(ResolveErrorKind::CanNotDestructure, &node))
        }
    }

    fn resolve_postfix_member_func_call(
        &mut self,
        resolved_node: &ResolvedNode,
        found_function: &ResolvedFunctionRef,
        struct_type: &ResolvedStructTypeRef,
        is_mutable: bool,
        arguments: &[MutableOrImmutableExpression],
    ) -> Result<ResolvedPostfix, ResolveError> {
        let signature = found_function.signature();

        let self_type = &signature.parameters[0];
        if !self_type
            .resolved_type
            .clone()
            .unwrap()
            .same_type(&ResolvedType::Struct(struct_type.clone()))
            || self_type.is_mutable && !is_mutable
        {
            return Err(
                self.create_err_resolved(ResolveErrorKind::SelfNotCorrectType, resolved_node)
            );
        }

        let resolved_arguments = self.resolve_and_verify_parameters(
            &resolved_node,
            &signature.parameters[1..],
            arguments,
        )?;

        let kind = ResolvedPostfixKind::MemberCall(found_function.clone(), resolved_arguments);
        let postfix = ResolvedPostfix {
            node: resolved_node.clone(),
            ty: *signature.return_type.clone(),
            kind,
        };

        Ok(postfix)
    }

    fn resolve_postfix_field_call(
        &mut self,
        resolved_node: &ResolvedNode,
        struct_type: &ResolvedStructTypeRef,
        field: &ResolvedAnonymousStructFieldType,
        index: usize,
        signature: &FunctionTypeSignature,
        arguments: &[MutableOrImmutableExpression],
    ) -> Result<Vec<ResolvedPostfix>, ResolveError> {
        let mut suffixes = Vec::new();
        //let field_name_str = self.get_text(member_name).to_string();
        let struct_field_kind = ResolvedPostfixKind::StructField(struct_type.clone(), index);

        let struct_field_postfix = ResolvedPostfix {
            node: resolved_node.clone(),
            ty: field.field_type.clone(),
            kind: struct_field_kind,
        };

        suffixes.push(struct_field_postfix);

        let resolved_arguments =
            self.resolve_and_verify_parameters(&resolved_node, &signature.parameters, arguments)?;

        let call_kind = ResolvedPostfixKind::FunctionCall(resolved_arguments);

        let call_postfix = ResolvedPostfix {
            node: resolved_node.clone(),
            ty: *signature.return_type.clone(),
            kind: call_kind,
        };
        suffixes.push(call_postfix);

        Ok(suffixes)
    }

    fn resolve_postfix_member_call(
        &mut self,
        struct_type: &ResolvedStructTypeRef,
        is_mutable: bool,
        member_name: &Node,
        arguments: &[MutableOrImmutableExpression],
        suffixes: &mut Vec<ResolvedPostfix>,
    ) -> Result<ResolvedType, ResolveError> {
        let field_name_str = self.get_text(member_name).to_string();

        let resolved_node = self.to_node(member_name);
        let binding = struct_type.borrow();
        let postfixes = if let Some(found_function_member) = binding.functions.get(&field_name_str)
        {
            let postfix = self.resolve_postfix_member_func_call(
                &resolved_node,
                found_function_member,
                struct_type,
                is_mutable,
                arguments,
            )?;
            vec![postfix]
        } else if let Some(found_field) =
            binding.anon_struct_type.defined_fields.get(&field_name_str)
        {
            if let ResolvedType::Function(signature) = &found_field.field_type {
                let index = binding
                    .anon_struct_type
                    .defined_fields
                    .get_index(&field_name_str)
                    .expect("should work");
                self.resolve_postfix_field_call(
                    &resolved_node,
                    struct_type,
                    found_field,
                    index,
                    signature,
                    arguments,
                )?
            } else {
                return Err(self.create_err(
                    ResolveErrorKind::NotValidLocationStartingPoint,
                    &member_name,
                ));
            }
        } else {
            return Err(self.create_err(
                ResolveErrorKind::NotValidLocationStartingPoint,
                &member_name,
            ));
        };

        let last_type = postfixes.last().unwrap().ty.clone();
        suffixes.extend(postfixes);

        Ok(last_type)
    }

    /*
    pub fn resolve_range(&mut self, min_value: &Expression, max_value: &Expression, range_mode: &RangeMode) -> ResolvedRange {
        let min_expression =
            self.resolve_expression(min_value, Some(&ResolvedType::Int))?;
        let max_expression =
            self.resolve_expression(max_value, Some(&ResolvedType::Int))?;

        ResolvedRange {
            min: min_expression,
            max: max_expression,
            mode: convert_range_mode(range_mode),
        }
    }

     */
}
