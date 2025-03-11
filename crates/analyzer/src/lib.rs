/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod access;
pub mod call;
pub mod constant;
pub mod def;
pub mod err;
mod instantiator;
pub mod internal;
pub mod literal;
pub mod operator;
pub mod pattern;
pub mod prelude;
mod structure;
pub mod types;
pub mod variable;

use crate::err::{Error, ErrorKind};
use crate::instantiator::{Instantiator, TypeVariableScope};
use seq_map::SeqMap;
use std::any::Any;
use std::mem::take;
use std::num::{ParseFloatError, ParseIntError};
use std::rc::Rc;
use swamp_script_modules::prelude::*;
use swamp_script_modules::symtbl::SymbolTableRef;
use swamp_script_node::{FileId, Node, Span};
use swamp_script_semantic::prelude::*;
use swamp_script_semantic::{
    ArgumentExpressionOrLocation, LocationAccess, LocationAccessKind, MutOrImmutableExpression,
    NormalPattern, Postfix, PostfixKind, RangeMode, SingleLocationExpression,
    SingleLocationExpressionKind, SingleMutLocationExpression, TypeWithMut, WhenBinding,
};
use swamp_script_source_map::SourceMap;
use swamp_script_types::prelude::*;
use swamp_script_types::{
    ParameterizedTypeBlueprint, all_types_are_concrete, all_types_are_concrete_or_unit,
};
use tracing::{debug, error, info, trace};

#[must_use]
pub fn convert_range_mode(range_mode: &swamp_script_ast::RangeMode) -> RangeMode {
    match range_mode {
        swamp_script_ast::RangeMode::Inclusive => RangeMode::Inclusive,
        swamp_script_ast::RangeMode::Exclusive => RangeMode::Exclusive,
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum LocationSide {
    Lhs,
    Rhs,
}

#[derive(Debug)]
pub struct Program {
    pub state: ProgramState,
    pub modules: Modules,
    pub default_symbol_table: SymbolTable,
}

impl Default for Program {
    fn default() -> Self {
        Self::new(ProgramState::new(), Modules::new(), SymbolTable::new(&[]))
    }
}

impl Program {
    #[must_use]
    pub const fn new(
        state: ProgramState,
        modules: Modules,
        default_symbol_table: SymbolTable,
    ) -> Self {
        Self {
            state,
            modules,
            default_symbol_table,
        }
    }
}

#[must_use]
pub const fn convert_span(without: &swamp_script_ast::SpanWithoutFileId, file_id: FileId) -> Span {
    Span {
        file_id,
        offset: without.offset,
        length: without.length,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeContextScope {
    InsideFunction, // Allow return, but not break
    InsideLoop,     // Allow break and return
    /// Inside both a function and a loop
    /// Allows: return, break, and continue
    InsideBothFunctionAndLoop,
    ArgumentOrOutsideFunction, // Allow neither break nor return
}

impl TypeContextScope {
    /// Returns true if return statements are allowed in this scope
    #[must_use]
    pub fn allows_return(&self) -> bool {
        matches!(self, Self::InsideFunction | Self::InsideBothFunctionAndLoop)
    }

    /// Returns true if break statements are allowed in this scope
    #[must_use]
    pub fn allows_break(&self) -> bool {
        matches!(self, Self::InsideLoop | Self::InsideBothFunctionAndLoop)
    }

    /// Returns true if continue statements are allowed in this scope
    #[must_use]
    pub fn allows_continue(&self) -> bool {
        self.allows_break() // Same rules as break
    }

    /// Creates a new scope when entering a function
    #[must_use]
    pub fn enter_function(&self) -> Self {
        match self {
            Self::ArgumentOrOutsideFunction => Self::InsideFunction,
            Self::InsideLoop => Self::InsideBothFunctionAndLoop,
            _ => *self,
        }
    }

    /// Creates a new scope when entering a loop
    #[must_use]
    pub fn enter_loop(&self) -> Self {
        match self {
            Self::ArgumentOrOutsideFunction => Self::InsideLoop,
            Self::InsideFunction => Self::InsideBothFunctionAndLoop,
            _ => *self,
        }
    }
}

/// Type checking context
#[derive(Debug, Clone)]
pub struct TypeContext<'a> {
    /// Expected type for the current expression
    pub expected_type: Option<&'a Type>,

    /// Return type of the enclosing function
    pub return_type: Option<&'a Type>,

    pub scope: TypeContextScope,

    pub is_in_compare_like: bool,
}

impl TypeContext<'_> {
    pub(crate) fn allows_continue(&self) -> bool {
        self.scope.allows_continue() && self.is_in_compare_like
    }
}

impl TypeContext<'_> {
    pub(crate) fn allows_return(&self) -> bool {
        self.scope.allows_return() && self.is_in_compare_like
    }
}

impl TypeContext<'_> {
    pub(crate) fn allows_break(&self) -> bool {
        self.scope.allows_break()
    }
}

impl<'a> TypeContext<'a> {
    #[must_use]
    pub const fn new(
        expected_type: Option<&'a Type>,
        return_type: Option<&'a Type>,
        scope: TypeContextScope,
    ) -> Self {
        Self {
            expected_type,
            return_type,
            scope,
            is_in_compare_like: false,
        }
    }

    #[must_use]
    pub const fn new_argument(required_type: &'a Type) -> Self {
        Self {
            expected_type: Some(required_type),
            return_type: None,
            scope: TypeContextScope::ArgumentOrOutsideFunction,
            is_in_compare_like: false,
        }
    }

    #[must_use]
    pub const fn new_unsure_argument(expected_type: Option<&'a Type>) -> Self {
        Self {
            expected_type,
            return_type: None,
            scope: TypeContextScope::ArgumentOrOutsideFunction,
            is_in_compare_like: false,
        }
    }

    #[must_use]
    pub const fn new_anything_argument() -> Self {
        Self {
            expected_type: None,
            return_type: None,
            scope: TypeContextScope::ArgumentOrOutsideFunction,
            is_in_compare_like: false,
        }
    }

    #[must_use]
    pub const fn new_function(required_type: &'a Type) -> Self {
        Self {
            expected_type: Some(required_type),
            return_type: Some(required_type),
            scope: TypeContextScope::InsideFunction,
            is_in_compare_like: false,
        }
    }

    #[must_use]
    pub const fn with_expected_type(&self, expected_type: Option<&'a Type>) -> Self {
        Self {
            expected_type,
            return_type: self.return_type,
            scope: self.scope,
            is_in_compare_like: self.is_in_compare_like,
        }
    }

    pub(crate) const fn we_know_expected_type(&self, found_type: &'a Type) -> Self {
        self.with_expected_type(Some(found_type))
    }

    /// # Panics
    ///
    #[must_use]
    pub const fn for_return(&self) -> Self {
        Self {
            expected_type: Some(self.return_type.unwrap()),
            return_type: Some(self.return_type.unwrap()),
            scope: TypeContextScope::ArgumentOrOutsideFunction,
            is_in_compare_like: false,
        }
    }

    #[must_use]
    pub fn enter_function(&self, required_type: &'a Type) -> Self {
        Self {
            expected_type: Some(required_type),
            return_type: Some(required_type),
            scope: self.scope.enter_function(),
            is_in_compare_like: false,
        }
    }

    /// Creates a new scope when entering a loop
    #[must_use]
    pub fn enter_loop(&self) -> Self {
        Self {
            expected_type: self.expected_type,
            return_type: self.return_type,
            scope: self.scope.enter_loop(),
            is_in_compare_like: self.is_in_compare_like,
        }
    }

    /// Creates a new scope when entering a loop
    #[must_use]
    pub fn enter_compare(&self) -> Self {
        Self {
            expected_type: self.expected_type,
            return_type: self.return_type,
            scope: self.scope.enter_loop(),
            is_in_compare_like: true,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum BlockScopeMode {
    Open,
    Closed,
}

#[derive(Debug)]
pub struct BlockScope {
    mode: BlockScopeMode,
    variables: SeqMap<String, VariableRef>,
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
        }
    }
}

pub struct SharedState<'a> {
    pub state: &'a mut ProgramState,
    pub lookup_table: SymbolTable,
    pub definition_table: SymbolTable,
    pub modules: &'a Modules,
    pub source_map: &'a SourceMap,
    pub file_id: FileId,
    pub core_symbol_table: SymbolTableRef,
    type_variables: Option<TypeVariableScope>,
}

impl<'a> SharedState<'a> {
    #[must_use]
    pub fn get_symbol_table(&'a self, path: &[String]) -> Option<&'a SymbolTable> {
        if path.is_empty() {
            return Some(&self.lookup_table);
        }
        self.get_module(path)
            .map_or(None, |module| Some(&module.symbol_table))
    }

    #[must_use]
    pub fn get_module(&'a self, path: &[String]) -> Option<&'a ModuleRef> {
        let resolved_path = {
            self.lookup_table.get_package_version(&path[0]).map_or_else(
                || path.to_vec(),
                |found_version| {
                    let mut new_path = path.to_vec();
                    let complete_name = format!("{}-{found_version}", path[0]);
                    // new_path[0] = complete_name;
                    //new_path
                    path.to_vec()
                },
            )
        };

        if path.len() == 1 {
            if let Some(module_ref) = self.lookup_table.get_module_link(&path[0]) {
                return Some(module_ref);
            }
        }

        if let Some(x) = self.modules.get(&resolved_path) {
            return Some(x);
        }

        None
    }
}

pub struct FunctionScopeState {
    pub block_scope_stack: Vec<BlockScope>,
    pub return_type: Type,
}

impl FunctionScopeState {
    #[must_use]
    pub fn new(return_type: Type) -> Self {
        Self {
            block_scope_stack: vec![BlockScope::new()],
            return_type,
        }
    }
}

pub struct Analyzer<'a> {
    pub shared: SharedState<'a>,
    scope: FunctionScopeState,
    global: FunctionScopeState,
    module_path: Vec<String>,
}

impl<'a> Analyzer<'a> {
    pub fn new(
        state: &'a mut ProgramState,
        modules: &'a Modules,
        core_symbol_table: SymbolTableRef,
        source_map: &'a SourceMap,
        module_path: &[String],
        file_id: FileId,
    ) -> Self {
        let shared = SharedState {
            state,
            lookup_table: SymbolTable::new(&[]),
            definition_table: SymbolTable::new(module_path),
            modules,
            core_symbol_table,
            source_map,
            file_id,
            type_variables: None,
        };
        Self {
            scope: FunctionScopeState::new(Type::Unit),
            global: FunctionScopeState::new(Type::Unit),
            shared,
            module_path: module_path.to_vec(),
        }
    }

    fn start_function(&mut self, return_type: Type) {
        self.global.block_scope_stack = take(&mut self.scope.block_scope_stack);
        self.scope = FunctionScopeState::new(return_type);
    }

    fn stop_function(&mut self) {
        self.scope.block_scope_stack = take(&mut self.global.block_scope_stack);
    }

    fn analyze_if_expression(
        &mut self,
        condition: &swamp_script_ast::Expression,
        true_expression: &swamp_script_ast::Expression,
        maybe_false_expression: Option<&swamp_script_ast::Expression>,
        context: &TypeContext,
    ) -> Result<Expression, Error> {
        let resolved_condition = self.analyze_bool_argument_expression(condition)?;

        let branch_context = context.enter_compare();

        let true_expr = self.analyze_expression(true_expression, &branch_context)?;
        let resolved_true = Box::new(true_expr);

        let mut detected = context.expected_type.cloned();
        if detected.is_none() && !matches!(resolved_true.ty, Type::Never) {
            detected = Some(resolved_true.ty.clone());
        }

        // Analyze the false branch if it exists
        let else_statements = if let Some(false_expression) = maybe_false_expression {
            let else_context = branch_context.with_expected_type(detected.as_ref());
            let else_expr = self.analyze_expression(false_expression, &else_context)?;
            if detected.is_none() && !matches!(else_expr.ty, Type::Never) {
                detected = Some(else_expr.ty.clone());
            }

            Some(Box::new(else_expr))
        } else {
            None
        };

        Ok(self.create_expr(
            ExpressionKind::If(resolved_condition, resolved_true, else_statements),
            detected.unwrap(),
            &condition.node,
        ))
    }

    fn get_text(&self, ast_node: &swamp_script_ast::Node) -> &str {
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

    fn get_text_resolved(&self, resolved_node: &Node) -> &str {
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

    fn get_path(&self, ident: &swamp_script_ast::QualifiedTypeIdentifier) -> (Vec<String>, String) {
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

    fn analyze_return_type(
        &mut self,
        function: &swamp_script_ast::Function,
    ) -> Result<Type, Error> {
        let ast_return_type = match function {
            swamp_script_ast::Function::Internal(x) => &x.declaration.return_type,
            swamp_script_ast::Function::External(x) => &x.return_type,
        };

        let resolved_return_type = match ast_return_type {
            None => Type::Unit,
            Some(x) => self.analyze_type(x)?,
        };

        Ok(resolved_return_type)
    }

    fn analyze_function_body_expression(
        &mut self,
        expression: &swamp_script_ast::Expression,
        return_type: &Type,
    ) -> Result<Expression, Error> {
        let context = TypeContext::new_function(return_type);
        let resolved_statement = self.analyze_expression(expression, &context)?;

        Ok(resolved_statement)
    }

    fn analyze_maybe_type(
        &mut self,
        maybe_type: Option<&swamp_script_ast::Type>,
    ) -> Result<Type, Error> {
        let found_type = match maybe_type {
            None => Type::Unit,
            Some(ast_type) => self.analyze_type(ast_type)?,
        };
        Ok(found_type)
    }

    fn analyze_for_pattern(
        &mut self,
        pattern: &swamp_script_ast::ForPattern,
        key_type: Option<&Type>,
        value_type: &Type,
    ) -> Result<ForPattern, Error> {
        match pattern {
            swamp_script_ast::ForPattern::Single(var) => {
                let variable_ref = self.create_local_variable(
                    &var.identifier,
                    Option::from(&var.is_mut),
                    value_type,
                )?;
                Ok(ForPattern::Single(variable_ref))
            }
            swamp_script_ast::ForPattern::Pair(first, second) => {
                let found_key = key_type.unwrap();
                let first_var_ref = self.create_local_variable(
                    &first.identifier,
                    Option::from(&first.is_mut),
                    found_key,
                )?;
                let second_var_ref = self.create_local_variable(
                    &second.identifier,
                    Option::from(&second.is_mut),
                    value_type,
                )?;
                Ok(ForPattern::Pair(first_var_ref, second_var_ref))
            }
        }
    }

    fn analyze_parameters(
        &mut self,
        parameters: &Vec<swamp_script_ast::Parameter>,
    ) -> Result<Vec<TypeForParameter>, Error> {
        let mut resolved_parameters = Vec::new();
        for parameter in parameters {
            let param_type = self.analyze_type(&parameter.param_type)?;
            resolved_parameters.push(TypeForParameter {
                name: self.get_text(&parameter.variable.name).to_string(),
                resolved_type: param_type,
                is_mutable: parameter.variable.is_mutable.is_some(),
                node: Some(ParameterNode {
                    is_mutable: self.to_node_option(Option::from(&parameter.variable.is_mutable)),
                    name: self.to_node(&parameter.variable.name),
                }),
            });
        }
        Ok(resolved_parameters)
    }

    /// # Errors
    ///
    pub fn analyze_immutable_argument(
        &mut self,
        ast_expression: &swamp_script_ast::Expression,
        expected_type: &Type,
    ) -> Result<Expression, Error> {
        let context = TypeContext::new_argument(expected_type);
        self.analyze_expression(ast_expression, &context)
    }

    /// # Errors
    ///
    pub fn analyze_start_chain_expression_get_mutability(
        &mut self,
        ast_expression: &swamp_script_ast::Expression,
        expected_type: Option<&Type>,
    ) -> Result<(Expression, bool), Error> {
        let any_parameter_context = TypeContext::new_unsure_argument(expected_type);
        let resolved = self.analyze_expression(ast_expression, &any_parameter_context)?;
        let mutability = match resolved.kind {
            ExpressionKind::VariableAccess(ref resolved_variable) => resolved_variable.is_mutable(),
            _ => false,
        };

        Ok((resolved, mutability))
    }

    pub fn debug_expression(&self, expr: &swamp_script_ast::Expression) {
        let (line, col) = self
            .shared
            .source_map
            .get_span_location_utf8(self.shared.file_id, expr.node.span.offset as usize);
        let source_line = self
            .shared
            .source_map
            .get_source_line(self.shared.file_id, line);
        trace!(?line, ?col, ?source_line, "analyzing");
    }

    /// # Errors
    ///
    #[allow(clippy::too_many_lines)]
    pub fn analyze_expression(
        &mut self,
        ast_expression: &swamp_script_ast::Expression,
        context: &TypeContext,
    ) -> Result<Expression, Error> {
        //self.debug_expression(&ast_expression);

        let expr = self.analyze_expression_internal(ast_expression, context)?;

        let encountered_type = expr.ty.clone();

        if let Some(found_expected_type) = context.expected_type {
            if found_expected_type.compatible_with(&encountered_type) {
                return Ok(expr);
            }

            let result = self.coerce_expression(
                expr,
                found_expected_type,
                &encountered_type,
                &ast_expression.node,
            )?;

            return Ok(result);
        }

        Ok(expr)
    }

    /// # Errors
    ///
    #[allow(clippy::too_many_lines)]
    pub fn analyze_expression_internal(
        &mut self,
        ast_expression: &swamp_script_ast::Expression,
        context: &TypeContext,
    ) -> Result<Expression, Error> {
        let expression = match &ast_expression.kind {
            swamp_script_ast::ExpressionKind::Break => {
                self.analyze_break(context, &ast_expression.node)?
            }
            swamp_script_ast::ExpressionKind::Return(optional_expression) => self.analyze_return(
                context,
                optional_expression.as_deref(),
                &ast_expression.node,
            )?,

            swamp_script_ast::ExpressionKind::Continue => {
                self.analyze_continue(context, &ast_expression.node)?
            }

            // Lookups
            swamp_script_ast::ExpressionKind::PostfixChain(postfix_chain) => {
                self.analyze_postfix_chain(postfix_chain)?
            }

            swamp_script_ast::ExpressionKind::VariableDefinition(
                variable,
                maybe_annotation,
                source_expression,
            ) => self.analyze_create_variable(
                variable,
                Option::from(maybe_annotation),
                source_expression,
            )?,

            swamp_script_ast::ExpressionKind::VariableAssignment(variable, source_expression) => {
                self.analyze_variable_assignment(variable, source_expression)?
            }
            swamp_script_ast::ExpressionKind::DestructuringAssignment(variables, expression) => {
                self.analyze_destructuring(&ast_expression.node, variables, expression)?
            }

            swamp_script_ast::ExpressionKind::IdentifierReference(qualified_identifier) => {
                self.analyze_identifier(qualified_identifier)?
            }
            swamp_script_ast::ExpressionKind::VariableReference(variable) => {
                self.analyze_variable_reference(&variable.name)?
            }

            swamp_script_ast::ExpressionKind::StaticMemberFunctionReference(
                type_identifier,
                member_name,
            ) => self.analyze_static_member_access(type_identifier, member_name)?,

            swamp_script_ast::ExpressionKind::ConstantReference(constant_identifier) => {
                self.analyze_constant_access(constant_identifier)?
            }

            swamp_script_ast::ExpressionKind::Assignment(location, source) => {
                self.analyze_assignment(location, source)?
            }
            swamp_script_ast::ExpressionKind::CompoundAssignment(target, op, source) => {
                self.analyze_assignment_compound(target, op, source)?
            }

            // Operator
            swamp_script_ast::ExpressionKind::BinaryOp(resolved_a, operator, resolved_b) => {
                let (resolved_op, result_type) =
                    self.analyze_binary_op(resolved_a, operator, resolved_b)?;

                self.create_expr(
                    ExpressionKind::BinaryOp(resolved_op),
                    result_type,
                    &ast_expression.node,
                )
            }
            swamp_script_ast::ExpressionKind::UnaryOp(operator, expression) => {
                let (resolved_op, result_type) = self.analyze_unary_op(operator, expression)?;
                self.create_expr(
                    ExpressionKind::UnaryOp(resolved_op),
                    result_type,
                    &ast_expression.node,
                )
            }

            swamp_script_ast::ExpressionKind::Block(expressions) => {
                let (block, resulting_type) =
                    self.analyze_block(&ast_expression.node, context, expressions)?;
                self.create_expr(
                    ExpressionKind::Block(block),
                    resulting_type,
                    &ast_expression.node,
                )
            }

            swamp_script_ast::ExpressionKind::With(variable_bindings, expression) => {
                self.analyze_with_expr(context, variable_bindings, expression)?
            }

            swamp_script_ast::ExpressionKind::When(variable_bindings, true_expr, else_expr) => {
                self.analyze_when_expr(context, variable_bindings, true_expr, else_expr.as_deref())?
            }

            swamp_script_ast::ExpressionKind::InterpolatedString(string_parts) => {
                let kind = ExpressionKind::InterpolatedString(
                    self.analyze_interpolated_string(string_parts)?,
                );

                self.create_expr(kind, Type::String, &ast_expression.node)
            }

            // Creation
            swamp_script_ast::ExpressionKind::NamedStructLiteral(
                struct_identifier,
                fields,
                has_rest,
            ) => self.analyze_struct_instantiation(struct_identifier, fields, *has_rest)?,

            swamp_script_ast::ExpressionKind::AnonymousStructLiteral(
                fields,
                rest_was_specified,
            ) => self.analyze_anonymous_struct_literal(
                &ast_expression.node,
                fields,
                *rest_was_specified,
                context,
            )?,

            swamp_script_ast::ExpressionKind::Range(min_value, max_value, range_mode) => {
                let range = self.analyze_range(min_value, max_value, range_mode)?;
                self.create_expr(
                    ExpressionKind::Range(Box::from(range.min), Box::from(range.max), range.mode),
                    Type::Iterable(Box::from(Type::Int)),
                    &ast_expression.node,
                )
            }

            swamp_script_ast::ExpressionKind::Literal(literal) => {
                self.analyze_complex_literal_to_expression(&ast_expression.node, literal, context)?
            }

            swamp_script_ast::ExpressionKind::ForLoop(pattern, iterable_expression, statements) => {
                let resolved_iterator =
                    self.analyze_iterable(pattern.any_mut(), &iterable_expression.expression)?;

                self.push_block_scope("for_loop");
                let pattern = self.analyze_for_pattern(
                    pattern,
                    resolved_iterator.key_type.as_ref(),
                    &resolved_iterator.value_type,
                )?;
                let resolved_statements =
                    self.analyze_expression(statements, &context.enter_loop())?;
                self.pop_block_scope("for_loop");
                let resolved_type = resolved_statements.ty.clone();
                self.create_expr(
                    ExpressionKind::ForLoop(
                        pattern,
                        resolved_iterator,
                        Box::from(resolved_statements),
                    ),
                    resolved_type,
                    &ast_expression.node,
                )
            }
            swamp_script_ast::ExpressionKind::WhileLoop(expression, statements) => {
                let condition = self.analyze_bool_argument_expression(expression)?;
                //self.push_block_scope("while_loop");
                let resolved_statements =
                    self.analyze_expression(statements, &context.enter_loop())?;
                let resolved_type = resolved_statements.ty.clone();
                //self.pop_block_scope("while_loop");

                self.create_expr(
                    ExpressionKind::WhileLoop(condition, Box::from(resolved_statements)),
                    resolved_type,
                    &ast_expression.node,
                )
            }

            swamp_script_ast::ExpressionKind::If(
                expression,
                true_expression,
                maybe_false_expression,
            ) => self.analyze_if_expression(
                expression,
                true_expression,
                maybe_false_expression.as_deref(),
                context,
            )?,

            swamp_script_ast::ExpressionKind::Match(expression, arms) => {
                let (match_expr, return_type) = self.analyze_match(expression, context, arms)?;
                self.create_expr(
                    ExpressionKind::Match(match_expr),
                    return_type,
                    &ast_expression.node,
                )
            }
            swamp_script_ast::ExpressionKind::Guard(guard_expressions) => {
                self.analyze_guard(&ast_expression.node, context, guard_expressions)?
            }
        };

        //info!(ty=%expression.ty, kind=?expression.kind,  "resolved expression");

        Ok(expression)
    }

    fn get_struct_type(
        &mut self,
        qualified_type_identifier: &swamp_script_ast::QualifiedTypeIdentifier,
    ) -> Result<NamedStructType, Error> {
        let maybe_struct_type = self.analyze_named_type(qualified_type_identifier)?;
        match maybe_struct_type {
            Type::NamedStruct(struct_type) => Ok(struct_type),
            _ => Err(self.create_err(
                // For other Type variants that are not Struct
                ErrorKind::UnknownStructTypeReference,
                &qualified_type_identifier.name.0,
            )),
        }
    }

    pub(crate) fn analyze_named_type(
        &mut self,
        type_name_to_find: &swamp_script_ast::QualifiedTypeIdentifier,
    ) -> Result<Type, Error> {
        let (path, name) = self.get_path(type_name_to_find);

        if path.is_empty() {
            // Check if it is a type variable first!
            if let Some(found_scope) = &self.shared.type_variables {
                if let Some(found_type) = found_scope.type_variables.get(&name) {
                    return Ok(found_type.clone());
                }
            }
        }

        let symbol = {
            let maybe_symbol_table = self.shared.get_symbol_table(&path);
            let symbol_table = maybe_symbol_table.ok_or_else(|| {
                self.create_err(ErrorKind::UnknownSymbol, &type_name_to_find.name.0)
            })?;
            symbol_table
                .get_symbol(&name)
                .ok_or_else(|| {
                    self.create_err(ErrorKind::UnknownSymbol, &type_name_to_find.name.0)
                })?
                .clone()
        };

        let mut analyzed_type_parameters = Vec::new();

        for analyzed_type in &type_name_to_find.generic_params {
            let ty = self.analyze_type(analyzed_type)?;

            analyzed_type_parameters.push(ty);
        }

        let result_type = if !analyzed_type_parameters.is_empty() {
            self.analyze_generic_type(
                &symbol,
                &analyzed_type_parameters,
                &type_name_to_find.name.0,
            )?
        } else {
            match &symbol {
                Symbol::Type(base_type) => base_type.clone(),
                Symbol::Alias(alias_type) => alias_type.referenced_type.clone(),
                _ => {
                    return Err(
                        self.create_err(ErrorKind::UnexpectedType, &type_name_to_find.name.0)
                    );
                }
            }
        };

        Ok(result_type)
    }

    fn create_default_value_for_type(
        &mut self,
        node: &swamp_script_ast::Node,
        field_type: &Type,
    ) -> Result<Expression, Error> {
        let kind = match field_type {
            Type::Bool => ExpressionKind::Literal(Literal::BoolLiteral(false)),
            Type::Int => ExpressionKind::Literal(Literal::IntLiteral(0)),
            Type::Float => ExpressionKind::Literal(Literal::FloatLiteral(Fp::zero())),
            Type::String => ExpressionKind::Literal(Literal::StringLiteral(String::new())),
            Type::Tuple(tuple_type_ref) => {
                let mut expressions = Vec::new();
                for resolved_type in tuple_type_ref {
                    let expr = self.create_default_value_for_type(node, resolved_type)?;
                    expressions.push(expr);
                }
                ExpressionKind::Literal(Literal::TupleLiteral(tuple_type_ref.clone(), expressions))
            }
            Type::Optional(_optional_type) => ExpressionKind::Literal(Literal::NoneLiteral),

            Type::NamedStruct(struct_ref) => {
                self.create_default_static_call(node, &Type::NamedStruct(struct_ref.clone()))?
            }
            _ => {
                return Err(
                    self.create_err(ErrorKind::NoDefaultImplemented(field_type.clone()), node)
                );
            }
        };

        let expr = self.create_expr(kind, field_type.clone(), node);
        Ok(expr)
    }

    fn create_static_call(
        &mut self,
        function_name: &str,
        arguments: &[ArgumentExpressionOrLocation],
        node: &swamp_script_ast::Node,
        ty: &Type,
    ) -> Result<ExpressionKind, Error> {
        self.lookup_associated_function(ty, function_name)
            .map_or_else(
                || {
                    Err(self.create_err(
                        ErrorKind::NoAssociatedFunction(ty.clone(), function_name.to_string()),
                        node,
                    ))
                },
                |function| {
                    let kind = match &*function {
                        Function::Internal(internal_function) => {
                            ExpressionKind::InternalFunctionAccess(internal_function.clone())
                        }
                        Function::External(external_function) => {
                            ExpressionKind::ExternalFunctionAccess(external_function.clone())
                        }
                    };

                    let base_expr =
                        self.create_expr(kind, Type::Function(function.signature().clone()), node);

                    let empty_call_postfix = Postfix {
                        node: self.to_node(node),
                        ty: *function.signature().return_type.clone(),
                        kind: PostfixKind::FunctionCall(arguments.to_vec()),
                    };

                    let kind =
                        ExpressionKind::PostfixChain(Box::new(base_expr), vec![empty_call_postfix]);

                    Ok(kind)
                },
            )
    }

    fn create_default_static_call(
        &mut self,
        node: &swamp_script_ast::Node,
        ty: &Type,
    ) -> Result<ExpressionKind, Error> {
        self.create_static_call("default", &[], node, ty)
    }

    fn add_postfix(
        &mut self,
        vec: &mut Vec<Postfix>,
        kind: PostfixKind,
        ty: Type,
        node: &swamp_script_ast::Node,
    ) {
        let resolved_node = self.to_node(node);
        let postfix = Postfix {
            node: resolved_node,
            ty,
            kind,
        };

        vec.push(postfix);
    }

    /// # Panics
    ///
    /// # Errors
    ///
    pub fn analyze_struct_field(
        &mut self,
        field_name: &swamp_script_ast::Node,
        tv: Type,
    ) -> Result<(AnonymousStructType, usize, Type), Error> {
        let field_name_str = self.get_text(field_name).to_string();

        let anon_struct_ref = match &tv {
            Type::NamedStruct(struct_type) => struct_type.anon_struct_type.clone(),
            Type::AnonymousStruct(anon_struct) => anon_struct.clone(),
            _ => return Err(self.create_err(ErrorKind::UnknownStructField, field_name)),
        };

        if let Some(found_field) = anon_struct_ref
            .field_name_sorted_fields
            .get(&field_name_str)
        {
            let index = anon_struct_ref
                .field_name_sorted_fields
                .get_index(&field_name_str)
                .expect("checked earlier");

            return Ok((
                anon_struct_ref.clone(),
                index,
                found_field.field_type.clone(),
            ));
        }

        Err(self.create_err(ErrorKind::UnknownStructField, field_name))
    }

    #[allow(clippy::too_many_lines)]
    fn analyze_postfix_chain(
        &mut self,
        chain: &swamp_script_ast::PostfixChain,
    ) -> Result<Expression, Error> {
        let (start, is_mutable) =
            self.analyze_start_chain_expression_get_mutability(&chain.base, None)?;

        if let ExpressionKind::IntrinsicFunctionAccess(some_access) = &start.kind {
            assert_eq!(chain.postfixes.len(), 1);
            let call_postifx = &chain.postfixes[0];
            if let swamp_script_ast::Postfix::FunctionCall(member, arguments) = &call_postifx {
                let resolved_arguments = self.analyze_and_verify_parameters(
                    &start.node,
                    &some_access.signature.parameters,
                    &arguments,
                )?;

                return Ok(self.create_expr(
                    ExpressionKind::IntrinsicCallEx(
                        some_access.intrinsic.clone(),
                        resolved_arguments,
                    ),
                    *some_access.signature.return_type.clone(),
                    &chain.base.node,
                ));
            } else {
                panic!("not sure here");
            }
        }

        let mut tv = TypeWithMut {
            resolved_type: start.ty.clone(),
            is_mutable,
        };

        let mut uncertain = false;

        let mut suffixes = Vec::new();

        for item in &chain.postfixes {
            match item {
                swamp_script_ast::Postfix::FieldAccess(field_name) => {
                    let (struct_type_ref, index, return_type) =
                        self.analyze_struct_field(&field_name.clone(), tv.resolved_type)?;
                    self.add_postfix(
                        &mut suffixes,
                        PostfixKind::StructField(struct_type_ref.clone(), index),
                        return_type.clone(),
                        field_name,
                    );

                    tv.resolved_type = return_type.clone();
                    // keep previous `is_mutable`
                }
                swamp_script_ast::Postfix::MemberCall(member_name, ast_arguments) => {
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
                    } else {
                        let member_name_str = self.get_text(member_name).to_string();

                        if let Some(found_member) = self
                            .shared
                            .state
                            .associated_impls
                            .get_member_function(&tv.resolved_type, &member_name_str)
                        {
                            let return_type = self.analyze_postfix_member_call(
                                &tv.resolved_type,
                                tv.is_mutable,
                                member_name,
                                ast_arguments,
                                &mut suffixes,
                            )?;

                            //self.add_postfix(&mut suffixes, kind, return_type.clone(), member_name);
                            tv.resolved_type = return_type.clone();
                            tv.is_mutable = false;
                        } else {
                            return Err(
                                self.create_err(ErrorKind::UnknownMemberFunction, member_name)
                            );
                        }
                    }
                }
                swamp_script_ast::Postfix::FunctionCall(node, arguments) => {
                    if let Type::Function(signature) = &tv.resolved_type {
                        let resolved_node = self.to_node(node);
                        let resolved_arguments = self.analyze_and_verify_parameters(
                            &resolved_node,
                            &signature.parameters,
                            arguments,
                        )?;

                        let call_kind = PostfixKind::FunctionCall(resolved_arguments);

                        self.add_postfix(
                            &mut suffixes,
                            call_kind,
                            *signature.return_type.clone(),
                            node,
                        );

                        tv.resolved_type = *signature.return_type.clone();
                        tv.is_mutable = false;
                    } else {
                        panic!("{}", &format!("what is this type {:?} ", tv.resolved_type))
                    }
                }

                swamp_script_ast::Postfix::Subscript(index_expr) => {
                    let collection_type = tv.resolved_type.clone();
                    if let Some(found) = self
                        .shared
                        .state
                        .associated_impls
                        .get_member_function(&tv.resolved_type, "subscript")
                        .cloned()
                    {
                        let cloned = found.clone();
                        let required_type = &found.signature().parameters[1].resolved_type;
                        let subscript_lookup_context = TypeContext::new_argument(&required_type);
                        let analyzed_expr =
                            self.analyze_expression(index_expr, &subscript_lookup_context)?;

                        let return_type = *found.signature().return_type.clone();

                        let argument = ArgumentExpressionOrLocation::Expression(analyzed_expr);
                        self.add_postfix(
                            &mut suffixes,
                            PostfixKind::MemberCall(cloned, vec![argument]),
                            return_type.clone(),
                            &index_expr.node,
                        );
                        tv.resolved_type = return_type.clone();
                    } else {
                        error!(?collection_type, "missing subscript");
                        return Err(
                            self.create_err(ErrorKind::MissingSubscriptMember, &index_expr.node)
                        );
                    }
                }

                swamp_script_ast::Postfix::NoneCoalesce(default_expr) => {
                    let unwrapped_type = if let Type::Optional(unwrapped_type) = &tv.resolved_type {
                        unwrapped_type
                    } else if uncertain {
                        &tv.resolved_type
                    } else {
                        return Err(
                            self.create_err(ErrorKind::CanNotNoneCoalesce, &default_expr.node)
                        );
                    };

                    let unwrapped_type_context = TypeContext::new_argument(unwrapped_type);
                    let resolved_default_expr =
                        self.analyze_expression(default_expr, &unwrapped_type_context)?;
                    self.add_postfix(
                        &mut suffixes,
                        PostfixKind::NoneCoalesce(resolved_default_expr),
                        unwrapped_type.clone(),
                        &default_expr.node,
                    );
                    tv.resolved_type = unwrapped_type.clone();
                    uncertain = false; // the chain is safe, because this will always solve None
                }

                swamp_script_ast::Postfix::OptionUnwrap(option_node) => {
                    if let Type::Optional(unwrapped_type) = &tv.resolved_type {
                        uncertain = true;
                        self.add_postfix(
                            &mut suffixes,
                            PostfixKind::OptionUnwrap,
                            *unwrapped_type.clone(),
                            option_node,
                        );
                        tv.resolved_type = *unwrapped_type.clone();
                    } else {
                        return Err(self.create_err(ErrorKind::ExpectedOptional, option_node));
                    }
                }
            }
        }

        if uncertain {
            if let Type::Optional(_) = tv.resolved_type {
            } else {
                tv.resolved_type = Type::Optional(Box::from(tv.resolved_type.clone()));
            }
        }

        Ok(self.create_expr(
            ExpressionKind::PostfixChain(Box::new(start), suffixes),
            tv.resolved_type,
            &chain.base.node,
        ))
    }

    fn analyze_bool_argument_expression(
        &mut self,
        expression: &swamp_script_ast::Expression,
    ) -> Result<BooleanExpression, Error> {
        let bool_context = TypeContext::new_argument(&Type::Bool);
        let resolved_expression = self.analyze_expression(expression, &bool_context)?;
        let expr_type = resolved_expression.ty.clone();

        let bool_expression = match expr_type {
            Type::Bool => resolved_expression,
            Type::Optional(_) => self.create_expr(
                ExpressionKind::CoerceOptionToBool(Box::new(resolved_expression)),
                Type::Bool,
                &expression.node,
            ),
            _ => {
                return Err(self.create_err(ErrorKind::ExpectedBooleanExpression, &expression.node));
            }
        };

        Ok(BooleanExpression {
            expression: Box::from(bool_expression),
        })
    }

    fn analyze_iterable(
        &mut self,
        force_mut: Option<swamp_script_ast::Node>,
        expression: &swamp_script_ast::MutableOrImmutableExpression,
    ) -> Result<Iterable, Error> {
        let any_context = TypeContext::new_anything_argument();
        let resolved_expression: MutOrImmutableExpression = if force_mut.is_some() {
            let resolved_node = self.to_node(&force_mut.unwrap());
            MutOrImmutableExpression {
                expression_or_location: ArgumentExpressionOrLocation::Location(
                    self.analyze_to_location(
                        &expression.expression,
                        &any_context,
                        LocationSide::Rhs,
                    )?,
                ),
                is_mutable: Some(resolved_node),
            }
        } else {
            self.analyze_mut_or_immutable_expression(expression, &any_context, LocationSide::Rhs)?
        };

        let resolved_type = &resolved_expression.ty().clone();
        let (key_type, value_type): (Option<Type>, Type) = match resolved_type {
            Type::String => (Some(Type::Int), Type::String),
            Type::Iterable(item_type) => (None, *item_type.clone()),

            _ => {
                if let Some(found_iter_fn) = self
                    .shared
                    .state
                    .associated_impls
                    .get_internal_member_function(resolved_type, "iter")
                {
                    let ret_type = found_iter_fn.signature.return_type.clone();
                    match *ret_type {
                        Type::Tuple(tuple_items) => {
                            (Some(tuple_items[0].clone()), tuple_items[1].clone())
                        }
                        _ => {
                            return Err(self.create_err(
                                ErrorKind::NotAnIterator,
                                &expression.expression.node,
                            ));
                        }
                    }
                } else {
                    return Err(
                        self.create_err(ErrorKind::NotAnIterator, &expression.expression.node)
                    );
                }
            }
        };

        Ok(Iterable {
            key_type,
            value_type,
            resolved_expression: Box::new(resolved_expression),
        })
    }

    fn analyze_argument_expressions(
        &mut self,
        expected_type: Option<&Type>,
        ast_expressions: &[swamp_script_ast::Expression],
    ) -> Result<Vec<Expression>, Error> {
        let mut resolved_expressions = Vec::new();
        let argument_expressions_context = TypeContext::new_unsure_argument(expected_type);
        for expression in ast_expressions {
            resolved_expressions
                .push(self.analyze_expression(expression, &argument_expressions_context)?);
        }
        Ok(resolved_expressions)
    }

    fn analyze_block(
        &mut self,
        _node: &swamp_script_ast::Node,
        context: &TypeContext,
        ast_expressions: &[swamp_script_ast::Expression],
    ) -> Result<(Vec<Expression>, Type), Error> {
        if ast_expressions.is_empty() {
            return Ok((vec![], Type::Unit));
        }

        self.push_block_scope("block");

        let mut resolved_expressions = Vec::with_capacity(ast_expressions.len());

        for expression in &ast_expressions[..ast_expressions.len() - 1] {
            let stmt_context = context.with_expected_type(Some(&Type::Unit));
            let expr = self.analyze_expression(expression, &stmt_context)?;

            if matches!(expr.ty, Type::Never) {
                resolved_expressions.push(expr);
                return Ok((resolved_expressions, Type::Never));
            }

            resolved_expressions.push(expr);
        }

        // Process the last expression - it determines the block's type
        let last_expr =
            self.analyze_expression(&ast_expressions[ast_expressions.len() - 1], context)?;
        let last_type = last_expr.ty.clone();
        resolved_expressions.push(last_expr);

        self.pop_block_scope("block");

        Ok((resolved_expressions, last_type))
    }

    fn analyze_interpolated_string(
        &mut self,
        string_parts: &[swamp_script_ast::StringPart],
    ) -> Result<Vec<StringPart>, Error> {
        let mut resolved_parts = Vec::new();
        for part in string_parts {
            let resolved_string_part = match part {
                swamp_script_ast::StringPart::Literal(string_node, processed_string) => {
                    StringPart::Literal(self.to_node(string_node), processed_string.to_string())
                }
                swamp_script_ast::StringPart::Interpolation(expression, format_specifier) => {
                    let any_context = TypeContext::new_anything_argument();
                    let expr = self.analyze_expression(expression, &any_context)?;
                    let resolved_format_specifier =
                        self.analyze_format_specifier(Option::from(format_specifier));
                    StringPart::Interpolation(expr, resolved_format_specifier)
                }
            };

            resolved_parts.push(resolved_string_part);
        }

        Ok(resolved_parts)
    }

    pub(crate) fn analyze_identifier(
        &self,
        qualified_func_name: &swamp_script_ast::QualifiedIdentifier,
    ) -> Result<Expression, Error> {
        // Must check variable first, since that is more intuitive for the user.
        // local variables before other functions
        if qualified_func_name.module_path.is_none()
            && qualified_func_name.generic_params.is_empty()
        {
            if let Some(found_variable) = self.try_find_variable(&qualified_func_name.name) {
                return Ok(self.create_expr(
                    ExpressionKind::VariableAccess(found_variable.clone()),
                    found_variable.resolved_type.clone(),
                    &qualified_func_name.name,
                ));
            }
        }

        let path = self.get_module_path(qualified_func_name.module_path.as_ref());
        let function_name = self.get_text(&qualified_func_name.name);

        if let Some(found_table) = self.shared.get_symbol_table(&path) {
            if let Some(found_func) = found_table.get_function(function_name) {
                let (kind, signature) = match found_func {
                    FuncDef::Internal(internal_fn) => (
                        ExpressionKind::InternalFunctionAccess(internal_fn.clone()),
                        &internal_fn.signature,
                    ),
                    FuncDef::External(external_fn) => (
                        ExpressionKind::ExternalFunctionAccess(external_fn.clone()),
                        &external_fn.signature,
                    ),
                    // Can not have a reference to an intrinsic function
                    FuncDef::Intrinsic(intrinsic_fn) => (
                        ExpressionKind::IntrinsicFunctionAccess(intrinsic_fn.clone()),
                        &intrinsic_fn.signature,
                    ),
                };

                return Ok(self.create_expr(
                    kind,
                    Type::Function(signature.clone()),
                    &qualified_func_name.name,
                ));
            }
        }
        Err(self.create_err(ErrorKind::UnknownFunction, &qualified_func_name.name))
    }

    // The ast assumes it is something similar to a variable, but it can be a function reference as well.
    fn analyze_variable_reference(
        &self,
        var_node: &swamp_script_ast::Node,
    ) -> Result<Expression, Error> {
        let text = self.get_text(var_node);

        if let Some(found_variable) = self.try_find_variable(&var_node) {
            return Ok(self.create_expr(
                ExpressionKind::VariableAccess(found_variable.clone()),
                found_variable.resolved_type.clone(),
                &var_node,
            ));
        }
        Err(self.create_err(ErrorKind::UnknownIdentifier, var_node))
    }

    fn analyze_usize_index(
        &mut self,
        usize_expression: &swamp_script_ast::Expression,
    ) -> Result<Expression, Error> {
        let int_context = TypeContext::new_argument(&Type::Int);
        let lookup_expression = self.analyze_expression(usize_expression, &int_context)?;
        let lookup_resolution = lookup_expression.ty.clone();

        match &lookup_resolution {
            Type::Int => {}
            _ => Err(self.create_err(
                ErrorKind::ArrayIndexMustBeInt(lookup_resolution),
                &usize_expression.node,
            ))?,
        }

        Ok(lookup_expression)
    }

    fn analyze_slice_type_helper(
        &mut self,
        node: &swamp_script_ast::Node,
        items: &[swamp_script_ast::Expression],
    ) -> Result<(Type, Vec<Expression>), Error> {
        let expressions = self.analyze_argument_expressions(None, items)?;
        let element_type = if expressions.is_empty() {
            Type::Unit
        } else {
            expressions[0].ty.clone()
        };

        Ok((element_type, expressions))
    }

    fn push_block_scope(&mut self, _debug_str: &str) {
        self.scope.block_scope_stack.push(BlockScope {
            mode: BlockScopeMode::Open,
            variables: SeqMap::default(),
        });
    }

    fn pop_block_scope(&mut self, _debug_str: &str) {
        self.scope.block_scope_stack.pop();
    }

    fn push_closed_block_scope(&mut self) {
        self.scope.block_scope_stack.push(BlockScope {
            mode: BlockScopeMode::Closed,
            variables: SeqMap::default(),
        });
    }

    fn pop_closed_block_scope(&mut self) {
        self.scope.block_scope_stack.pop();
    }

    fn analyze_enum_variant_ref(
        &self,
        qualified_type_identifier: &swamp_script_ast::QualifiedTypeIdentifier,
        variant_name: &swamp_script_ast::LocalTypeIdentifier,
    ) -> Result<EnumVariantType, Error> {
        let variant_name_string = self.get_text(&variant_name.0).to_string();
        self.get_enum_variant_type(qualified_type_identifier, &variant_name_string)
    }

    fn analyze_match(
        &mut self,
        scrutinee: &swamp_script_ast::MutableOrImmutableExpression,
        default_context: &TypeContext,
        arms: &Vec<swamp_script_ast::MatchArm>,
    ) -> Result<(Match, Type), Error> {
        let mut known_type = default_context.expected_type.cloned();
        let own_context = default_context.clone();
        // Analyze the scrutinee with no specific expected type
        let scrutinee_context = TypeContext::new_anything_argument();
        let resolved_scrutinee = self.analyze_mut_or_immutable_expression(
            scrutinee,
            &scrutinee_context,
            LocationSide::Rhs,
        )?;
        let scrutinee_type = resolved_scrutinee.ty().clone();

        // Ensure we have at least one arm
        if arms.is_empty() {
            return Err(self.create_err(ErrorKind::EmptyMatch, &scrutinee.expression.node));
        }

        let mut resolved_arms = Vec::with_capacity(arms.len());

        for arm in arms {
            let (resolved_arm, _anyone_wants_mutable) = self.analyze_arm(
                arm,
                &resolved_scrutinee,
                &own_context.with_expected_type(known_type.as_ref()),
                &scrutinee_type,
            )?;

            if known_type.is_none() && !matches!(resolved_arm.expression.ty, Type::Never) {
                known_type = Some(resolved_arm.expression.ty.clone());
            }
            resolved_arms.push(resolved_arm);
        }

        known_type.map_or_else(
            || {
                Err(self.create_err(
                    ErrorKind::MatchArmsMustHaveTypes,
                    &scrutinee.expression.node,
                ))
            },
            |encountered_type| {
                if matches!(encountered_type, Type::Never) {
                    Err(self.create_err(
                        ErrorKind::IncompatibleTypes(encountered_type.clone(), encountered_type),
                        &scrutinee.expression.node,
                    ))
                } else {
                    Ok((
                        Match {
                            expression: Box::new(resolved_scrutinee),
                            arms: resolved_arms,
                        },
                        encountered_type,
                    ))
                }
            },
        )
    }

    fn analyze_arm(
        &mut self,
        arm: &swamp_script_ast::MatchArm,
        _expression: &MutOrImmutableExpression,
        type_context: &TypeContext,
        expected_condition_type: &Type,
    ) -> Result<(MatchArm, bool), Error> {
        let (resolved_pattern, scope_was_pushed, anyone_wants_mutable) =
            self.analyze_pattern(&arm.pattern, expected_condition_type)?;

        let resolved_expression = self.analyze_expression(&arm.expression, type_context)?;
        if scope_was_pushed {
            self.pop_block_scope("analyze_arm");
        }

        let resolved_type = resolved_expression.ty.clone();

        Ok((
            MatchArm {
                pattern: resolved_pattern,
                expression: Box::from(resolved_expression),
                expression_type: resolved_type,
            },
            anyone_wants_mutable,
        ))
    }

    fn str_to_int(text: &str) -> Result<i32, ParseIntError> {
        text.parse::<i32>()
    }

    fn str_to_float(text: &str) -> Result<f32, ParseFloatError> {
        text.parse::<f32>()
    }

    fn analyze_pattern_literal(
        &mut self,
        node: &swamp_script_ast::Node,
        ast_literal: &swamp_script_ast::LiteralKind,
        expected_condition_type: &Type,
    ) -> Result<NormalPattern, Error> {
        let required_condition_type_context = TypeContext::new_argument(expected_condition_type);
        let (literal, ty) =
            self.analyze_literal(node, ast_literal, &required_condition_type_context)?;

        if !ty.compatible_with(expected_condition_type) {
            return Err(self.create_err(
                ErrorKind::IncompatibleTypes(ty, expected_condition_type.clone()),
                node,
            ));
        }

        Ok(NormalPattern::Literal(literal))
    }

    const fn to_node(&self, node: &swamp_script_ast::Node) -> Node {
        Node {
            span: Span {
                file_id: self.shared.file_id,
                offset: node.span.offset,
                length: node.span.length,
            },
        }
    }

    fn get_module_path(&self, module_path: Option<&swamp_script_ast::ModulePath>) -> Vec<String> {
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
        qualified_type_identifier: &swamp_script_ast::QualifiedTypeIdentifier,
        variant_name: &str,
    ) -> Result<EnumVariantType, Error> {
        let (symbol_table, enum_name) =
            self.get_symbol_table_and_name(qualified_type_identifier)?;
        symbol_table
            .get_enum_variant_type(&enum_name, variant_name)
            .ok_or_else(|| {
                self.create_err(
                    ErrorKind::UnknownEnumVariantType,
                    &qualified_type_identifier.name.0,
                )
            })
    }

    pub(crate) fn get_symbol_table_and_name(
        &self,
        type_identifier: &swamp_script_ast::QualifiedTypeIdentifier,
    ) -> Result<(&SymbolTable, String), Error> {
        let path = self.get_module_path(type_identifier.module_path.as_ref());
        let name = self.get_text(&type_identifier.name.0).to_string();

        let maybe_symbol_table = self.shared.get_symbol_table(&path);
        maybe_symbol_table.map_or_else(
            || Err(self.create_err(ErrorKind::UnknownModule, &type_identifier.name.0)),
            |symbol_table| Ok((symbol_table, name)),
        )
    }

    const fn analyze_compound_operator(
        &self,
        ast_operator: &swamp_script_ast::CompoundOperator,
    ) -> CompoundOperator {
        let resolved_node = self.to_node(&ast_operator.node);
        let resolved_kind = match ast_operator.kind {
            swamp_script_ast::CompoundOperatorKind::Add => CompoundOperatorKind::Add,
            swamp_script_ast::CompoundOperatorKind::Sub => CompoundOperatorKind::Sub,
            swamp_script_ast::CompoundOperatorKind::Mul => CompoundOperatorKind::Mul,
            swamp_script_ast::CompoundOperatorKind::Div => CompoundOperatorKind::Div,
            swamp_script_ast::CompoundOperatorKind::Modulo => CompoundOperatorKind::Modulo,
        };

        CompoundOperator {
            node: resolved_node,
            kind: resolved_kind,
        }
    }

    const fn to_node_option(&self, maybe_node: Option<&swamp_script_ast::Node>) -> Option<Node> {
        match maybe_node {
            None => None,
            Some(node) => Some(self.to_node(node)),
        }
    }

    const fn analyze_format_specifier(
        &self,
        ast_format_specifier: Option<&swamp_script_ast::FormatSpecifier>,
    ) -> Option<FormatSpecifier> {
        let f = match ast_format_specifier {
            None => return None,
            Some(ast_format) => match ast_format {
                swamp_script_ast::FormatSpecifier::LowerHex(node) => FormatSpecifier {
                    node: self.to_node(node),
                    kind: FormatSpecifierKind::LowerHex,
                },
                swamp_script_ast::FormatSpecifier::UpperHex(node) => FormatSpecifier {
                    node: self.to_node(node),
                    kind: FormatSpecifierKind::UpperHex,
                },
                swamp_script_ast::FormatSpecifier::Binary(node) => FormatSpecifier {
                    node: self.to_node(node),
                    kind: FormatSpecifierKind::Binary,
                },
                swamp_script_ast::FormatSpecifier::Float(node) => FormatSpecifier {
                    node: self.to_node(node),
                    kind: FormatSpecifierKind::Float,
                },
                swamp_script_ast::FormatSpecifier::Precision(value, node, x) => {
                    let (precision_type, precision_node) = match x {
                        swamp_script_ast::PrecisionType::Float(node) => {
                            (PrecisionType::Float, self.to_node(node))
                        }
                        swamp_script_ast::PrecisionType::String(node) => {
                            (PrecisionType::String, self.to_node(node))
                        }
                    };
                    FormatSpecifier {
                        node: self.to_node(node),
                        kind: FormatSpecifierKind::Precision(
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

    fn analyze_with_expr(
        &mut self,
        context: &TypeContext,
        variables: &[swamp_script_ast::VariableBinding],
        expression: &swamp_script_ast::Expression,
    ) -> Result<Expression, Error> {
        let mut variable_expressions = Vec::new();

        for variable in variables {
            let any_context = TypeContext::new_anything_argument();
            let var = self.analyze_mut_or_immutable_expression(
                &variable.expression,
                &any_context,
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

        let resolved_expression = self.analyze_expression(expression, context)?;
        let block_type = resolved_expression.ty.clone();
        expressions.push(resolved_expression);

        let block_expression_kind = ExpressionKind::Block(expressions);
        self.pop_closed_block_scope();

        let block_expr = self.create_expr(block_expression_kind, block_type, &expression.node);
        Ok(block_expr)
    }

    fn analyze_when_expr(
        &mut self,
        context: &TypeContext,
        variables: &[swamp_script_ast::WhenBinding],
        true_expr: &swamp_script_ast::Expression,
        else_expr: Option<&swamp_script_ast::Expression>,
    ) -> Result<Expression, Error> {
        // Since we are making new variable bindings, we have to push a scope for them
        self.push_block_scope("when");
        let mut bindings = Vec::new();
        for variable_binding in variables {
            let mut_expr = if let Some(found_expr) = &variable_binding.expression {
                let any_context = TypeContext::new_anything_argument();
                self.analyze_mut_or_immutable_expression(
                    found_expr,
                    &any_context,
                    LocationSide::Rhs,
                )?
            } else {
                let same_var = self.find_variable(&variable_binding.variable)?;

                let is_mutable = same_var.mutable_node.clone();
                let argument_expression = if same_var.is_mutable() {
                    let loc = SingleLocationExpression {
                        kind: SingleLocationExpressionKind::MutVariableRef,
                        node: self.to_node(&variable_binding.variable.name),
                        ty: same_var.resolved_type.clone(),
                        starting_variable: same_var,
                        access_chain: vec![],
                    };
                    ArgumentExpressionOrLocation::Location(loc)
                } else {
                    let generated_expr_kind = ExpressionKind::VariableAccess(same_var.clone());
                    let generated_expression = self.create_expr(
                        generated_expr_kind,
                        same_var.resolved_type.clone(),
                        &variable_binding.variable.name,
                    );
                    ArgumentExpressionOrLocation::Expression(generated_expression)
                };

                MutOrImmutableExpression {
                    expression_or_location: argument_expression,
                    is_mutable,
                }
            };

            let ty = mut_expr.ty();
            let debug_text = self.get_text(&variable_binding.variable.name).to_string();

            if let Type::Optional(found_ty) = ty {
                let variable_ref = self.create_variable(&variable_binding.variable, found_ty)?;

                let binding = WhenBinding {
                    variable: variable_ref,
                    expr: mut_expr,
                };
                bindings.push(binding);
            } else {
                return Err(self.create_err(ErrorKind::ExpectedOptional, &true_expr.node));
            }
        }

        let resolved_true = self.analyze_expression(true_expr, context)?;
        let block_type = resolved_true.ty.clone();

        self.pop_block_scope("when");

        let maybe_resolved_else = if let Some(found_else) = else_expr {
            let block_type_for_true_context = context.we_know_expected_type(&block_type);
            Some(Box::new(self.analyze_expression(
                found_else,
                &block_type_for_true_context,
            )?))
        } else {
            None
        };

        let when_kind =
            ExpressionKind::When(bindings, Box::from(resolved_true), maybe_resolved_else);

        let block_expr = self.create_expr(when_kind, block_type, &true_expr.node);
        Ok(block_expr)
    }

    fn analyze_guard(
        &mut self,
        node: &swamp_script_ast::Node,
        context: &TypeContext,
        guard_expressions: &Vec<swamp_script_ast::GuardExpr>,
    ) -> Result<Expression, Error> {
        let mut guards = Vec::new();
        let mut found_wildcard = None;
        let mut detected_type = context.expected_type.cloned();

        for guard in guard_expressions {
            let resolved_condition = match &guard.clause {
                swamp_script_ast::GuardClause::Wildcard(x) => {
                    if found_wildcard.is_some() {
                        return Err(
                            self.create_err(ErrorKind::GuardCanNotHaveMultipleWildcards, node)
                        );
                    }
                    found_wildcard = Some(x);
                    None
                }
                swamp_script_ast::GuardClause::Expression(clause_expr) => {
                    if found_wildcard.is_some() {
                        return Err(self.create_err(ErrorKind::WildcardMustBeLastInGuard, node));
                    }
                    Some(self.analyze_bool_argument_expression(clause_expr)?)
                }
            };

            let resolved_result = self.analyze_expression(
                &guard.result,
                &context.with_expected_type(detected_type.as_ref()),
            )?;
            let ty = resolved_result.ty.clone();
            if detected_type.is_none() && !matches!(ty, Type::Never) {
                detected_type = Some(ty.clone());
            }

            guards.push(Guard {
                condition: resolved_condition,
                result: resolved_result,
            });
        }

        if found_wildcard.is_none() {
            return Err(self.create_err(ErrorKind::GuardMustHaveWildcard, node));
        }

        let kind = ExpressionKind::Guard(guards);

        detected_type.map_or_else(
            || Err(self.create_err(ErrorKind::GuardHasNoType, node)),
            |found_expecting_type| {
                let expr = self.create_expr(kind, found_expecting_type, node);
                Ok(expr)
            },
        )
    }

    /// # Errors
    ///
    pub fn analyze_variable_assignment(
        &mut self,
        variable: &swamp_script_ast::Variable,
        source_expression: &swamp_script_ast::MutableOrImmutableExpression,
    ) -> Result<Expression, Error> {
        let any_argument_context = TypeContext::new_anything_argument();
        let source_expr = self.analyze_mut_or_immutable_expression(
            source_expression,
            &any_argument_context,
            LocationSide::Rhs,
        )?;
        let ty = source_expr.ty().clone();
        if !ty.is_concrete() {
            return Err(self.create_err(ErrorKind::VariableTypeMustBeConcrete, &variable.name));
        }

        let maybe_found_variable = self.try_find_variable(&variable.name);

        let kind: ExpressionKind = if let Some(found_var) = maybe_found_variable {
            if !found_var.is_mutable() {
                return Err(self.create_err(ErrorKind::VariableIsNotMutable, &variable.name));
            }
            if !found_var.resolved_type.assignable_type(&ty) {
                return Err(self.create_err(
                    ErrorKind::IncompatibleTypes(found_var.resolved_type.clone(), ty.clone()),
                    &variable.name,
                ));
            }
            ExpressionKind::VariableReassignment(found_var, Box::from(source_expr))
        } else {
            let new_var = self.create_variable(variable, &ty)?;
            ExpressionKind::VariableDefinition(new_var, Box::from(source_expr))
        };

        Ok(self.create_expr(kind, Type::Unit, &variable.name))
    }

    fn analyze_create_variable(
        &mut self,
        var: &swamp_script_ast::Variable,
        annotation_type: Option<&swamp_script_ast::Type>,
        source_expression: &swamp_script_ast::MutableOrImmutableExpression,
    ) -> Result<Expression, Error> {
        let ty = if let Some(found_ast_type) = annotation_type {
            Some(self.analyze_type(found_ast_type)?)
        } else {
            None
        };

        let unsure_arg_context = TypeContext::new_unsure_argument(ty.as_ref());

        let resolved_source = self.analyze_mut_or_immutable_expression(
            source_expression,
            &unsure_arg_context,
            LocationSide::Rhs,
        )?;

        let var_ref = self.create_local_variable(
            &var.name,
            Option::from(&var.is_mutable),
            resolved_source.ty(),
        )?;

        let resolved_type = resolved_source.ty().clone();
        assert_ne!(resolved_type, Type::Unit);
        let kind = ExpressionKind::VariableDefinition(var_ref, Box::from(resolved_source));

        let resolved_expr = self.create_expr(kind, Type::Unit, &var.name);

        Ok(resolved_expr)
    }

    fn add_location_item(
        &mut self,
        vec: &mut Vec<LocationAccess>,
        kind: LocationAccessKind,
        ty: Type,
        ast_node: &swamp_script_ast::Node,
    ) {
        let resolved_node = self.to_node(ast_node);
        let postfix = LocationAccess {
            node: resolved_node.clone(),
            ty,
            kind,
        };

        vec.push(postfix);
    }

    #[allow(clippy::too_many_lines)]
    fn analyze_chain_to_location(
        &mut self,
        chain: &swamp_script_ast::PostfixChain,
        context: &TypeContext,
        location_side: LocationSide,
    ) -> Result<SingleLocationExpression, Error> {
        let mut items = Vec::new();

        let nothing_context =
            TypeContext::new(None, None, TypeContextScope::ArgumentOrOutsideFunction);

        let base_expr = self.analyze_expression(&chain.base, &nothing_context)?;
        let ExpressionKind::VariableAccess(start_variable) = base_expr.kind else {
            return Err(self.create_err(ErrorKind::NotValidLocationStartingPoint, &chain.base.node));
        };

        if !start_variable.is_mutable() {
            return Err(self.create_err(ErrorKind::VariableIsNotMutable, &chain.base.node));
        }

        let mut ty = start_variable.resolved_type.clone();
        for (i, item) in chain.postfixes.iter().enumerate() {
            match &item {
                swamp_script_ast::Postfix::FieldAccess(field_name_node) => {
                    //let field_name_resolved = self.to_node(field_name_node)
                    let (struct_type_ref, index, return_type) =
                        self.analyze_struct_field(field_name_node, ty)?;
                    self.add_location_item(
                        &mut items,
                        LocationAccessKind::FieldIndex(struct_type_ref.clone(), index),
                        return_type.clone(),
                        field_name_node,
                    );

                    ty = return_type.clone();
                }
                swamp_script_ast::Postfix::Subscript(key_expression) => {
                    let is_last_in_chain = i == chain.postfixes.len() - 1;
                    let create_if_not_exists =
                        is_last_in_chain && location_side == LocationSide::Lhs;

                    let subscript_member_function_name = if create_if_not_exists {
                        "subscript_mut_create_if_needed"
                    } else {
                        "subscript_mut"
                    };

                    if let Some(found) = self
                        .shared
                        .state
                        .associated_impls
                        .get_internal_member_function(&ty, subscript_member_function_name)
                        .cloned()
                    {
                        let intrinsic_to_call = match &found.body.kind {
                            ExpressionKind::Block(expressions) => {
                                assert_eq!(expressions.len(), 1);
                                let first_kind = &expressions[0].kind;
                                if let ExpressionKind::IntrinsicCallEx(intrinsic_fn, args) =
                                    first_kind
                                {
                                    intrinsic_fn.clone()
                                } else {
                                    panic!("must be postfix");
                                }
                            }
                            _ => panic!("illegal subscript_mut"),
                        };

                        let create_if_not_exists_bool_expr = self.create_expr(
                            ExpressionKind::Literal(Literal::BoolLiteral(create_if_not_exists)),
                            Type::Bool,
                            &chain.base.node,
                        );

                        let required_type = &found.signature.parameters[1].resolved_type;
                        let subscript_lookup_context = TypeContext::new_argument(&required_type);
                        let analyzed_key_expression =
                            self.analyze_expression(key_expression, &subscript_lookup_context)?;
                        let return_type = *found.signature.return_type.clone();
                        ty = return_type.clone();
                        //let argument = ArgumentExpressionOrLocation::Expression(analyzed_expr);
                        self.add_location_item(
                            &mut items,
                            LocationAccessKind::IntrinsicCallMut(
                                intrinsic_to_call,
                                vec![analyzed_key_expression, create_if_not_exists_bool_expr],
                            ),
                            return_type,
                            &key_expression.node,
                        );
                    } else {
                        return Err(self
                            .create_err(ErrorKind::MissingSubscriptMember, &key_expression.node));
                    }
                }

                swamp_script_ast::Postfix::MemberCall(node, _) => {
                    return Err(self.create_err(ErrorKind::CallsCanNotBePartOfChain, node));
                }

                swamp_script_ast::Postfix::FunctionCall(node, _) => {
                    return Err(self.create_err(ErrorKind::CallsCanNotBePartOfChain, node));
                }
                swamp_script_ast::Postfix::OptionUnwrap(node) => {
                    return Err(self.create_err(ErrorKind::UnwrapCanNotBePartOfChain, node));
                }
                swamp_script_ast::Postfix::NoneCoalesce(expr) => {
                    return Err(
                        self.create_err(ErrorKind::NoneCoalesceCanNotBePartOfChain, &expr.node)
                    );
                }
            }
        }

        if let Some(found_expected_type) = context.expected_type {
            if !ty.compatible_with(found_expected_type) {
                return Err(self.create_err(
                    ErrorKind::IncompatibleTypes(ty, found_expected_type.clone()),
                    &chain.base.node,
                ));
            }
        }

        let location = SingleLocationExpression {
            kind: SingleLocationExpressionKind::MutVariableRef,
            node: self.to_node(&chain.base.node),
            ty,
            starting_variable: start_variable,
            access_chain: items,
        };
        Ok(location)
    }

    fn analyze_to_location(
        &mut self,
        expr: &swamp_script_ast::Expression,
        context: &TypeContext,
        location_type: LocationSide,
    ) -> Result<SingleLocationExpression, Error> {
        match &expr.kind {
            swamp_script_ast::ExpressionKind::PostfixChain(chain) => {
                self.analyze_chain_to_location(chain, context, location_type)
            }
            swamp_script_ast::ExpressionKind::VariableReference(variable) => {
                let var = self.find_variable(variable)?;
                if var.is_mutable() {
                    Ok(SingleLocationExpression {
                        kind: SingleLocationExpressionKind::MutVariableRef,
                        node: self.to_node(&variable.name),
                        ty: var.resolved_type.clone(),
                        starting_variable: var,
                        access_chain: vec![],
                    })
                } else {
                    Err(self.create_err(ErrorKind::VariableIsNotMutable, &expr.node))
                }
            }
            swamp_script_ast::ExpressionKind::IdentifierReference(qualified_identifier) => {
                let generated_var = swamp_script_ast::Variable {
                    name: qualified_identifier.name.clone(),
                    is_mutable: None,
                };
                let var = self.find_variable(&generated_var)?;
                if var.is_mutable() {
                    Ok(SingleLocationExpression {
                        kind: SingleLocationExpressionKind::MutVariableRef,
                        node: self.to_node(&generated_var.name),
                        ty: var.resolved_type.clone(),
                        starting_variable: var,
                        access_chain: vec![],
                    })
                } else {
                    Err(self.create_err(ErrorKind::VariableIsNotMutable, &expr.node))
                }
            }
            _ => {
                error!(?expr.kind, "error");
                Err(self.create_err(ErrorKind::NotValidLocationStartingPoint, &expr.node))
            }
        }
    }

    fn analyze_assignment_compound(
        &mut self,
        target_expression: &swamp_script_ast::Expression,
        ast_op: &swamp_script_ast::CompoundOperator,
        ast_source_expression: &swamp_script_ast::Expression,
    ) -> Result<Expression, Error> {
        let resolved_op = self.analyze_compound_operator(ast_op);
        let any_argument_context = TypeContext::new_anything_argument();
        let source_expr = self.analyze_expression(ast_source_expression, &any_argument_context)?;
        let source_expr_type_context = TypeContext::new_argument(&source_expr.ty);

        let resolved_location = SingleMutLocationExpression(self.analyze_to_location(
            target_expression,
            &source_expr_type_context,
            LocationSide::Rhs,
        )?);

        let kind = ExpressionKind::CompoundAssignment(
            resolved_location,
            resolved_op.kind,
            Box::from(source_expr),
        );

        let expr = self.create_expr(kind, Type::Unit, &target_expression.node);

        Ok(expr)
    }

    fn analyze_assignment(
        &mut self,
        target_location: &swamp_script_ast::Expression,
        ast_source_expression: &swamp_script_ast::Expression,
    ) -> Result<Expression, Error> {
        let any_argument_context = TypeContext::new_anything_argument();
        let resolved_location =
            self.analyze_to_location(target_location, &any_argument_context, LocationSide::Lhs)?;

        let ty = resolved_location.ty.clone();
        if ty == Type::Unit {
            error!(?ast_source_expression, "unit problem");
        }

        let lhs_argument_context = TypeContext::new_argument(&ty);
        let source_expr = self.analyze_expression(ast_source_expression, &lhs_argument_context)?;

        let mut_location = SingleMutLocationExpression(resolved_location);

        let kind = ExpressionKind::Assignment(Box::from(mut_location), Box::from(source_expr));

        let expr = self.create_expr(kind, Type::Unit, &target_location.node); // Assignments are always of type Unit

        Ok(expr)
    }

    #[must_use]
    pub fn create_mut_single_location_expr(
        &self,
        kind: SingleLocationExpressionKind,
        ty: Type,
        ast_node: &swamp_script_ast::Node,
    ) -> SingleMutLocationExpression {
        SingleMutLocationExpression(SingleLocationExpression {
            kind,
            ty,
            starting_variable: Rc::new(Variable {
                name: Node::default(),
                resolved_type: Type::Int,
                mutable_node: None,
                scope_index: 0,
                variable_index: 0,
            }),
            node: self.to_node(ast_node),
            access_chain: vec![],
        })
    }

    #[must_use]
    pub fn create_single_location_expr(
        &self,
        kind: SingleLocationExpressionKind,
        ty: Type,
        ast_node: &swamp_script_ast::Node,
    ) -> SingleLocationExpression {
        SingleLocationExpression {
            kind,
            ty,
            starting_variable: Rc::new(Variable {
                name: Node::default(),
                resolved_type: Type::Int,
                mutable_node: None,
                scope_index: 0,
                variable_index: 0,
            }),
            node: self.to_node(ast_node),
            access_chain: vec![],
        }
    }

    #[must_use]
    pub fn create_single_location_expr_resolved(
        &self,
        kind: SingleLocationExpressionKind,
        ty: Type,
        node: &Node,
    ) -> SingleLocationExpression {
        SingleLocationExpression {
            kind,
            ty,
            starting_variable: Rc::new(Variable {
                name: Node::default(),
                resolved_type: Type::Int,
                mutable_node: None,
                scope_index: 0,
                variable_index: 0,
            }),
            node: node.clone(),
            access_chain: vec![],
        }
    }
    #[must_use]
    pub fn create_mut_single_location_expr_resolved(
        &self,
        kind: SingleLocationExpressionKind,
        ty: Type,
        node: &Node,
    ) -> SingleMutLocationExpression {
        SingleMutLocationExpression(SingleLocationExpression {
            kind,
            ty,
            starting_variable: Rc::new(Variable {
                name: Node::default(),
                resolved_type: Type::Int,
                mutable_node: None,
                scope_index: 0,
                variable_index: 0,
            }),
            node: node.clone(),
            access_chain: vec![],
        })
    }

    #[must_use]
    pub const fn create_expr(
        &self,
        kind: ExpressionKind,
        ty: Type,
        ast_node: &swamp_script_ast::Node,
    ) -> Expression {
        //info!(%ty, ?kind, "create_expr()");
        Expression {
            kind,
            ty,
            node: self.to_node(ast_node),
        }
    }

    #[must_use]
    pub fn create_expr_resolved(
        &self,
        kind: ExpressionKind,
        ty: Type,
        ast_node: &Node,
    ) -> Expression {
        Expression {
            kind,
            ty,
            node: ast_node.clone(),
        }
    }

    fn analyze_destructuring(
        &mut self,
        node: &swamp_script_ast::Node,
        target_ast_variables: &[swamp_script_ast::Variable],
        tuple_expression: &swamp_script_ast::Expression,
    ) -> Result<Expression, Error> {
        let any_context = TypeContext::new_anything_argument();
        let tuple_resolved = self.analyze_expression(tuple_expression, &any_context)?;
        let tuple_expr_type = &tuple_resolved.ty;

        let mut variable_refs = Vec::new();
        if let Type::Tuple(tuple) = tuple_expr_type.clone() {
            if target_ast_variables.len() > tuple.len() {
                return Err(self.create_err(ErrorKind::TooManyDestructureVariables, node));
            }
            for (variable_ref, tuple_type) in target_ast_variables.iter().zip(tuple.clone()) {
                let (variable_ref, _is_reassignment) =
                    self.set_or_overwrite_variable_with_type(variable_ref, &tuple_type)?;
                variable_refs.push(variable_ref);
            }
            let expr_kind =
                ExpressionKind::TupleDestructuring(variable_refs, tuple, Box::from(tuple_resolved));

            Ok(self.create_expr(expr_kind, Type::Unit, node))
        } else {
            Err(self.create_err(ErrorKind::CanNotDestructure, node))
        }
    }

    fn analyze_postfix_member_func_call(
        &mut self,
        resolved_node: &Node,
        found_function: &FunctionRef,
        encountered_self_type: &Type,
        is_mutable: bool,
        arguments: &[swamp_script_ast::MutableOrImmutableExpression],
    ) -> Result<Postfix, Error> {
        let signature = found_function.signature();

        let self_type = &signature.parameters[0];
        if !self_type
            .resolved_type
            .compatible_with(encountered_self_type)
            || self_type.is_mutable && !is_mutable
        {
            return Err(self.create_err_resolved(ErrorKind::SelfNotCorrectType, resolved_node));
        }

        let resolved_arguments = self.analyze_and_verify_parameters(
            resolved_node,
            &signature.parameters[1..],
            arguments,
        )?;

        let kind = PostfixKind::MemberCall(found_function.clone(), resolved_arguments);
        let postfix = Postfix {
            node: resolved_node.clone(),
            ty: *signature.return_type.clone(),
            kind,
        };

        Ok(postfix)
    }

    fn analyze_postfix_field_call(
        &mut self,
        resolved_node: &Node,
        struct_type: &NamedStructType,
        field: &StructTypeField,
        index: usize,
        signature: &Signature,
        arguments: &[swamp_script_ast::MutableOrImmutableExpression],
    ) -> Result<Vec<Postfix>, Error> {
        let mut suffixes = Vec::new();
        //let field_name_str = self.get_text(member_name).to_string();
        let struct_field_kind =
            PostfixKind::StructField(struct_type.anon_struct_type.clone(), index);

        let struct_field_postfix = Postfix {
            node: resolved_node.clone(),
            ty: field.field_type.clone(),
            kind: struct_field_kind,
        };

        suffixes.push(struct_field_postfix);

        let resolved_arguments =
            self.analyze_and_verify_parameters(resolved_node, &signature.parameters, arguments)?;

        let call_kind = PostfixKind::FunctionCall(resolved_arguments);

        let call_postfix = Postfix {
            node: resolved_node.clone(),
            ty: *signature.return_type.clone(),
            kind: call_kind,
        };
        suffixes.push(call_postfix);

        Ok(suffixes)
    }

    fn analyze_postfix_member_call(
        &mut self,
        type_that_member_is_on: &Type,
        is_mutable: bool,
        member_name: &swamp_script_ast::Node,
        arguments: &[swamp_script_ast::MutableOrImmutableExpression],
        suffixes: &mut Vec<Postfix>,
    ) -> Result<Type, Error> {
        let field_name_str = self.get_text(member_name).to_string();

        let resolved_node = self.to_node(member_name);

        let maybe_function = self
            .shared
            .state
            .associated_impls
            .get_member_function(type_that_member_is_on, &field_name_str)
            .cloned();

        let postfixes = match maybe_function {
            Some(found_function_member) => {
                let postfix = self.analyze_postfix_member_func_call(
                    &resolved_node,
                    &found_function_member,
                    type_that_member_is_on,
                    is_mutable,
                    arguments,
                )?;
                vec![postfix]
            }
            _ => {
                return Err(self.create_err(ErrorKind::NotValidLocationStartingPoint, member_name));
            } // TODO: Support function calls
        };

        let last_type = postfixes.last().unwrap().ty.clone();
        suffixes.extend(postfixes);

        Ok(last_type)
    }

    fn analyze_break(
        &self,
        context: &TypeContext,
        node: &swamp_script_ast::Node,
    ) -> Result<Expression, Error> {
        if !context.allows_break() {
            return Err(self.create_err(ErrorKind::BreakOutsideLoop, node));
        }

        Ok(Expression {
            kind: ExpressionKind::Break,
            ty: Type::Never,
            node: self.to_node(node),
        })
    }

    fn analyze_return(
        &mut self,
        context: &TypeContext,
        optional_expression: Option<&swamp_script_ast::Expression>,
        node: &swamp_script_ast::Node,
    ) -> Result<Expression, Error> {
        if !context.allows_return() {
            return Err(self.create_err(ErrorKind::ReturnOutsideCompare, node));
        }

        let return_context = context.for_return();
        let inner = if let Some(expr) = optional_expression {
            Some(Box::new(self.analyze_expression(expr, &return_context)?))
        } else {
            // Empty return
            None
        };

        Ok(self.create_expr(ExpressionKind::Return(inner), Type::Never, node))
    }

    fn analyze_continue(
        &self,
        context: &TypeContext,
        node: &swamp_script_ast::Node,
    ) -> Result<Expression, Error> {
        if !context.allows_continue() {
            return Err(self.create_err(ErrorKind::ContinueOutsideLoop, node));
        }
        Ok(self.create_expr(ExpressionKind::Continue, Type::Never, node))
    }

    fn coerce_expression(
        &self,
        expr: Expression,
        expected_type: &Type,
        encountered_type: &Type,
        node: &swamp_script_ast::Node,
    ) -> Result<Expression, Error> {
        if !matches!(encountered_type, Type::Optional(_)) {
            // If an optional is expected, we can wrap it
            if let Type::Optional(expected_inner_type) = expected_type {
                if encountered_type.compatible_with(expected_inner_type) {
                    let wrapped = self.create_expr(
                        ExpressionKind::Option(Option::from(Box::new(expr))),
                        expected_type.clone(),
                        node,
                    );
                    return Ok(wrapped);
                }
            }
        } else if matches!(expected_type, &Type::Bool) {
            if let Type::Optional(_inner_type) = encountered_type {
                let wrapped = self.create_expr(
                    ExpressionKind::CoerceOptionToBool(Box::from(expr)),
                    Type::Bool,
                    node,
                );
                return Ok(wrapped);
            }
        }

        Ok(expr)
    }

    fn instantiate_blueprint_and_members(
        &mut self,
        blueprint: &ParameterizedTypeBlueprint,
        analyzed_type_parameters: &[Type],
    ) -> Result<Type, Error> {
        assert!(all_types_are_concrete_or_unit(analyzed_type_parameters));

        if let Some(existing) = self.shared.state.instantiation_cache.get(
            &blueprint.defined_in_module_path,
            &blueprint.name(),
            analyzed_type_parameters,
        ) {
            debug!(?blueprint, "found existing type");
            return Ok(existing.clone());
        }

        let scope = Instantiator::create_type_parameter_scope_from_variables(
            &blueprint.type_variables,
            analyzed_type_parameters,
        )?;
        let (_ignore_if_changed, instantiated_type) =
            Instantiator::instantiate_blueprint(blueprint, &scope)?;

        let new_impls = {
            let mut new_impls = SeqMap::new();
            let maybe_member_functions = self
                .shared
                .state
                .associated_impls
                .functions
                .get(&Type::Blueprint(blueprint.clone()));
            if let Some(found_member_functions) = maybe_member_functions {
                for (func_name, func_ref) in &found_member_functions.functions {
                    let (_replaced, new_signature) = Instantiator::instantiate_signature(
                        func_ref.signature().clone(),
                        &instantiated_type,
                        &scope,
                    )?;
                    let new_func = match &**func_ref {
                        Function::Internal(internal) => {
                            let func_ref = Rc::new(InternalFunctionDefinition {
                                body: internal.body.clone(),
                                name: LocalIdentifier(Default::default()),
                                assigned_name: "".to_string(),
                                signature: new_signature,
                            });
                            Function::Internal(func_ref)
                        }
                        Function::External(blueprint_external) => {
                            let func_ref = Rc::new(ExternalFunctionDefinition {
                                name: None,
                                assigned_name: "".to_string(),
                                signature: new_signature,
                                id: blueprint_external.id,
                            });
                            Function::External(func_ref)
                        }
                    };
                    new_impls.insert(func_name.clone(), new_func).unwrap();
                }
            }
            new_impls
        };

        self.shared
            .state
            .associated_impls
            .prepare(&instantiated_type);
        for (name, func) in &new_impls {
            //info!(?name, ?func, id=?instantiated_type, "ADDING");
            self.shared.state.associated_impls.add_member_function(
                &instantiated_type,
                &*name,
                func.clone().into(),
            )?;
        }

        self.shared
            .state
            .instantiation_cache
            .add(
                &blueprint.defined_in_module_path,
                &blueprint.name(),
                instantiated_type.clone(),
                analyzed_type_parameters,
            )
            .unwrap();

        Ok(instantiated_type)
    }

    #[allow(clippy::too_many_lines)]
    fn analyze_generic_type(
        &mut self,
        symbol: &Symbol,
        analyzed_type_parameters: &[Type],
        node: &swamp_script_ast::Node,
    ) -> Result<Type, Error> {
        let ty = match symbol {
            //            Symbol::Type(base_type) => base_type.clone(),
            //          Symbol::Alias(alias_type) => alias_type.referenced_type.clone(),
            Symbol::Blueprint(blueprint) => {
                if all_types_are_concrete_or_unit(analyzed_type_parameters) {
                    self.instantiate_blueprint_and_members(blueprint, analyzed_type_parameters)?
                } else {
                    Type::Generic(blueprint.clone(), analyzed_type_parameters.to_vec())
                }
            }
            _ => return Err(self.create_err(ErrorKind::UnknownSymbol, &node)),
        };

        Ok(ty)
    }
}
