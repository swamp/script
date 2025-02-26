/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod access;
pub mod call;
pub mod constant;
pub mod def;
pub mod err;
pub mod literal;
pub mod lookup;
pub mod operator;
pub mod pattern;
pub mod prelude;
mod structure;
pub mod types;
pub mod variable;

use crate::err::{Error, ErrorKind};
use seq_map::SeqMap;
use std::mem::take;
use std::num::{ParseFloatError, ParseIntError};
use std::rc::Rc;
use swamp_script_modules::modules::Modules;
use swamp_script_modules::symtbl::{FuncDef, Symbol, SymbolTable, SymbolTableRef};
use swamp_script_semantic::prelude::*;
use swamp_script_semantic::{
    ArgumentExpressionOrLocation, IntrinsicFunctionDefinitionRef, LocationAccess,
    LocationAccessKind, MutOrImmutableExpression, NormalPattern, Postfix, PostfixKind, RangeMode,
    SingleLocationExpression, SingleLocationExpressionKind, SingleMutLocationExpression,
    TypeWithMut, WhenBinding,
};
use swamp_script_source_map::SourceMap;
use tracing::error;
use tracing::info;

#[must_use]
pub fn convert_range_mode(range_mode: &RangeMode) -> RangeMode {
    match range_mode {
        RangeMode::Inclusive => RangeMode::Inclusive,
        RangeMode::Exclusive => RangeMode::Exclusive,
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum LocationSide {
    Lhs,
    Rhs,
}

#[derive(Debug)]
pub struct AutoUseModules {
    pub modules: Vec<SymbolTableRef>,
}

#[derive(Debug)]
pub struct Program {
    pub state: ProgramState,
    pub modules: Modules,
    pub auto_use_modules: AutoUseModules,
}

impl Program {}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

impl Program {
    #[must_use]
    pub fn new() -> Self {
        Self {
            state: ProgramState::new(),
            modules: Modules::new(),
            auto_use_modules: AutoUseModules {
                modules: Vec::new(),
            },
        }
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
    pub current_path: Vec<String>,
}

impl<'a> SharedState<'a> {
    #[must_use]
    pub fn get_symbol_table(&'a self, path: &[String]) -> Option<&'a SymbolTable> {
        if path.is_empty() {
            return Some(&self.lookup_table);
        }
        let resolved_path = {
            self.lookup_table.get_package_version(&path[0]).map_or_else(
                || path.to_vec(),
                |found_version| {
                    let mut new_path = path.to_vec();
                    let complete_name = format!("{}-{found_version}", path[0]);
                    info!(path=?path[0], found_version, complete_name, "switched out version");
                    new_path[0] = complete_name;
                    new_path
                },
            )
        };

        if path.len() == 1 {
            if let Some(module_ref) = self.lookup_table.get_module_link(&path[0]) {
                return Some(&module_ref.namespace.symbol_table);
            }
        }

        if let Some(x) = self.modules.get(&resolved_path) {
            return Some(&x.namespace.symbol_table);
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
}

impl<'a> Analyzer<'a> {
    pub fn new(
        state: &'a mut ProgramState,
        modules: &'a Modules,
        source_map: &'a SourceMap,
        current_path: &[String],
        file_id: FileId,
    ) -> Self {
        let shared = SharedState {
            state,
            lookup_table: SymbolTable::default(),
            definition_table: SymbolTable::default(),
            modules,
            source_map,
            current_path: current_path.to_vec(),
            file_id,
        };
        Self {
            scope: FunctionScopeState::new(Type::Unit),
            global: FunctionScopeState::new(Type::Unit),
            shared,
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
        expected_type: Option<&Type>,
        true_expression: &swamp_script_ast::Expression,
        maybe_false_expression: Option<&swamp_script_ast::Expression>,
    ) -> Result<Expression, Error> {
        let resolved_condition = self.analyze_bool_expression(condition)?;
        self.push_block_scope("if-true");
        let true_expr = self.analyze_expression(true_expression, expected_type)?;
        self.pop_block_scope("if-true");
        let if_result_type = true_expr.ty.clone();
        // For the true branch
        let resolved_true = Box::new(true_expr);

        // For the else branch
        let else_statements = if let Some(false_expression) = maybe_false_expression {
            self.push_block_scope("if-false");
            let else_expr =
                Box::new(self.analyze_expression(false_expression, Some(&if_result_type))?);
            self.pop_block_scope("if-false");
            Some(else_expr)
        } else {
            None
        };

        Ok(self.create_expr(
            ExpressionKind::If(resolved_condition, resolved_true, else_statements),
            if_result_type,
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

    fn get_path(
        &mut self,
        ident: &swamp_script_ast::QualifiedTypeIdentifier,
    ) -> (Vec<String>, String) {
        let name = self.get_text(&ident.name.0).to_string();
        let complete_name = { name };

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
        (path, complete_name)
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
        let resolved_statement = self.analyze_expression(expression, Some(return_type))?;
        //let wrapped_expr = Self::check_and_wrap_return_value(resolved_statement, return_type)?;

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

    fn current_function_return_type(&self) -> Type {
        self.scope.return_type.clone()
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

    #[must_use]
    pub fn lookup_associated_function(
        &self,
        ty: &Type,
        function_name: &str,
    ) -> Option<&FunctionRef> {
        info!(%ty, ?function_name, "looking up member function");

        self.shared
            .state
            .associated_impls
            .get_member_function(ty, function_name)
            .map(|function| function)
    }

    pub fn analyze_expression_get_mutability(
        &mut self,
        ast_expression: &swamp_script_ast::Expression,
        expected_type: Option<&Type>,
    ) -> Result<(Expression, bool), Error> {
        let resolved = self.analyze_expression(ast_expression, expected_type)?;
        let mutability = match resolved.kind {
            ExpressionKind::VariableAccess(ref resolved_variable) => resolved_variable.is_mutable(),
            _ => false,
        };

        Ok((resolved, mutability))
    }

    /// # Errors
    ///
    #[allow(clippy::too_many_lines)]
    pub fn analyze_expression(
        &mut self,
        ast_expression: &swamp_script_ast::Expression,
        expected_type: Option<&Type>,
    ) -> Result<Expression, Error> {
        let expr = self.analyze_expression_internal(ast_expression, expected_type)?;
        let encountered_type = expr.ty.clone();
        if let Some(found_expected_type) = expected_type {
            if found_expected_type.same_type(&encountered_type) {
                return Ok(expr);
            } else if !matches!(encountered_type, Type::Optional(_)) {
                // If an optional is expected, we can wrap it
                if let Type::Optional(expected_inner_type) = found_expected_type {
                    if encountered_type.same_type(expected_inner_type) {
                        let wrapped = self.create_expr(
                            ExpressionKind::Option(Option::from(Box::new(expr))),
                            found_expected_type.clone(),
                            &ast_expression.node,
                        );
                        return Ok(wrapped);
                    }
                }
            } else if matches!(found_expected_type, &Type::Bool) {
                if let Type::Optional(_inner_type) = encountered_type {
                    let wrapped = self.create_expr(
                        ExpressionKind::CoerceOptionToBool(Box::from(expr)),
                        Type::Bool,
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
                ErrorKind::IncompatibleTypes(found_expected_type.clone(), encountered_type),
                &ast_expression.node,
            ));
        }

        Ok(expr)
    }

    /// # Errors
    ///
    #[allow(clippy::too_many_lines)]
    pub fn analyze_expression_internal(
        &mut self,
        ast_expression: &swamp_script_ast::Expression,
        expected_type: Option<&Type>,
    ) -> Result<Expression, Error> {
        //info!(?ast_expression, "resolving");
        let expression = match &ast_expression.kind {
            // Lookups
            swamp_script_ast::ExpressionKind::PostfixChain(postfix_chain) => {
                if let swamp_script_ast::ExpressionKind::StaticFunctionReference(
                    qualified_func_name,
                ) = &postfix_chain.base.kind
                {
                    let path = self.get_module_path(qualified_func_name.module_path.as_ref());
                    let function_name = self.get_text(&qualified_func_name.name.0);

                    let maybe_intrinsic: Option<IntrinsicFunctionDefinitionRef> = {
                        self.shared.get_symbol_table(&path).map_or_else(
                            || None,
                            |found_table| {
                                found_table.get_intrinsic_function(function_name).cloned()
                            },
                        )
                    };

                    if let Some(check_intrinsic) = maybe_intrinsic {
                        if postfix_chain.postfixes.len() == 1 {
                            if let swamp_script_ast::Postfix::FunctionCall(_node, arguments) =
                                &postfix_chain.postfixes[0]
                            {
                                let analyzed_arguments = self.analyze_and_verify_parameters(
                                    &self.to_node(&qualified_func_name.name.0),
                                    &check_intrinsic.signature.parameters,
                                    arguments,
                                )?;
                                return Ok(self.create_expr(
                                    ExpressionKind::IntrinsicCall(
                                        check_intrinsic.intrinsic.clone(),
                                        analyzed_arguments,
                                    ),
                                    *check_intrinsic.signature.return_type.clone(),
                                    &qualified_func_name.name.0,
                                ));
                            }
                        } else {
                            return Err(
                                self.create_err(ErrorKind::UnknownIntrinsic, &ast_expression.node)
                            );
                        }
                    }
                }

                self.analyze_postfix_chain(postfix_chain)?
            }

            swamp_script_ast::ExpressionKind::IdentifierReference(variable) => {
                self.analyze_identifier_reference(&variable.name)?
            }
            swamp_script_ast::ExpressionKind::VariableDefinition(
                variable,
                coerce_type,
                source_expression,
            ) => self.analyze_create_variable(
                variable,
                Option::from(coerce_type),
                source_expression,
            )?,
            swamp_script_ast::ExpressionKind::VariableAssignment(variable, source_expression) => {
                self.analyze_variable_assignment(variable, source_expression)?
            }
            swamp_script_ast::ExpressionKind::DestructuringAssignment(variables, expression) => {
                self.analyze_destructuring(&ast_expression.node, variables, expression)?
            }

            swamp_script_ast::ExpressionKind::StaticMemberFunctionReference(
                type_identifier,
                member_name,
            ) => self.analyze_static_member_access(type_identifier, member_name)?,
            swamp_script_ast::ExpressionKind::StaticFunctionReference(qualified_identifier) => {
                self.analyze_static_function_access(qualified_identifier)?
            }

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
                    self.analyze_block(&ast_expression.node, expected_type, expressions)?;
                self.create_expr(
                    ExpressionKind::Block(block),
                    resulting_type,
                    &ast_expression.node,
                )
            }

            swamp_script_ast::ExpressionKind::With(variable_bindings, expression) => {
                self.analyze_with_expr(expected_type, variable_bindings, expression)?
            }

            swamp_script_ast::ExpressionKind::When(variable_bindings, true_expr, else_expr) => self
                .analyze_when_expr(
                    expected_type,
                    variable_bindings,
                    true_expr,
                    else_expr.as_deref(),
                )?,

            swamp_script_ast::ExpressionKind::InterpolatedString(string_parts) => {
                let kind = ExpressionKind::InterpolatedString(
                    self.analyze_interpolated_string(string_parts)?,
                );

                self.create_expr(kind, Type::String, &ast_expression.node)
            }

            // Creation
            swamp_script_ast::ExpressionKind::StructLiteral(
                struct_identifier,
                fields,
                has_rest,
            ) => self.analyze_struct_literal(struct_identifier, fields, *has_rest)?,
            swamp_script_ast::ExpressionKind::Range(min_value, max_value, range_mode) => {
                let range = self.analyze_range(min_value, max_value, range_mode)?;
                self.create_expr(
                    ExpressionKind::Range(Box::from(range.min), Box::from(range.max), range.mode),
                    Type::Range,
                    &ast_expression.node,
                )
            }

            swamp_script_ast::ExpressionKind::Literal(literal) => {
                let (literal, resolved_type) =
                    self.analyze_literal(&ast_expression.node, literal, expected_type)?;
                self.create_expr(
                    ExpressionKind::Literal(literal),
                    resolved_type,
                    &ast_expression.node,
                )
            }

            swamp_script_ast::ExpressionKind::ForLoop(
                pattern,
                iteratable_expression,
                statements,
            ) => {
                let resolved_iterator =
                    self.analyze_iterable(pattern.any_mut(), &iteratable_expression.expression)?;

                self.push_block_scope("for_loop");
                let pattern = self.analyze_for_pattern(
                    pattern,
                    resolved_iterator.key_type.as_ref(),
                    &resolved_iterator.value_type,
                )?;
                let resolved_statements = self.analyze_expression(statements, expected_type)?;
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
                let condition = self.analyze_bool_expression(expression)?;
                self.push_block_scope("while_loop");
                let resolved_statements = self.analyze_expression(statements, expected_type)?;
                let resolved_type = resolved_statements.ty.clone();
                self.pop_block_scope("while_loop");

                self.create_expr(
                    ExpressionKind::WhileLoop(condition, Box::from(resolved_statements)),
                    resolved_type,
                    &ast_expression.node,
                )
            }
            swamp_script_ast::ExpressionKind::Return(expr) => {
                let (wrapped_expr, return_type) = if let Some(found_expr) = expr {
                    let return_type = self.current_function_return_type();
                    let resolved_expr = self.analyze_expression(found_expr, Some(&return_type))?;
                    (Some(Box::new(resolved_expr)), return_type)
                } else {
                    (None, Type::Unit)
                };
                self.create_expr(
                    ExpressionKind::Return(wrapped_expr),
                    return_type,
                    &ast_expression.node,
                )
            }
            swamp_script_ast::ExpressionKind::Break => {
                self.create_expr(ExpressionKind::Break, Type::Unit, &ast_expression.node)
            }
            swamp_script_ast::ExpressionKind::Continue => {
                self.create_expr(ExpressionKind::Continue, Type::Unit, &ast_expression.node)
            }

            swamp_script_ast::ExpressionKind::If(
                expression,
                true_expression,
                maybe_false_expression,
            ) => self.analyze_if_expression(
                expression,
                expected_type,
                true_expression,
                maybe_false_expression.as_deref(),
            )?,
            swamp_script_ast::ExpressionKind::Match(expression, arms) => {
                let (match_expr, return_type) =
                    self.analyze_match(expression, expected_type, arms)?;
                self.create_expr(
                    ExpressionKind::Match(match_expr),
                    return_type,
                    &ast_expression.node,
                )
            }
            swamp_script_ast::ExpressionKind::Guard(guard_expressions) => {
                self.analyze_guard(&ast_expression.node, expected_type, guard_expressions)?
            }
        };

        //info!(ty=%expression.ty, kind=?expression.kind,  "resolved expression");

        Ok(expression)
    }

    fn get_struct_type(
        &mut self,
        qualified_type_identifier: &swamp_script_ast::QualifiedTypeIdentifier,
    ) -> Result<StructTypeRef, Error> {
        let maybe_struct_type = self.analyze_named_type(qualified_type_identifier)?;
        match maybe_struct_type {
            Type::Struct(struct_type) => Ok(struct_type),
            _ => Err(self.create_err(
                // For other Type variants that are not Struct
                ErrorKind::UnknownStructTypeReference,
                &qualified_type_identifier.name.0,
            )),
        }
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
                for resolved_type in &tuple_type_ref.0 {
                    let expr = self.create_default_value_for_type(node, resolved_type)?;
                    expressions.push(expr);
                }
                ExpressionKind::Literal(Literal::TupleLiteral(tuple_type_ref.clone(), expressions))
            }
            Type::Optional(_optional_type) => ExpressionKind::Literal(Literal::NoneLiteral),

            Type::Struct(struct_ref) => {
                self.create_default_static_call(node, &Type::Struct(struct_ref.clone()))?
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

    fn create_default_static_call(
        &mut self,
        node: &swamp_script_ast::Node,
        ty: &Type,
    ) -> Result<ExpressionKind, Error> {
        self.lookup_associated_function(ty, "default").map_or_else(
            || Err(self.create_err(ErrorKind::NoDefaultImplementedForType(ty.clone()), node)),
            |function_info| {
                let kind = Self::convert_to_function_access(&function_info);

                let base_expr = self.create_expr(
                    kind,
                    Type::Function(function_info.signature().clone()),
                    node,
                );

                let empty_call_postfix = Postfix {
                    node: self.to_node(node),
                    ty: *function_info.signature().return_type.clone(),
                    kind: PostfixKind::FunctionCall(vec![]),
                };

                let kind =
                    ExpressionKind::PostfixChain(Box::new(base_expr), vec![empty_call_postfix]);

                Ok(kind)
            },
        )
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

    pub fn analyze_struct_field(
        &self,
        field_name: &swamp_script_ast::Node,
        tv: &Type,
    ) -> Result<(StructTypeRef, usize, Type), Error> {
        let field_name_str = self.get_text(field_name).to_string();

        if let Type::Struct(struct_type) = &tv {
            if let Some(found_field) = struct_type
                .anon_struct_type
                .defined_fields
                .get(&field_name_str)
            {
                let index = struct_type
                    .anon_struct_type
                    .defined_fields
                    .get_index(&field_name_str)
                    .expect("checked earlier");

                return Ok((struct_type.clone(), index, found_field.field_type.clone()));
            }
        }

        Err(self.create_err(ErrorKind::UnknownStructField, field_name))
    }

    #[allow(clippy::too_many_lines)]
    fn analyze_postfix_chain(
        &mut self,
        chain: &swamp_script_ast::PostfixChain,
    ) -> Result<Expression, Error> {
        let (start, is_mutable) = self.analyze_expression_get_mutability(&chain.base, None)?;

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
                        self.analyze_struct_field(&field_name.clone(), &tv.resolved_type)?;
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
                    if let Type::Struct(struct_type) = &tv.resolved_type.clone() {
                        let return_type = self.analyze_postfix_member_call(
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
                        return Err(
                            self.create_err(ErrorKind::NotValidLocationStartingPoint, member_name)
                        );
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
                        self.add_postfix(
                            &mut suffixes,
                            PostfixKind::FunctionCall(resolved_arguments),
                            *signature.return_type.clone(),
                            node,
                        );

                        tv.resolved_type = *signature.return_type.clone();
                        tv.is_mutable = false;
                    };
                }

                swamp_script_ast::Postfix::Subscript(index_expr) => {
                    let collection_type = tv.resolved_type.clone();
                    match &collection_type {
                        Type::String => {
                            if let swamp_script_ast::ExpressionKind::Range(min, max, mode) =
                                &index_expr.kind
                            {
                                let range = self.analyze_range(min, max, mode)?;

                                self.add_postfix(
                                    &mut suffixes,
                                    PostfixKind::StringRangeIndex(range),
                                    collection_type.clone(),
                                    &index_expr.node,
                                );

                                tv.resolved_type = Type::String;
                            } else {
                                let resolved_index_expr =
                                    self.analyze_expression(index_expr, Some(&Type::Int))?;
                                self.add_postfix(
                                    &mut suffixes,
                                    PostfixKind::StringIndex(resolved_index_expr),
                                    Type::String,
                                    &index_expr.node,
                                );

                                tv.resolved_type = Type::String;
                            }

                            tv.is_mutable = false;
                        }

                        /* TODO: FIX
                        Type::Array(array_type_ref) => {
                            if let swamp_script_ast::ExpressionKind::Range(
                                min_expr,
                                max_expr,
                                mode,
                            ) = &index_expr.kind
                            {
                                let range = self.analyze_range(min_expr, max_expr, mode)?;

                                self.add_postfix(
                                    &mut suffixes,
                                    PostfixKind::ArrayRangeIndex(array_type_ref.clone(), range),
                                    collection_type.clone(),
                                    &index_expr.node,
                                );

                                tv.resolved_type = collection_type.clone();
                            } else {
                                let resolved_index_expr =
                                    self.analyze_expression(index_expr, Some(&Type::Int))?;
                                self.add_postfix(
                                    &mut suffixes,
                                    PostfixKind::ArrayIndex(
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

                        Type::Map(map_type_ref) => {
                            let resolved_key_expr =
                                self.analyze_expression(index_expr, Some(&map_type_ref.key_type))?;
                            let return_type =
                                Type::Optional(Box::from(map_type_ref.value_type.clone()));
                            self.add_postfix(
                                &mut suffixes,
                                PostfixKind::MapIndex(map_type_ref.clone(), resolved_key_expr),
                                return_type.clone(),
                                &index_expr.node,
                            );

                            tv.resolved_type = return_type;
                            tv.is_mutable = false;
                        }
                        */
                        Type::Struct(resolved_struct_ref) => {
                            let subscript_fn = self
                                .shared
                                .state
                                .associated_impls
                                .get_member_function(
                                    &Type::Struct(resolved_struct_ref.clone()),
                                    "subscript",
                                )
                                .unwrap();
                            let lookup_returns = &subscript_fn.signature().return_type;

                            tv.resolved_type = *lookup_returns.clone();
                            tv.is_mutable = false;
                        }

                        Type::External(found_rust_type) => {
                            if found_rust_type.number == SPARSE_TYPE_ID {
                                /*
                                let sparse_id = self
                                    .shared
                                    .lookup
                                    .get_rust_type(&["std".to_string()], "SparseId")
                                    .expect("SparseId is missing");
                                let contained_type = &generic_type_parameters[0];
                                let resolved_key = self.analyze_expression(
                                    index_expr,
                                    Some(&Type::RustType(sparse_id)),
                                )?;

                                let return_type =
                                    Type::Optional(Box::new(contained_type.clone()));

                                self.add_postfix(
                                    &mut suffixes,
                                    PostfixKind::RustTypeIndexRef(
                                        found_rust_type.clone(),
                                        resolved_key,
                                    ),
                                    return_type.clone(),
                                    &index_expr.node,
                                );

                                tv.resolved_type = return_type;
                                tv.is_mutable = false;

                                 */
                            } else {
                                panic!("unknown generic type lookup")
                            }
                        }

                        _ => {
                            return Err(self.create_err(
                                ErrorKind::ExpectedArray(collection_type),
                                &index_expr.node,
                            ));
                        }
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

                    let resolved_default_expr =
                        self.analyze_expression(default_expr, Some(unwrapped_type))?;
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

    fn analyze_bool_expression(
        &mut self,
        expression: &swamp_script_ast::Expression,
    ) -> Result<BooleanExpression, Error> {
        let resolved_expression = self.analyze_expression(expression, Some(&Type::Bool))?;
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
        let resolved_expression: MutOrImmutableExpression = if force_mut.is_some() {
            let resolved_node = self.to_node(&force_mut.unwrap());
            MutOrImmutableExpression {
                expression_or_location: ArgumentExpressionOrLocation::Location(
                    self.analyze_to_location(&expression.expression, None, &LocationSide::Rhs)?,
                ),
                is_mutable: Some(resolved_node),
            }
        } else {
            self.analyze_mut_or_immutable_expression(expression, None, &LocationSide::Rhs)?
        };

        let resolved_type = &resolved_expression.ty().clone();
        let (key_type, value_type): (Option<Type>, Type) = match resolved_type {
            Type::Range => (None, Type::Int),
            Type::String => (Some(Type::Int), Type::String),
            //Type::Iterator(item_type) => (None, *item_type.clone()),
            Type::Struct(resolved_struct_ref) => {
                let associated_function_info = self
                    .lookup_associated_function(resolved_type, "iter")
                    .expect("todo: missing iter() associated function");
                match &*associated_function_info.signature().return_type {
                    Type::Tuple(tuple_ref) => {
                        if tuple_ref.0.len() == 2 {
                            (Some(tuple_ref.0[0].clone()), tuple_ref.0[1].clone())
                        } else {
                            panic!("wrong iter")
                        }
                    }
                    a => (None, a.clone()),
                }
            }
            Type::External(_rust_type_ref) => {
                /*
                // TODO: HACK: We assume it is a container that iterates over the type parameters
                // TODO: HACK: We assume that it is a sparse map
                // TODO: HACK: Remove hardcoded number
                let rust_type_ref_for_id = self
                    .shared
                    .lookup
                    .get_rust_type(&["std".to_string()], "SparseId")
                    .expect("SparseId was missing");
                let rust_id_type = Type::RustType(rust_type_ref_for_id);
                (Some(rust_id_type), params[0].clone())

                 */
                // TODO: FIX THIS
                (None, Type::Unit)
            }
            _ => return Err(self.create_err(ErrorKind::NotAnIterator, &expression.expression.node)),
        };

        Ok(Iterable {
            key_type,
            value_type,
            resolved_expression: Box::new(resolved_expression),
        })
    }

    fn analyze_expressions(
        &mut self,
        expected_type: Option<&Type>,
        ast_expressions: &[swamp_script_ast::Expression],
    ) -> Result<Vec<Expression>, Error> {
        let mut resolved_expressions = Vec::new();
        for expression in ast_expressions {
            resolved_expressions.push(self.analyze_expression(expression, expected_type)?);
        }
        Ok(resolved_expressions)
    }

    fn analyze_block(
        &mut self,
        node: &swamp_script_ast::Node,
        expected_type_for_last: Option<&Type>,
        ast_expressions: &[swamp_script_ast::Expression],
    ) -> Result<(Vec<Expression>, Type), Error> {
        if ast_expressions.is_empty() {
            return if matches!(expected_type_for_last, Some(Type::Unit)) {
                Ok((vec![], Type::Unit))
            } else {
                Err(self.create_err(ErrorKind::EmptyBlockWrongType, node))
            };
        }
        let mut resolved_expressions = Vec::new();

        // need special handling for the last expression
        let (last, all_but_last) = ast_expressions.split_last().unwrap();

        for expression in all_but_last {
            resolved_expressions.push(self.analyze_expression(expression, None)?);
        }
        let last_expr = self.analyze_expression(last, expected_type_for_last)?;
        let last_type = last_expr.ty.clone();
        resolved_expressions.push(last_expr);
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
                    let expr = self.analyze_expression(expression, None)?;
                    let resolved_format_specifier =
                        self.analyze_format_specifier(format_specifier.as_ref());
                    StringPart::Interpolation(expr, resolved_format_specifier)
                }
            };

            resolved_parts.push(resolved_string_part);
        }

        Ok(resolved_parts)
    }

    // The ast assumes it is something similar to a variable, but it can be a function reference as well.
    fn analyze_identifier_reference(
        &self,
        var_node: &swamp_script_ast::Node,
    ) -> Result<Expression, Error> {
        let text = self.get_text(var_node);
        if let Some(found_symbol) = self.shared.lookup_table.get_symbol(text) {
            let expr = match found_symbol {
                Symbol::FunctionDefinition(func) => match func {
                    FuncDef::External(found_external_function) => self.create_expr(
                        ExpressionKind::ExternalFunctionAccess(found_external_function.clone()),
                        Type::Function(found_external_function.signature.clone()),
                        var_node,
                    ),
                    FuncDef::Internal(found_internal_function) => self.create_expr(
                        ExpressionKind::InternalFunctionAccess(found_internal_function.clone()),
                        Type::Function(found_internal_function.signature.clone()),
                        var_node,
                    ),
                    FuncDef::Intrinsic(found_intrinsic_function) => self.create_expr(
                        ExpressionKind::IntrinsicFunctionAccess(found_intrinsic_function.clone()),
                        Type::Function(found_intrinsic_function.signature.clone()),
                        var_node,
                    ),
                },

                _ => {
                    return Err(self.create_err(ErrorKind::UnknownVariable, var_node));
                }
            };
            return Ok(expr);
        }

        self.try_find_variable(var_node).map_or_else(
            || Err(self.create_err(ErrorKind::UnknownVariable, var_node)),
            |variable_ref| {
                Ok(self.create_expr(
                    ExpressionKind::VariableAccess(variable_ref.clone()),
                    variable_ref.resolved_type.clone(),
                    var_node,
                ))
            },
        )
    }

    fn analyze_array_type_helper(
        &mut self,
        node: &swamp_script_ast::Node,
        items: &[swamp_script_ast::Expression],
        expected_type: Option<&Type>,
    ) -> Result<(VecTypeRef, Vec<Expression>), Error> {
        let expressions = self.analyze_expressions(None, items)?;
        let item_type = if expressions.is_empty() {
            if let Some(found_expected_type) = expected_type {
                info!(?found_expected_type, "found array type");
                /*
                if let Type::Array(found) = found_expected_type {
                    found.item_type.clone()
                } else {
                    return Err(self.create_err(ErrorKind::NotAnArray, node));
                }

                 */
                Type::Unit
            } else {
                return Err(self.create_err(ErrorKind::NotAnArray, node));
            }
        } else {
            expressions[0].ty.clone()
        };

        let array_type = VecType { item_type };

        let array_type_ref = Rc::new(array_type);

        Ok((array_type_ref, expressions))
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
    ) -> Result<EnumVariantTypeRef, Error> {
        let variant_name_string = self.get_text(&variant_name.0).to_string();
        self.get_enum_variant_type(qualified_type_identifier, &variant_name_string)
    }

    #[allow(unused)]
    fn analyze_enum_variant_literal(
        &mut self,
        ast_variant: &swamp_script_ast::EnumVariantLiteral,
    ) -> Result<Literal, Error> {
        let (qualified_name, variant_name) = match ast_variant {
            swamp_script_ast::EnumVariantLiteral::Simple(name, variant) => (name, variant),
            swamp_script_ast::EnumVariantLiteral::Tuple(name, variant, _) => (name, variant),
            swamp_script_ast::EnumVariantLiteral::Struct(name, variant, _) => (name, variant),
        };

        let variant_ref = self.analyze_enum_variant_ref(qualified_name, variant_name)?;

        let resolved_data = match ast_variant {
            swamp_script_ast::EnumVariantLiteral::Simple(_qualified_name, _variant_name) => {
                EnumLiteralData::Nothing
            }
            swamp_script_ast::EnumVariantLiteral::Tuple(
                _qualified_name,
                _variant_name,
                expressions,
            ) => EnumLiteralData::Tuple(self.analyze_expressions(None, expressions)?),
            swamp_script_ast::EnumVariantLiteral::Struct(
                _qualified_name,
                variant_name,
                field_expressions,
            ) => {
                if let EnumVariantType::Struct(struct_ref) = &*variant_ref {
                    let resolved = self.analyze_anon_struct_instantiation(
                        &variant_name.0.clone(),
                        &struct_ref.anon_struct,
                        field_expressions,
                        false,
                    )?;
                    EnumLiteralData::Struct(resolved)
                } else {
                    return Err(self.create_err(
                        ErrorKind::WrongEnumVariantContainer(variant_ref.clone()),
                        &variant_name.0,
                    ));
                }
            }
        };

        Ok(Literal::EnumVariantLiteral(variant_ref, resolved_data))
    }

    fn analyze_match(
        &mut self,
        condition_expression: &swamp_script_ast::Expression,
        expected_type: Option<&Type>,
        arms: &Vec<swamp_script_ast::MatchArm>,
    ) -> Result<(Match, Type), Error> {
        let resolved_expression = self.analyze_expression(condition_expression, None)?;
        let resolved_type = resolved_expression.ty.clone();

        let mut resolved_arms = Vec::new();

        let mut detected_expected_type: Option<Type> = expected_type.cloned();

        for arm in arms {
            let resolved_arm = self.analyze_arm(
                arm,
                &resolved_expression,
                detected_expected_type.as_ref(),
                &resolved_type,
            )?;
            detected_expected_type = Some(resolved_arm.expression.ty.clone());
            resolved_arms.push(resolved_arm);
        }

        Ok((
            Match {
                expression: Box::new(resolved_expression),
                arms: resolved_arms,
            },
            detected_expected_type.expect("match arm failed"),
        ))
    }

    fn analyze_arm(
        &mut self,
        arm: &swamp_script_ast::MatchArm,
        _expression: &Expression,
        expected_return_type: Option<&Type>,
        expected_condition_type: &Type,
    ) -> Result<MatchArm, Error> {
        let (resolved_pattern, scope_was_pushed) =
            self.analyze_pattern(&arm.pattern, expected_condition_type)?;

        let resolved_expression = self.analyze_expression(&arm.expression, expected_return_type)?;
        if scope_was_pushed {
            self.pop_block_scope("analyze_arm");
        }

        let resolved_type = resolved_expression.ty.clone();

        Ok(MatchArm {
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

    fn analyze_pattern_literal(
        &mut self,
        node: &swamp_script_ast::Node,
        ast_literal: &swamp_script_ast::LiteralKind,
        expected_condition_type: &Type,
    ) -> Result<NormalPattern, Error> {
        let (resolved_literal, literal_type) =
            self.analyze_literal(node, ast_literal, Some(expected_condition_type))?;

        if !literal_type.same_type(expected_condition_type) {
            return Err(self.create_err(
                ErrorKind::IncompatibleTypes(literal_type, expected_condition_type.clone()),
                node,
            ));
        }

        Ok(NormalPattern::Literal(resolved_literal))
    }

    const fn to_node(&self, node: &swamp_script_ast::Node) -> Node {
        Node {
            span: Span {
                file_id: self.shared.file_id,
                offset: node.span.offset,
                length: node.span.length,
            },
            markdown_doc: None,
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
    ) -> Result<EnumVariantTypeRef, Error> {
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
        expected_type: Option<&Type>,
        variables: &[swamp_script_ast::VariableBinding],
        expression: &swamp_script_ast::Expression,
    ) -> Result<Expression, Error> {
        let mut variable_expressions = Vec::new();

        for variable in variables {
            let var = self.analyze_mut_or_immutable_expression(
                &variable.expression,
                None,
                &LocationSide::Rhs,
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

        let resolved_expression = self.analyze_expression(expression, expected_type)?;
        let block_type = resolved_expression.ty.clone();
        expressions.push(resolved_expression);

        let block_expression_kind = ExpressionKind::Block(expressions);
        self.pop_closed_block_scope();

        let block_expr = self.create_expr(block_expression_kind, block_type, &expression.node);
        Ok(block_expr)
    }

    fn analyze_when_expr(
        &mut self,
        expected_type: Option<&Type>,
        variables: &[swamp_script_ast::WhenBinding],
        true_expr: &swamp_script_ast::Expression,
        else_expr: Option<&swamp_script_ast::Expression>,
    ) -> Result<Expression, Error> {
        self.push_block_scope("when");
        let mut bindings = Vec::new();
        for variable_binding in variables {
            let mut_expr = if let Some(found_expr) = &variable_binding.expression {
                self.analyze_mut_or_immutable_expression(found_expr, None, &LocationSide::Rhs)?
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

        let resolved_true = self.analyze_expression(true_expr, expected_type)?;
        let block_type = resolved_true.ty.clone();

        self.pop_block_scope("when");

        let maybe_resolved_else = if let Some(found_else) = else_expr {
            Some(Box::new(
                self.analyze_expression(found_else, Some(&block_type))?,
            ))
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
        expected_type: Option<&Type>,
        guard_expressions: &Vec<swamp_script_ast::GuardExpr>,
    ) -> Result<Expression, Error> {
        let mut expecting_type = expected_type.cloned();
        let mut guards = Vec::new();
        let mut found_wildcard = None;

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
                    Some(self.analyze_bool_expression(clause_expr)?)
                }
            };

            let resolved_result =
                self.analyze_expression(&guard.result, expecting_type.as_ref())?;
            let ty = resolved_result.ty.clone();
            if expecting_type.is_none() {
                expecting_type = Some(ty);
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

        expecting_type.map_or_else(
            || Err(self.create_err(ErrorKind::GuardHasNoType, node)),
            |found_expecting_type| {
                let expr = self.create_expr(kind, found_expecting_type, node);
                Ok(expr)
            },
        )
    }

    pub fn analyze_variable_assignment(
        &mut self,
        variable: &swamp_script_ast::Variable,
        source_expression: &swamp_script_ast::MutableOrImmutableExpression,
    ) -> Result<Expression, Error> {
        let source_expr =
            self.analyze_mut_or_immutable_expression(source_expression, None, &LocationSide::Rhs)?;
        let ty = source_expr.ty().clone();

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
        coerce_type: Option<&swamp_script_ast::Type>,
        source_expression: &swamp_script_ast::MutableOrImmutableExpression,
    ) -> Result<Expression, Error> {
        let ty = if let Some(found_ast_type) = coerce_type {
            Some(self.analyze_type(found_ast_type)?)
        } else {
            None
        };

        let resolved_source = self.analyze_mut_or_immutable_expression(
            source_expression,
            ty.as_ref(),
            &LocationSide::Rhs,
        )?;

        let var_ref = self.create_local_variable(
            &var.name,
            Option::from(&var.is_mutable),
            resolved_source.ty(),
        )?;

        let resolved_type = resolved_source.ty().clone();
        assert!(
            !matches!(resolved_type, Type::Unit),
            "Assertion failed: resolved_type should NOT be Type::Unit, but it was."
        );

        let kind = ExpressionKind::VariableDefinition(var_ref, Box::from(resolved_source));

        let resolved_expr = self.create_expr(kind, resolved_type, &var.name);

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
            node: resolved_node,
            ty,
            kind,
        };

        vec.push(postfix);
    }

    #[allow(clippy::too_many_lines)]
    fn analyze_chain_to_location(
        &mut self,
        chain: &swamp_script_ast::PostfixChain,
        expected_type: Option<Type>,
        _location_side: &LocationSide,
    ) -> Result<SingleLocationExpression, Error> {
        let mut items = Vec::new();

        let base_expr = self.analyze_expression(&chain.base, None)?;
        let ExpressionKind::VariableAccess(start_variable) = base_expr.kind else {
            return Err(self.create_err(ErrorKind::NotValidLocationStartingPoint, &chain.base.node));
        };

        let mut ty = start_variable.resolved_type.clone();
        for item in &chain.postfixes {
            match &item {
                swamp_script_ast::Postfix::FieldAccess(field_name_node) => {
                    //let field_name_resolved = self.to_node(field_name_node)
                    let (struct_type_ref, index, return_type) =
                        self.analyze_struct_field(field_name_node, &ty)?;
                    self.add_location_item(
                        &mut items,
                        LocationAccessKind::FieldIndex(struct_type_ref.clone(), index),
                        return_type.clone(),
                        field_name_node,
                    );

                    ty = return_type.clone();
                }
                swamp_script_ast::Postfix::Subscript(lookup_expr) => {
                    let is_range = if let swamp_script_ast::ExpressionKind::Range(min, max, mode) =
                        &lookup_expr.kind
                    {
                        Some(self.analyze_range(min, max, mode)?)
                    } else {
                        None
                    };
                    match &ty {
                        Type::String => {
                            if let Some(range) = is_range {
                                self.add_location_item(
                                    &mut items,
                                    LocationAccessKind::StringRange(range),
                                    Type::String,
                                    &lookup_expr.node,
                                );
                                ty = Type::String;
                            } else {
                                let index_expr =
                                    self.analyze_expression(lookup_expr, Some(&Type::Int))?; // TODO: Support slice (range)
                                self.add_location_item(
                                    &mut items,
                                    LocationAccessKind::StringIndex(index_expr),
                                    Type::String,
                                    &lookup_expr.node,
                                );
                                ty = Type::String;
                            }
                        }
                        /*

                        Type::Array(array_type) => {
                            let index_expr =
                                self.analyze_expression(lookup_expr, Some(&Type::Int))?; // TODO: Support slice (range)
                            self.add_location_item(
                                &mut items,
                                LocationAccessKind::ArrayIndex(array_type.clone(), index_expr),
                                array_type.item_type.clone(),
                                &lookup_expr.node,
                            );
                            ty = array_type.item_type.clone();
                        }

                         */

                        /*
                        Type::Map(map_type) => {
                            let key_expr = self.analyze_expression(
                                lookup_expr,
                                Some(&map_type.key_type.clone()),
                            )?;
                            let is_last = i == chain.postfixes.len() - 1;
                            let allow_auto_insert = is_last && *location_side == LocationSide::Lhs;
                            let (kind, lookup_type) = if allow_auto_insert {
                                // If this is the last postfix in the chain, then it is a "bare" access and auto-insert is allowed
                                // the type is `value_type` since this lookup is safe. we can create a memory location if there wasn't one
                                (
                                    LocationAccessKind::MapIndexInsertIfNonExisting(
                                        map_type.clone(),
                                        key_expr,
                                    ),
                                    map_type.value_type.clone(),
                                )
                            } else {
                                let optional_value_type =
                                    Type::Optional(Box::from(map_type.value_type.clone()));
                                (
                                    LocationAccessKind::MapIndex(map_type.clone(), key_expr),
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

                         */
                        Type::Struct(resolved_struct_ref) => {
                            let return_type = {
                                let subscript_fn = self
                                    .shared
                                    .state
                                    .associated_impls
                                    .get_member_function(
                                        &Type::Struct(resolved_struct_ref.clone()),
                                        "subscript_mut",
                                    )
                                    .unwrap();
                                &subscript_fn.signature().parameters[2].resolved_type.clone()
                            };
                            ty = return_type.clone();
                            //tv.resolved_type = *lookup_returns.clone();
                            //tv.is_mutable = false;
                        }

                        Type::External(_external_type) => {
                            let _val_type = Type::Unit; // TODO: generic_params[0].clone();

                            /*
                            if rust_type.number == SPARSE_TYPE_ID {
                                let sparse_id_type = self
                                    .shared
                                    .lookup_table
                                    .get_rust_type(&["std".to_string()], "SparseId")
                                    .expect("should have SparseId");

                                let key_type = Type::External(sparse_id_type);

                                let key_expr =
                                    self.analyze_expression(lookup_expr, Some(&key_type.clone()))?;

                                self.add_location_item(
                                    &mut items,
                                    LocationAccessKind::RustTypeIndex(rust_type.clone(), key_expr),
                                    key_type.clone(),
                                    &lookup_expr.node,
                                );

                                ty = Type::Optional(Box::from(val_type.clone()));
                            }

                             */
                        }

                        _ => {
                            return Err(
                                self.create_err(ErrorKind::IllegalIndexInChain, &lookup_expr.node)
                            );
                        }
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

        if let Some(found_expected_type) = expected_type {
            if !ty.same_type(&found_expected_type) {
                return Err(self.create_err(
                    ErrorKind::IncompatibleTypes(ty, found_expected_type),
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
        expected_type: Option<Type>,
        location_type: &LocationSide,
    ) -> Result<SingleLocationExpression, Error> {
        //let resolved_expr = self.analyze_expression(expr, Some(&expected_type))?;
        match &expr.kind {
            swamp_script_ast::ExpressionKind::PostfixChain(chain) => {
                self.analyze_chain_to_location(chain, expected_type, location_type)
            }
            swamp_script_ast::ExpressionKind::IdentifierReference(variable) => {
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
            _ => Err(self.create_err(ErrorKind::NotValidLocationStartingPoint, &expr.node)),
        }
    }

    #[allow(clippy::single_match)]
    fn check_special_assignment_compound(
        &mut self,
        _target_expression: &swamp_script_ast::Expression,
        target_type: &Type,
        _op: &CompoundOperatorKind,
        _source: &swamp_script_ast::Expression,
        _source_type: &Type,
    ) -> Result<Option<ExpressionKind>, Error> {
        match &target_type {
            /*
            Type::Array(array_type) => {
                if *op == CompoundOperatorKind::Add && source_type.same_type(&array_type.item_type)
                {
                    // Handle ArrayPush
                    let target_location = SingleMutLocationExpression(self.analyze_to_location(
                        target_expression,
                        Some(target_type.clone()),
                        &LocationSide::Rhs,
                    )?);
                    let resolved_source = self.analyze_expression(source, Some(source_type))?;
                    return Ok(Option::from(ExpressionKind::ArrayPush(
                        target_location,
                        Box::from(resolved_source),
                    )));
                } else if *op == CompoundOperatorKind::Add && source_type.same_type(target_type) {
                    // Handle ArrayExtend
                    let target_location = SingleMutLocationExpression(self.analyze_to_location(
                        target_expression,
                        Some(target_type.clone()),
                        &LocationSide::Rhs,
                    )?);
                    let resolved_source = self.analyze_expression(source, Some(source_type))?;
                    return Ok(Option::from(ExpressionKind::ArrayExtend(
                        target_location,
                        Box::from(resolved_source),
                    )));
                }
            }

             */
            _ => {}
        }

        Ok(None)
    }

    fn analyze_assignment_compound(
        &mut self,
        target_expression: &swamp_script_ast::Expression,
        ast_op: &swamp_script_ast::CompoundOperator,
        ast_source_expression: &swamp_script_ast::Expression,
    ) -> Result<Expression, Error> {
        let resolved_op = self.analyze_compound_operator(ast_op);
        let source_expr = self.analyze_expression(ast_source_expression, None)?;

        let resolved_location = SingleMutLocationExpression(self.analyze_to_location(
            target_expression,
            Some(source_expr.ty.clone()),
            &LocationSide::Rhs,
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
            ExpressionKind::CompoundAssignment(
                resolved_location,
                resolved_op.kind,
                Box::from(source_expr),
            )
        };

        let expr = self.create_expr(kind, Type::Unit, &target_expression.node);

        Ok(expr)
    }

    fn analyze_assignment(
        &mut self,
        target_location: &swamp_script_ast::Expression,
        ast_source_expression: &swamp_script_ast::Expression,
    ) -> Result<Expression, Error> {
        let resolved_location =
            self.analyze_to_location(target_location, None, &LocationSide::Lhs)?;

        let ty = resolved_location.ty.clone();
        assert!(
            !matches!(ty, Type::Unit),
            "Assertion failed: resolved_type should NOT be Type::Unit, but it was."
        );

        let source_expr = self.analyze_expression(ast_source_expression, Some(&ty))?;

        let mut_location = SingleMutLocationExpression(resolved_location);

        let kind = ExpressionKind::Assignment(Box::from(mut_location), Box::from(source_expr));

        let expr = self.create_expr(kind, Type::Unit, &target_location.node); // Assignments are always of type Unit

        Ok(expr)
    }

    #[must_use]
    pub fn create_expr(
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
        let tuple_resolved = self.analyze_expression(tuple_expression, None)?;
        let tuple_expr_type = &tuple_resolved.ty;

        let mut variable_refs = Vec::new();
        if let Type::Tuple(tuple) = tuple_expr_type.clone() {
            if target_ast_variables.len() > tuple.0.len() {
                return Err(self.create_err(ErrorKind::TooManyDestructureVariables, node));
            }
            for (variable_ref, tuple_type) in target_ast_variables.iter().zip(tuple.0.clone()) {
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
        struct_type: &StructTypeRef,
        is_mutable: bool,
        arguments: &[swamp_script_ast::MutableOrImmutableExpression],
    ) -> Result<Postfix, Error> {
        let signature = found_function.signature();

        let self_type = &signature.parameters[0];
        if !self_type
            .resolved_type
            .clone()
            .same_type(&Type::Struct(struct_type.clone()))
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
        struct_type: &StructTypeRef,
        field: &StructTypeField,
        index: usize,
        signature: &Signature,
        arguments: &[swamp_script_ast::MutableOrImmutableExpression],
    ) -> Result<Vec<Postfix>, Error> {
        let mut suffixes = Vec::new();
        //let field_name_str = self.get_text(member_name).to_string();
        let struct_field_kind = PostfixKind::StructField(struct_type.clone(), index);

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
        struct_type: &StructTypeRef,
        is_mutable: bool,
        member_name: &swamp_script_ast::Node,
        arguments: &[swamp_script_ast::MutableOrImmutableExpression],
        suffixes: &mut Vec<Postfix>,
    ) -> Result<Type, Error> {
        let field_name_str = self.get_text(member_name).to_string();

        let resolved_node = self.to_node(member_name);
        let binding = struct_type;
        let maybe = self
            .shared
            .state
            .associated_impls
            .get_member_function(&Type::Struct(struct_type.clone()), &field_name_str)
            .cloned();

        let postfixes = if let Some(found_function_member) = maybe {
            let postfix = self.analyze_postfix_member_func_call(
                &resolved_node,
                &found_function_member,
                struct_type,
                is_mutable,
                arguments,
            )?;
            vec![postfix]
        } else if let Some(found_field) =
            binding.anon_struct_type.defined_fields.get(&field_name_str)
        {
            if let Type::Function(signature) = &found_field.field_type {
                let index = binding
                    .anon_struct_type
                    .defined_fields
                    .get_index(&field_name_str)
                    .expect("should work");
                self.analyze_postfix_field_call(
                    &resolved_node,
                    struct_type,
                    found_field,
                    index,
                    signature,
                    arguments,
                )?
            } else {
                return Err(self.create_err(ErrorKind::NotAFunctionType, member_name));
            }
        } else {
            return Err(self.create_err(
                ErrorKind::ExpectedMemberFunctionOrFunctionTypeField,
                member_name,
            ));
        };

        let last_type = postfixes.last().unwrap().ty.clone();
        suffixes.extend(postfixes);

        Ok(last_type)
    }
}
