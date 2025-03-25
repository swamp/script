/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use crate::TypeContext;
use crate::err::{Error, ErrorKind};
use swamp_script_semantic::{Expression, ExpressionKind, Function, FunctionRef, Range, RangeMode};
use swamp_script_types::prelude::*;

impl Analyzer<'_> {
    #[must_use]
    pub fn convert_to_function_access_kind(function_ref: &FunctionRef) -> ExpressionKind {
        match &**function_ref {
            Function::Internal(internal_function) => {
                ExpressionKind::InternalFunctionAccess(internal_function.clone())
            }
            Function::External(external_function) => {
                ExpressionKind::ExternalFunctionAccess(external_function.clone())
            }
        }
    }

    #[must_use]
    pub fn lookup_associated_function(
        &self,
        ty: &Type,
        function_name: &str,
    ) -> Option<FunctionRef> {
        self.shared
            .state
            .instantiator
            .associated_impls
            .get_member_function(ty, function_name)
            .cloned()
    }

    #[must_use]
    pub fn convert_to_function_access_expr(
        &self,
        associated_function_info: &FunctionRef,
        ast_node: &swamp_script_ast::Node,
    ) -> Expression {
        let kind = Self::convert_to_function_access_kind(associated_function_info);
        self.create_expr(
            kind,
            Type::Function(associated_function_info.signature().clone()),
            ast_node,
        )
    }

    pub(crate) fn analyze_static_member_access(
        &mut self,
        named_type: &swamp_script_ast::QualifiedTypeIdentifier,
        member_name_node: &swamp_script_ast::Node,
    ) -> Result<Expression, Error> {
        let some_type = self.analyze_named_type(named_type)?;
        let member_name = self.get_text(member_name_node);
        self.lookup_associated_function(&some_type, member_name)
            .map_or_else(
                || Err(self.create_err(ErrorKind::UnknownMemberFunction, member_name_node)),
                |member_function| {
                    let expr =
                        self.convert_to_function_access_expr(&member_function, member_name_node);
                    Ok(expr)
                },
            )
    }

    pub(crate) fn analyze_min_max_expr(
        &mut self,
        min_expr: &swamp_script_ast::Expression,
        max_expr: &swamp_script_ast::Expression,
    ) -> Result<(Expression, Expression), Error> {
        let context = TypeContext::new_argument(&Type::Int);

        let resolved_min = self.analyze_expression(min_expr, &context)?;
        let resolved_max = self.analyze_expression(max_expr, &context)?;

        Ok((resolved_min, resolved_max))
    }

    /// # Errors
    ///
    pub fn analyze_range(
        &mut self,
        min_expr: &swamp_script_ast::Expression,
        max_expr: &swamp_script_ast::Expression,
        mode: &swamp_script_ast::RangeMode,
    ) -> Result<Range, Error> {
        let (min, max) = self.analyze_min_max_expr(min_expr, max_expr)?;

        let resolved_range_mode = match mode {
            swamp_script_ast::RangeMode::Inclusive => RangeMode::Inclusive,
            swamp_script_ast::RangeMode::Exclusive => RangeMode::Exclusive,
        };
        Ok(Range {
            min,
            max,
            mode: resolved_range_mode,
        })
    }
}
