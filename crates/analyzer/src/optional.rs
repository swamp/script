/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::ResolveError;
use crate::Resolver;
use swamp_script_ast::{Expression, Variable};
use swamp_script_semantic::{ResolvedExpression, ResolvedType};

impl<'a> Resolver<'a> {
    pub(crate) fn handle_optional_unwrap_statement(
        &mut self,
        var: &Variable,
        expr: &Expression,
        true_expression: &Expression,
        maybe_else_expression: &Option<Box<Expression>>,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_var_expr = self.resolve_expression(expr)?;

        if let ResolvedType::Optional(inner_type) = resolved_var_expr.resolution() {
            self.push_block_scope("if_unwrap");
            let resolved_var_ref =
                self.create_local_variable(&var.name, &var.is_mutable, &inner_type)?;
            let resolved_true = self.resolve_expression(true_expression)?;
            self.pop_block_scope("if_unwrap");

            let resolved_false = if let Some(else_expression) = maybe_else_expression {
                Some(Box::new(self.resolve_expression(else_expression)?))
            } else {
                None
            };

            Ok(ResolvedExpression::IfOnlyVariable {
                variable: resolved_var_ref,
                optional_expr: Box::new(resolved_var_expr),
                true_block: Box::from(resolved_true),
                false_block: resolved_false,
            })
        } else {
            Err(ResolveError::ExpectedOptional)
        }
    }

    pub(crate) fn handle_optional_assign_unwrap_statement(
        &mut self,
        var: &Variable,
        inner_expr: &Expression,
        statements: &Expression,
        maybe_else_statements: &Option<Box<Expression>>,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_expr = self.resolve_expression(inner_expr)?;

        if let ResolvedType::Optional(inner_type) = resolved_expr.resolution() {
            self.push_block_scope("if_assign_unwrap");
            let resolved_var_ref =
                self.create_local_variable(&var.name, &var.is_mutable, &inner_type)?;
            let resolved_true = self.resolve_expression(statements)?;
            self.pop_block_scope("if_assign_unwrap");

            let resolved_false = if let Some(else_statements) = maybe_else_statements {
                Some(Box::new(self.resolve_expression(else_statements)?))
            } else {
                None
            };

            Ok(ResolvedExpression::IfAssignExpression {
                variable: resolved_var_ref,
                optional_expr: Box::new(resolved_expr),
                true_block: Box::from(resolved_true),
                false_block: resolved_false,
            })
        } else {
            Err(ResolveError::ExpectedOptional)
        }
    }
}
