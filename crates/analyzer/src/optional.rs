/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::ResolveError;
use crate::Resolver;
use swamp_script_ast::{Expression, Node, Variable};
use swamp_script_semantic::{ResolvedExpression, ResolvedType, Spanned};

impl<'a> Resolver<'a> {
    pub(crate) fn handle_optional_unwrap_statement(
        &mut self,
        source_and_implicit_target_variable: &Variable,
        expr: &Expression,
        true_expression: &Expression,
        maybe_else_expression: &Option<Box<Expression>>,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_var_expr = self.resolve_expression(expr, &ResolvedType::Any)?;
        let resolved_var_type = resolved_var_expr.resolution();
        if resolved_var_type == ResolvedType::Any {
            println!("problem");
        }
        let found_var = self.find_variable(source_and_implicit_target_variable)?;

        if let ResolvedType::Optional(inner_type) = resolved_var_type {
            self.push_block_scope("if_unwrap");
            let mut_node = if found_var.is_mutable() {
                Some(Node::default())
            } else {
                None
            };
            let resolved_var_ref = self.create_local_variable(
                &source_and_implicit_target_variable.name,
                &mut_node,
                &inner_type,
            )?; // it inherits the mutable from the other variable
            let resolved_true = self.resolve_expression(true_expression, &ResolvedType::Any)?;
            self.pop_block_scope("if_unwrap");

            let resolved_false = if let Some(else_expression) = maybe_else_expression {
                Some(Box::new(
                    self.resolve_expression(else_expression, &ResolvedType::Any)?,
                ))
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
            Err(ResolveError::ExpectedOptional(resolved_var_expr.span()))
        }
    }

    pub(crate) fn handle_optional_assign_unwrap_statement(
        &mut self,
        target_variable: &Variable,
        inner_expr: &Expression,
        statements: &Expression,
        maybe_else_statements: &Option<Box<Expression>>,
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_expr = self.resolve_expression(inner_expr, &ResolvedType::Any)?;

        if let ResolvedType::Optional(inner_type) = resolved_expr.resolution() {
            /*
            let debug_var_name = self.get_text(&target_variable.name);
            let found_var = self.find_variable(target_variable)?;
            let mut_node = if found_var.is_mutable() {
                Some(Node::default())
            } else {
                None
            };
            */
            self.push_block_scope("if_assign_unwrap");

            let resolved_var_ref = self.create_local_variable(
                &target_variable.name,
                &target_variable.is_mutable,
                &inner_type,
            )?;
            let resolved_true = self.resolve_expression(statements, &ResolvedType::Any)?;
            self.pop_block_scope("if_assign_unwrap");

            let resolved_false = if let Some(else_statements) = maybe_else_statements {
                Some(Box::new(
                    self.resolve_expression(else_statements, &ResolvedType::Any)?,
                ))
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
            Err(ResolveError::ExpectedOptional(resolved_expr.span()))
        }
    }
}
