/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::{Error, ErrorKind};
use crate::{Analyzer, TypeContext};
use swamp_script_semantic::{
    BinaryOperator, BinaryOperatorKind, Type, UnaryOperator, UnaryOperatorKind,
};
use tracing::debug;

impl<'a> Analyzer<'a> {
    pub(crate) fn analyze_binary_op(
        &mut self,
        ast_left: &swamp_script_ast::Expression,
        ast_op: &swamp_script_ast::BinaryOperator,
        ast_right: &swamp_script_ast::Expression,
    ) -> Result<(BinaryOperator, Type), Error> {
        let anything_context = TypeContext::new_anything_argument();
        let left = self.analyze_expression(ast_left, &anything_context)?;
        let left_type = left.ty.clone();

        let right = self.analyze_expression(ast_right, &anything_context)?;
        let right_type = right.ty.clone();

        let kind = self.convert_binary_operator_kind(ast_op);
        let node = self.to_node(&ast_op.node);

        match (&kind, &left_type, &right_type) {
            // String concatenation - allow any type on the right
            (&BinaryOperatorKind::Add, Type::String, _) => Ok((
                BinaryOperator {
                    left: Box::new(left),
                    right: Box::new(right),
                    kind,
                    node,
                },
                Type::String,
            )),

            // Comparison operators
            (
                BinaryOperatorKind::Equal
                | BinaryOperatorKind::NotEqual
                | BinaryOperatorKind::GreaterThan
                | BinaryOperatorKind::GreaterEqual
                | BinaryOperatorKind::LessThan
                | BinaryOperatorKind::LessEqual,
                _,
                _,
            ) => {
                if !left_type.compatible_with(&right_type) {
                    debug!(?left_type, ?right_type, "type mismatch in comparison");
                    return Err(self.create_err(
                        ErrorKind::IncompatibleTypes(left_type, right_type),
                        &ast_op.node,
                    ));
                }
                Ok((
                    BinaryOperator {
                        left: Box::new(left),
                        right: Box::new(right),
                        kind,
                        node,
                    },
                    Type::Bool,
                ))
            }

            // All other operators require exact type matches
            _ => {
                if !left_type.compatible_with(&right_type) {
                    debug!(?left_type, ?right_type, "type mismatch in operation");
                    return Err(self.create_err_resolved(
                        ErrorKind::IncompatibleTypes(left_type, right_type),
                        &node,
                    ));
                }
                Ok((
                    BinaryOperator {
                        left: Box::new(left),
                        right: Box::new(right),
                        kind,
                        node,
                    },
                    left_type,
                ))
            }
        }
    }

    pub(crate) fn analyze_unary_op(
        &mut self,
        ast_op: &swamp_script_ast::UnaryOperator,
        ast_left: &swamp_script_ast::Expression,
    ) -> Result<(UnaryOperator, Type), Error> {
        let (node, kind, require_type) = match ast_op {
            swamp_script_ast::UnaryOperator::Not(node) => {
                (node, UnaryOperatorKind::Not, Some(&Type::Bool))
            }
            swamp_script_ast::UnaryOperator::Negate(node) => {
                (node, UnaryOperatorKind::Negate, None)
            }
        };
        let context = TypeContext::new_unsure_argument(require_type);
        let left = self.analyze_expression(ast_left, &context)?;
        let resolved_type = left.ty.clone();
        Ok((
            UnaryOperator {
                left: Box::new(left),
                kind,
                node: self.to_node(node),
            },
            resolved_type,
        ))
    }

    const fn convert_binary_operator_kind(
        &self,
        binary_operator: &swamp_script_ast::BinaryOperator,
    ) -> BinaryOperatorKind {
        match binary_operator.kind {
            swamp_script_ast::BinaryOperatorKind::Add => BinaryOperatorKind::Add,
            swamp_script_ast::BinaryOperatorKind::Subtract => BinaryOperatorKind::Subtract,
            swamp_script_ast::BinaryOperatorKind::Multiply => BinaryOperatorKind::Multiply,
            swamp_script_ast::BinaryOperatorKind::Divide => BinaryOperatorKind::Divide,
            swamp_script_ast::BinaryOperatorKind::Modulo => BinaryOperatorKind::Modulo,
            swamp_script_ast::BinaryOperatorKind::LogicalOr => BinaryOperatorKind::LogicalOr,
            swamp_script_ast::BinaryOperatorKind::LogicalAnd => BinaryOperatorKind::LogicalAnd,
            swamp_script_ast::BinaryOperatorKind::Equal => BinaryOperatorKind::Equal,
            swamp_script_ast::BinaryOperatorKind::NotEqual => BinaryOperatorKind::NotEqual,
            swamp_script_ast::BinaryOperatorKind::LessThan => BinaryOperatorKind::LessThan,
            swamp_script_ast::BinaryOperatorKind::LessEqual => BinaryOperatorKind::LessEqual,
            swamp_script_ast::BinaryOperatorKind::GreaterThan => BinaryOperatorKind::GreaterThan,
            swamp_script_ast::BinaryOperatorKind::GreaterEqual => BinaryOperatorKind::GreaterEqual,
            swamp_script_ast::BinaryOperatorKind::RangeExclusive => {
                BinaryOperatorKind::RangeExclusive
            }
        }
    }
}
