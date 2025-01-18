/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::ResolveError;
use crate::Resolver;
use swamp_script_ast::{BinaryOperator, Expression, PostfixOperator, UnaryOperator};
use swamp_script_semantic::{
    ResolvedBinaryOperator, ResolvedBinaryOperatorKind, ResolvedNode, ResolvedPostfixOperator,
    ResolvedPostfixOperatorKind, ResolvedType, ResolvedUnaryOperator, ResolvedUnaryOperatorKind,
    Spanned,
};
use tracing::debug;

impl<'a> Resolver<'a> {
    pub(crate) fn resolve_binary_op(
        &mut self,
        ast_left: &Expression,
        ast_op: &BinaryOperator,
        ast_right: &Expression,
    ) -> Result<ResolvedBinaryOperator, ResolveError> {
        let left = self.resolve_expression(ast_left, &ResolvedType::Any)?;
        let left_type = left.resolution();

        let right = self.resolve_expression(ast_right, &ResolvedType::Any)?;
        let right_type = right.resolution();

        let (resolved_node, kind) = self.convert_binary_operator_kind(ast_op);

        match (&kind, &left_type, &right_type) {
            // String concatenation - allow any type on the right
            (&ResolvedBinaryOperatorKind::Add, ResolvedType::String, _) => {
                Ok(ResolvedBinaryOperator {
                    left: Box::new(left),
                    right: Box::new(right),
                    kind,
                    node: resolved_node,
                    resolved_type: ResolvedType::String,
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
                    resolved_type: ResolvedType::Bool,
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

    pub(crate) fn resolve_unary_op(
        &mut self,
        ast_op: &UnaryOperator,
        ast_left: &Expression,
    ) -> Result<ResolvedUnaryOperator, ResolveError> {
        let left = self.resolve_expression(ast_left, &ResolvedType::Any)?;
        let resolved_type = left.resolution();

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

    pub(crate) fn resolve_postfix_op(
        &mut self,
        ast_op: &PostfixOperator,
        ast_left: &Expression,
    ) -> Result<ResolvedPostfixOperator, ResolveError> {
        let left = self.resolve_expression(ast_left, &ResolvedType::Any)?;
        let resolved_type = left.resolution();

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
}
