/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::{ResolveError, ResolveErrorKind};
use crate::Resolver;
use swamp_script_ast::{BinaryOperator, BinaryOperatorKind, Expression, UnaryOperator};
use swamp_script_semantic::{
    ResolvedBinaryOperator, ResolvedBinaryOperatorKind, ResolvedType, ResolvedUnaryOperator,
    ResolvedUnaryOperatorKind,
};
use tracing::debug;

impl<'a> Resolver<'a> {
    pub(crate) fn resolve_binary_op(
        &mut self,
        ast_left: &Expression,
        ast_op: &BinaryOperator,
        ast_right: &Expression,
    ) -> Result<(ResolvedBinaryOperator, ResolvedType), ResolveError> {
        let left = self.resolve_expression(ast_left, None)?;
        let left_type = left.ty.clone();

        let right = self.resolve_expression(ast_right, None)?;
        let right_type = right.ty.clone();

        let kind = self.convert_binary_operator_kind(ast_op);
        let node = self.to_node(&ast_op.node);

        match (&kind, &left_type, &right_type) {
            // String concatenation - allow any type on the right
            (&ResolvedBinaryOperatorKind::Add, ResolvedType::String, _) => Ok((
                ResolvedBinaryOperator {
                    left: Box::new(left),
                    right: Box::new(right),
                    kind,
                    node,
                },
                ResolvedType::String,
            )),

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
                    return Err(self.create_err(
                        ResolveErrorKind::IncompatibleTypes(left_type, right_type),
                        &ast_op.node,
                    ));
                }
                Ok((
                    ResolvedBinaryOperator {
                        left: Box::new(left),
                        right: Box::new(right),
                        kind,
                        node,
                    },
                    ResolvedType::Bool,
                ))
            }

            // All other operators require exact type matches
            _ => {
                if !left_type.same_type(&right_type) {
                    debug!(?left_type, ?right_type, "type mismatch in operation");
                    return Err(self.create_err_resolved(
                        ResolveErrorKind::IncompatibleTypes(left_type, right_type),
                        &node,
                    ));
                }
                Ok((
                    ResolvedBinaryOperator {
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

    pub(crate) fn resolve_unary_op(
        &mut self,
        ast_op: &UnaryOperator,
        ast_left: &Expression,
    ) -> Result<(ResolvedUnaryOperator, ResolvedType), ResolveError> {
        let (node, kind, require_type) = match ast_op {
            UnaryOperator::Not(node) => (
                node,
                ResolvedUnaryOperatorKind::Not,
                Some(&ResolvedType::Bool),
            ),
            UnaryOperator::Negate(node) => (node, ResolvedUnaryOperatorKind::Negate, None),
        };
        let left = self.resolve_expression(ast_left, require_type)?;
        let resolved_type = left.ty.clone();
        Ok((
            ResolvedUnaryOperator {
                left: Box::new(left),
                kind,
                node: self.to_node(node),
            },
            resolved_type,
        ))
    }

    const fn convert_binary_operator_kind(
        &self,
        binary_operator: &BinaryOperator,
    ) -> ResolvedBinaryOperatorKind {
        match binary_operator.kind {
            BinaryOperatorKind::Add => ResolvedBinaryOperatorKind::Add,
            BinaryOperatorKind::Subtract => ResolvedBinaryOperatorKind::Subtract,
            BinaryOperatorKind::Multiply => ResolvedBinaryOperatorKind::Multiply,
            BinaryOperatorKind::Divide => ResolvedBinaryOperatorKind::Divide,
            BinaryOperatorKind::Modulo => ResolvedBinaryOperatorKind::Modulo,
            BinaryOperatorKind::LogicalOr => ResolvedBinaryOperatorKind::LogicalOr,
            BinaryOperatorKind::LogicalAnd => ResolvedBinaryOperatorKind::LogicalAnd,
            BinaryOperatorKind::Equal => ResolvedBinaryOperatorKind::Equal,
            BinaryOperatorKind::NotEqual => ResolvedBinaryOperatorKind::NotEqual,
            BinaryOperatorKind::LessThan => ResolvedBinaryOperatorKind::LessThan,
            BinaryOperatorKind::LessEqual => ResolvedBinaryOperatorKind::LessEqual,
            BinaryOperatorKind::GreaterThan => ResolvedBinaryOperatorKind::GreaterThan,
            BinaryOperatorKind::GreaterEqual => ResolvedBinaryOperatorKind::GreaterEqual,
            BinaryOperatorKind::RangeExclusive => ResolvedBinaryOperatorKind::RangeExclusive,
        }
    }
}
