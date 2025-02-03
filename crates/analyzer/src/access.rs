/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::{ResolveError, ResolveErrorKind};
use crate::Resolver;
use swamp_script_ast::{Expression, Node, QualifiedTypeIdentifier, RangeMode};
use swamp_script_semantic::{
    ResolvedArrayTypeRef, ResolvedExpression, ResolvedExpressionKind, ResolvedFunction,
    ResolvedFunctionRef, ResolvedRange, ResolvedRangeMode, ResolvedType,
};

impl<'a> Resolver<'a> {
    fn convert_to_function_access(function: &ResolvedFunctionRef) -> ResolvedExpression {
        match &**function {
            ResolvedFunction::Internal(x) => ResolvedExpression {
                ty: ResolvedType::Function(x.signature.clone()),
                node: function.node(),
                kind: ResolvedExpressionKind::InternalFunctionAccess(x.clone()),
            },
            ResolvedFunction::External(y) => ResolvedExpression {
                ty: ResolvedType::Function(y.signature.clone()),
                node: function.node(),
                kind: ResolvedExpressionKind::ExternalFunctionAccess(y.clone()),
            },
        }
    }

    pub(crate) fn resolve_static_member_access(
        &self,
        struct_reference: &QualifiedTypeIdentifier,
        member_name_node: &Node,
    ) -> Result<ResolvedExpression, ResolveError> {
        let struct_type = self.get_struct_type(struct_reference)?;
        let member_name = self.get_text(member_name_node);
        let binding = struct_type.borrow();
        let member_function = binding
            .functions
            .get(&member_name.to_string())
            .ok_or_else(|| {
                self.create_err(ResolveErrorKind::UnknownMemberFunction, member_name_node)
            })?;

        let expr = Self::convert_to_function_access(member_function);

        Ok(expr)
    }

    pub(crate) fn resolve_min_max_expr(
        &mut self,
        min_expr: &Expression,
        max_expr: &Expression,
    ) -> Result<(ResolvedExpression, ResolvedExpression), ResolveError> {
        let resolved_min = self.resolve_expression(min_expr, Some(&ResolvedType::Int))?;
        let resolved_max = self.resolve_expression(max_expr, Some(&ResolvedType::Int))?;

        Ok((resolved_min, resolved_max))
    }

    pub fn resolve_range(
        &mut self,
        min_expr: &Expression,
        max_expr: &Expression,
        mode: &RangeMode,
    ) -> Result<ResolvedRange, ResolveError> {
        let (min, max) = self.resolve_min_max_expr(min_expr, max_expr)?;

        let resolved_range_mode = match mode {
            RangeMode::Inclusive => ResolvedRangeMode::Inclusive,
            RangeMode::Exclusive => ResolvedRangeMode::Exclusive,
        };
        Ok(ResolvedRange {
            min,
            max,
            mode: resolved_range_mode,
        })
    }

    /// # Errors
    ///
    pub fn resolve_array_range_access(
        &mut self,
        base_expression: ResolvedExpression,
        array_type_ref: &ResolvedArrayTypeRef,
        range: ResolvedRange,
    ) -> Result<ResolvedExpression, ResolveError> {
        let node = &base_expression.node.clone();

        Ok(self.create_expr_resolved(
            ResolvedExpressionKind::ArrayRangeAccess(Box::from(base_expression), Box::from(range)),
            ResolvedType::Array(array_type_ref.clone()),
            &node,
        ))
    }

    /// # Errors
    ///
    pub fn resolve_string_range_access(
        &mut self,
        base_expr: ResolvedExpression,
        range: ResolvedRange,
    ) -> Result<ResolvedExpression, ResolveError> {
        let ty = base_expr.ty.clone();
        let node = base_expr.node.clone();
        Ok(self.create_expr_resolved(
            ResolvedExpressionKind::StringRangeAccess(Box::from(base_expr), Box::from(range)),
            ty,
            &node,
        ))
    }
}
