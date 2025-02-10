/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::{ResolveError, ResolveErrorKind};
use crate::{LocationSide, Resolver};
use swamp_script_ast::MutableOrImmutableExpression;
use swamp_script_semantic::{
    ResolvedArgumentExpressionOrLocation, ResolvedMutOrImmutableExpression, ResolvedNode,
    ResolvedType, ResolvedTypeForParameter,
};

impl<'a> Resolver<'a> {
    pub fn resolve_argument(
        &mut self,
        fn_parameter: &ResolvedTypeForParameter,
        argument_expr: &MutableOrImmutableExpression,
    ) -> Result<ResolvedArgumentExpressionOrLocation, ResolveError> {
        let mut_or_immutable = if fn_parameter.is_mutable {
            let mut_location = self.resolve_to_location(
                &argument_expr.expression,
                Some(fn_parameter.resolved_type.clone()),
                LocationSide::Rhs,
            )?;
            ResolvedArgumentExpressionOrLocation::Location(mut_location)
        } else {
            let resolved_expr = self
                .resolve_expression(&argument_expr.expression, Some(&fn_parameter.resolved_type))?;
            ResolvedArgumentExpressionOrLocation::Expression(resolved_expr)
        };

        Ok(mut_or_immutable)
    }

    pub fn resolve_and_verify_parameters(
        &mut self,
        node: &ResolvedNode,
        fn_parameters: &[ResolvedTypeForParameter],
        arguments: &[MutableOrImmutableExpression],
    ) -> Result<Vec<ResolvedArgumentExpressionOrLocation>, ResolveError> {
        if fn_parameters.len() != arguments.len() {
            return Err(self.create_err_resolved(
                ResolveErrorKind::WrongNumberOfArguments(fn_parameters.len(), arguments.len()),
                node,
            ));
        }

        let mut resolved_arguments = Vec::new();
        for (fn_parameter, argument_expr) in fn_parameters.iter().zip(arguments) {
            let mut_or_immutable = self.resolve_argument(fn_parameter, argument_expr)?;
            resolved_arguments.push(mut_or_immutable);
        }

        //Self::verify_arguments(span, fn_parameters, &resolved_arguments)?;

        Ok(resolved_arguments)
    }

    pub fn resolve_mut_or_immutable_expression(
        &mut self,
        expr: &MutableOrImmutableExpression,
        expected_type: Option<&ResolvedType>,
        location_side: LocationSide,
    ) -> Result<ResolvedMutOrImmutableExpression, ResolveError> {
        let is_mutable = self.to_node_option(&expr.is_mutable);
        let expression_or_location = if is_mutable.is_some() {
            ResolvedArgumentExpressionOrLocation::Location(self.resolve_to_location(
                &expr.expression,
                expected_type.cloned(),
                location_side,
            )?)
        } else {
            ResolvedArgumentExpressionOrLocation::Expression(
                self.resolve_expression(&expr.expression, expected_type)?,
            )
        };

        Ok(ResolvedMutOrImmutableExpression {
            expression_or_location,
            is_mutable,
        })
    }
}
