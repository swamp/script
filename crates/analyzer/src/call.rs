/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::{ResolveError, ResolveErrorKind};
use crate::{LocationSide, Resolver, SPARSE_TYPE_ID};
use std::rc::Rc;
use swamp_script_ast::{MutableOrImmutableExpression, Node, QualifiedTypeIdentifier};
use swamp_script_semantic::{
    ResolvedArgumentExpressionOrLocation, ResolvedExpression, ResolvedExpressionKind,
    ResolvedMutOrImmutableExpression, ResolvedNode, ResolvedRustType, ResolvedType,
    ResolvedTypeForParameter,
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
                Some(fn_parameter.resolved_type.clone().unwrap()),
                LocationSide::Rhs,
            )?;
            ResolvedArgumentExpressionOrLocation::Location(mut_location)
        } else {
            let resolved_expr = self.resolve_expression(
                &argument_expr.expression,
                fn_parameter.resolved_type.as_ref(),
            )?;
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

    pub(crate) fn check_for_internal_static_call(
        &mut self,
        type_name: &QualifiedTypeIdentifier,
        function_name: &Node,
        arguments: &[MutableOrImmutableExpression],
    ) -> Result<Option<ResolvedExpression>, ResolveError> {
        let (type_name_text, function_name_text) = {
            (
                self.get_text(&type_name.name.0).to_string(),
                self.get_text(function_name),
            )
        };

        if type_name_text == "Sparse" && function_name_text == "new" {
            if !arguments.is_empty() {
                return Err(self.create_err(
                    ResolveErrorKind::WrongNumberOfArguments(arguments.len(), 0),
                    function_name,
                ));
            }
            let resolved_generic_type_parameters = self.resolve_types(&type_name.generic_params)?;
            if resolved_generic_type_parameters.len() != 1 {
                return Err(self.create_err(
                    ResolveErrorKind::WrongNumberOfTypeArguments(
                        resolved_generic_type_parameters.len(),
                        1,
                    ),
                    function_name,
                ));
            }

            let rust_type_ref = Rc::new(ResolvedRustType {
                type_name: type_name_text,
                number: SPARSE_TYPE_ID, // TODO: FIX hardcoded number
            });

            let rust_type_base = ResolvedType::RustType(rust_type_ref.clone());

            let generic_specific_type = ResolvedType::Generic(
                Box::from(rust_type_base.clone()),
                resolved_generic_type_parameters.clone(),
            );

            let value_item_type = resolved_generic_type_parameters[0].clone();

            let expr = self.create_expr(
                ResolvedExpressionKind::SparseNew(rust_type_ref, value_item_type),
                generic_specific_type,
                &type_name.name.0,
            );

            return Ok(Some(expr));
        }

        Ok(None)
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
