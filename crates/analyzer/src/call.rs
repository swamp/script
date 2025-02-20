/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::{Error, ErrorKind};
use crate::{Analyzer, LocationSide};
use swamp_script_ast::MutableOrImmutableExpression;
use swamp_script_semantic::{
    ArgumentExpressionOrLocation, MutOrImmutableExpression, Node, Type, TypeForParameter,
};

impl Analyzer<'_> {
    pub fn analyze_argument(
        &mut self,
        fn_parameter: &TypeForParameter,
        argument_expr: &MutableOrImmutableExpression,
    ) -> Result<ArgumentExpressionOrLocation, Error> {
        let mut_or_immutable = if fn_parameter.is_mutable {
            let mut_location = self.analyze_to_location(
                &argument_expr.expression,
                Some(fn_parameter.resolved_type.clone()),
                &LocationSide::Rhs,
            )?;
            ArgumentExpressionOrLocation::Location(mut_location)
        } else {
            let resolved_expr = self
                .analyze_expression(&argument_expr.expression, Some(&fn_parameter.resolved_type))?;
            ArgumentExpressionOrLocation::Expression(resolved_expr)
        };

        Ok(mut_or_immutable)
    }

    pub fn analyze_and_verify_parameters(
        &mut self,
        node: &Node,
        fn_parameters: &[TypeForParameter],
        arguments: &[MutableOrImmutableExpression],
    ) -> Result<Vec<ArgumentExpressionOrLocation>, Error> {
        if fn_parameters.len() != arguments.len() {
            return Err(self.create_err_resolved(
                ErrorKind::WrongNumberOfArguments(fn_parameters.len(), arguments.len()),
                node,
            ));
        }

        let mut resolved_arguments = Vec::new();
        for (fn_parameter, argument_expr) in fn_parameters.iter().zip(arguments) {
            let mut_or_immutable = self.analyze_argument(fn_parameter, argument_expr)?;
            resolved_arguments.push(mut_or_immutable);
        }

        //Self::verify_arguments(span, fn_parameters, &resolved_arguments)?;

        Ok(resolved_arguments)
    }

    pub fn analyze_mut_or_immutable_expression(
        &mut self,
        expr: &MutableOrImmutableExpression,
        expected_type: Option<&Type>,
        location_side: &LocationSide,
    ) -> Result<MutOrImmutableExpression, Error> {
        let is_mutable = self.to_node_option(Option::from(&expr.is_mutable));
        let expression_or_location = if is_mutable.is_some() {
            ArgumentExpressionOrLocation::Location(self.analyze_to_location(
                &expr.expression,
                expected_type.cloned(),
                location_side,
            )?)
        } else {
            ArgumentExpressionOrLocation::Expression(
                self.analyze_expression(&expr.expression, expected_type)?,
            )
        };

        Ok(MutOrImmutableExpression {
            expression_or_location,
            is_mutable,
        })
    }
}
