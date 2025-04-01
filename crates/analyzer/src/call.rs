/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::TypeContext;
use crate::err::{Error, ErrorKind};
use crate::{Analyzer, LocationSide};
use source_map_node::Node;
use swamp_semantic::MutRefOrImmutableExpression;
use swamp_types::prelude::*;

impl Analyzer<'_> {
    /// # Errors
    ///
    pub fn analyze_argument(
        &mut self,
        fn_parameter: &TypeForParameter,
        argument_expr: &swamp_ast::MutableReferenceOrImmutableExpression,
    ) -> Result<MutRefOrImmutableExpression, Error> {
        let context = TypeContext::new_argument(&fn_parameter.resolved_type);

        let mut_or_immutable = if fn_parameter.is_mutable {
            if argument_expr.is_mutable.is_none() {
                return Err(self.create_err(
                    ErrorKind::ArgumentIsNotMutable,
                    &argument_expr.expression.node,
                ));
            }
            let mut_location =
                self.analyze_to_location(&argument_expr.expression, &context, LocationSide::Rhs)?;
            MutRefOrImmutableExpression::Location(mut_location)
        } else {
            if argument_expr.is_mutable.is_some() {
                return Err(self.create_err(
                    ErrorKind::ParameterIsNotMutable,
                    &argument_expr.expression.node,
                ));
            }
            let resolved_expr = self.analyze_expression(&argument_expr.expression, &context)?;
            MutRefOrImmutableExpression::Expression(resolved_expr)
        };

        Ok(mut_or_immutable)
    }

    /// # Errors
    ///
    pub fn analyze_and_verify_parameters(
        &mut self,
        node: &Node,
        fn_parameters: &[TypeForParameter],
        arguments: &[swamp_ast::MutableReferenceOrImmutableExpression],
    ) -> Result<Vec<MutRefOrImmutableExpression>, Error> {
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

        Ok(resolved_arguments)
    }

    /// # Errors
    ///
    pub fn analyze_mut_or_immutable_expression(
        &mut self,
        expr: &swamp_ast::MutableReferenceOrImmutableExpression,
        context: &TypeContext,
        location_side: LocationSide,
    ) -> Result<MutRefOrImmutableExpression, Error> {
        let is_mutable = self.to_node_option(Option::from(&expr.is_mutable));
        let expression_or_location = if is_mutable.is_some() {
            MutRefOrImmutableExpression::Location(self.analyze_to_location(
                &expr.expression,
                context,
                location_side,
            )?)
        } else {
            MutRefOrImmutableExpression::Expression(
                self.analyze_expression(&expr.expression, context)?,
            )
        };

        Ok(expression_or_location)
    }
}
