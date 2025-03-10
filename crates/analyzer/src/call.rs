/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::TypeContext;
use crate::err::{Error, ErrorKind};
use crate::{Analyzer, LocationSide, SPARSE_TYPE_ID};
use std::rc::Rc;
use swamp_script_node::Node;
use swamp_script_semantic::intr::IntrinsicFunction;
use swamp_script_semantic::{
    ArgumentExpressionOrLocation, Expression, ExpressionKind, MutOrImmutableExpression,
};
use swamp_script_types::prelude::*;

impl Analyzer<'_> {
    /// # Errors
    ///
    pub fn analyze_argument(
        &mut self,
        fn_parameter: &TypeForParameter,
        argument_expr: &swamp_script_ast::MutableOrImmutableExpression,
    ) -> Result<ArgumentExpressionOrLocation, Error> {
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
            ArgumentExpressionOrLocation::Location(mut_location)
        } else {
            if argument_expr.is_mutable.is_some() {
                return Err(self.create_err(
                    ErrorKind::ParameterIsNotMutable,
                    &argument_expr.expression.node,
                ));
            }
            let resolved_expr = self.analyze_expression(&argument_expr.expression, &context)?;
            ArgumentExpressionOrLocation::Expression(resolved_expr)
        };

        Ok(mut_or_immutable)
    }

    /// # Errors
    ///
    pub fn analyze_and_verify_parameters(
        &mut self,
        node: &Node,
        fn_parameters: &[TypeForParameter],
        arguments: &[swamp_script_ast::MutableOrImmutableExpression],
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

        Ok(resolved_arguments)
    }

    pub(crate) fn check_for_internal_static_call(
        &mut self,
        type_name: &swamp_script_ast::QualifiedTypeIdentifier,
        function_name: &swamp_script_ast::Node,
        arguments: &[swamp_script_ast::MutableOrImmutableExpression],
    ) -> Result<Option<Expression>, Error> {
        let (type_name_text, function_name_text) = {
            (
                self.get_text(&type_name.name.0).to_string(),
                self.get_text(function_name).to_string(),
            )
        };

        if !type_name.generic_params.is_empty() {
            let resolved_generic_type_parameters = self.analyze_types(&type_name.generic_params)?;

            if type_name_text == "Sparse" && function_name_text == "new" {
                if !arguments.is_empty() {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(arguments.len(), 0),
                        function_name,
                    ));
                }
                if resolved_generic_type_parameters.len() != 1 {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfTypeArguments(
                            resolved_generic_type_parameters.len(),
                            1,
                        ),
                        function_name,
                    ));
                }

                let rust_type_ref = Rc::new(ExternalType {
                    type_name: type_name_text,
                    number: SPARSE_TYPE_ID, // TODO: FIX hardcoded number
                });

                let rust_type_base = Type::External(rust_type_ref.clone());

                let generic_specific_type = Type::Generic(
                    Box::from(rust_type_base.clone()),
                    resolved_generic_type_parameters.clone(),
                );

                let value_item_type = resolved_generic_type_parameters[0].clone();

                let expr = self.create_expr(
                    ExpressionKind::IntrinsicCallGeneric(
                        IntrinsicFunction::SparseNew,
                        resolved_generic_type_parameters,
                        vec![],
                    ),
                    generic_specific_type,
                    &type_name.name.0,
                );

                return Ok(Some(expr));
            }
        }

        Ok(None)
    }

    /// # Errors
    ///
    pub fn analyze_mut_or_immutable_expression(
        &mut self,
        expr: &swamp_script_ast::MutableOrImmutableExpression,
        context: &TypeContext,
        location_side: LocationSide,
    ) -> Result<MutOrImmutableExpression, Error> {
        let is_mutable = self.to_node_option(Option::from(&expr.is_mutable));
        let expression_or_location = if is_mutable.is_some() {
            ArgumentExpressionOrLocation::Location(self.analyze_to_location(
                &expr.expression,
                context,
                location_side,
            )?)
        } else {
            ArgumentExpressionOrLocation::Expression(
                self.analyze_expression(&expr.expression, context)?,
            )
        };

        Ok(MutOrImmutableExpression {
            expression_or_location,
            is_mutable,
        })
    }
}
