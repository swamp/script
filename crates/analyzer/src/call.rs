/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::ResolveError;
use crate::{Resolver, SPARSE_TYPE_ID};
use std::rc::Rc;
use swamp_script_ast::{Expression, Node, QualifiedTypeIdentifier};
use swamp_script_semantic::Span;
use swamp_script_semantic::{
    ResolvedExpression, ResolvedExternalFunctionCall, ResolvedExternalFunctionDefinitionRef,
    ResolvedFunction, ResolvedInternalFunctionCall, ResolvedInternalFunctionDefinitionRef,
    ResolvedLocalIdentifier, ResolvedMemberCall, ResolvedRustType, ResolvedStaticCall,
    ResolvedStaticCallGeneric, ResolvedStructTypeField, ResolvedStructTypeFieldRef, ResolvedType,
    ResolvedTypeForParameter, Spanned,
};
use tracing::info;

impl<'a> Resolver<'a> {
    fn verify_arguments(
        span: &Span,
        fn_parameters: &[ResolvedTypeForParameter],
        resolved_arguments: &[ResolvedExpression],
    ) -> Result<(), ResolveError> {
        if resolved_arguments.len() != fn_parameters.len() {
            return Err(ResolveError::WrongNumberOfArguments(
                span.clone(),
                resolved_arguments.len(),
                fn_parameters.len(),
            ));
        }

        for (parameter_index, resolved_argument_expression) in resolved_arguments.iter().enumerate()
        {
            let parameter = &fn_parameters[parameter_index];
            let parameter_type = &parameter;
            let argument_type = resolved_argument_expression.resolution();

            if !argument_type.same_type(&parameter_type.resolved_type) {
                info!(%argument_type, param_type=%parameter_type.resolved_type, "incompatible");
                return Err(ResolveError::IncompatibleArguments(
                    resolved_argument_expression.span(),
                    argument_type,
                    parameter_type.resolved_type.clone(),
                ));
            }

            if parameter.is_mutable && !resolved_argument_expression.is_coerce_to_mutable() {
                info!(expr=?resolved_argument_expression, "this is not mutable");
                return Err(ResolveError::ArgumentIsNotMutable(
                    resolved_argument_expression.span(),
                ));
            }
        }

        Ok(())
    }

    fn resolve_and_verify_parameters(
        &mut self,
        span: &Span,
        fn_parameters: &[ResolvedTypeForParameter],
        arguments: &[Expression],
    ) -> Result<Vec<ResolvedExpression>, ResolveError> {
        let resolved_arguments = self.resolve_expressions(arguments)?;

        Self::verify_arguments(span, fn_parameters, &resolved_arguments)?;

        Ok(resolved_arguments)
    }

    fn resolve_member_call(
        &mut self,
        self_expression: &ResolvedExpression,
        ast_member_expression: &Expression,
        ast_member_function_name: &Node,
        ast_arguments: &[Expression],
    ) -> Result<ResolvedMemberCall, ResolveError> {
        let (function_ref, resolved_expression) =
            self.resolve_into_member_function(ast_member_expression, ast_member_function_name)?;

        let (member_function_requires_mutable_self, signature) = match &*function_ref {
            ResolvedFunction::Internal(function_data) => {
                let first_param = function_data.signature.parameters.first().ok_or_else(|| {
                    ResolveError::WrongNumberOfArguments(resolved_expression.span(), 0, 1)
                })?;
                (first_param.is_mutable, &function_data.signature)
            }
            ResolvedFunction::External(external) => {
                let first_param = external.signature.parameters.first().ok_or_else(|| {
                    ResolveError::WrongNumberOfArguments(resolved_expression.span(), 0, 1)
                })?;
                (first_param.is_mutable, &external.signature)
            }
        };

        if member_function_requires_mutable_self && !self_expression.is_coerce_to_mutable() {
            return Err(ResolveError::ExpectedMutableLocation(
                self_expression.span(),
            ));
        }

        let resolved_arguments = self.resolve_and_verify_parameters(
            &resolved_expression.span(),
            &signature.parameters[1..],
            ast_arguments,
        )?;

        Ok(ResolvedMemberCall {
            function: function_ref.clone(),
            arguments: resolved_arguments,
            self_expression: Box::new(resolved_expression),
            // struct_type_ref: resolved_struct_type_ref.clone(),
            self_is_mutable: member_function_requires_mutable_self,
        })
    }

    pub(crate) fn resolve_function_call(
        &mut self,
        function_expression: &Expression,
        ast_arguments: &[Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        let function_expr = self.resolve_expression(function_expression)?;
        let resolution_type = function_expr.resolution();

        if let ResolvedType::Function(signature) = resolution_type {
            let resolved_arguments = self.resolve_and_verify_parameters(
                &function_expr.span(),
                &signature.parameters,
                ast_arguments,
            )?;

            Ok(ResolvedExpression::FunctionCall(
                signature,
                Box::from(function_expr),
                resolved_arguments,
            ))
        } else {
            Err(ResolveError::ExpectedFunctionTypeForFunctionCall(
                function_expr.span(),
            ))
        }
    }

    pub(crate) fn resolve_static_call(
        &mut self,
        type_name: &QualifiedTypeIdentifier,
        function_name: &Node,
        arguments: &[Expression],
    ) -> Result<ResolvedStaticCall, ResolveError> {
        let function_name_str = self.get_text(function_name).to_string();

        let struct_type_ref = self.find_struct_type(type_name)?;
        let struct_ref = struct_type_ref.borrow();

        if let Some(function_ref) = struct_ref.functions.get(&function_name_str) {
            let resolved_arguments = self.resolve_and_verify_parameters(
                &self.to_node(function_name).span,
                &function_ref.signature().parameters,
                arguments,
            )?;

            Ok(ResolvedStaticCall {
                function: function_ref.clone(),
                arguments: resolved_arguments,
            })
        } else {
            Err(ResolveError::CouldNotFindStaticMember(
                self.to_node(&type_name.name.0),
                self.to_node(function_name),
            ))
        }
    }

    #[allow(unused)] // TODO:
    fn resolve_internal_function_call(
        &mut self,
        fn_def: &ResolvedInternalFunctionDefinitionRef,
        function_expr: ResolvedExpression,
        arguments: &[Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_arguments = self.resolve_and_verify_parameters(
            &function_expr.span(),
            &fn_def.signature.parameters,
            arguments,
        )?;

        Ok(ResolvedExpression::FunctionInternalCall(
            ResolvedInternalFunctionCall {
                arguments: resolved_arguments,
                function_definition: fn_def.clone(),
                function_expression: Box::from(function_expr),
            },
        ))
    }

    #[allow(unused)] // TODO:
    fn resolve_external_function_call(
        &mut self,
        fn_def: &ResolvedExternalFunctionDefinitionRef,
        function_expr: ResolvedExpression,
        arguments: &[Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        let resolved_arguments = self.resolve_and_verify_parameters(
            &function_expr.span(),
            &fn_def.signature.parameters,
            arguments,
        )?;

        Ok(ResolvedExpression::FunctionExternalCall(
            ResolvedExternalFunctionCall {
                arguments: resolved_arguments,
                function_definition: fn_def.clone(),
                function_expression: Box::from(function_expr),
            },
        ))
    }

    pub(crate) fn resolve_static_call_generic(
        &mut self,
        type_name: &QualifiedTypeIdentifier,
        function_name: &Node,
        arguments: &[Expression],
    ) -> Result<ResolvedStaticCallGeneric, ResolveError> {
        let resolved_arguments = self.resolve_expressions(arguments)?;
        let resolved_generic_types = self.resolve_types(&type_name.generic_params)?;

        let struct_type_ref = self.find_struct_type(type_name)?;
        let struct_ref = struct_type_ref.borrow();

        let function_name_str = self.get_text(function_name).to_string();

        struct_ref.functions.get(&function_name_str).map_or_else(
            || {
                Err(ResolveError::CouldNotFindStaticMember(
                    self.to_node(&type_name.name.0),
                    self.to_node(function_name),
                ))
            },
            |function_ref| {
                Ok(ResolvedStaticCallGeneric {
                    function: function_ref.clone(),
                    arguments: resolved_arguments,
                    generic_types: resolved_generic_types,
                })
            },
        )
    }

    pub(crate) fn check_for_internal_static_call(
        &mut self,
        type_name: &QualifiedTypeIdentifier,
        function_name: &Node,
        arguments: &[Expression],
    ) -> Result<Option<ResolvedExpression>, ResolveError> {
        let (type_name_text, function_name_text) = {
            (
                self.get_text(&type_name.name.0).to_string(),
                self.get_text(function_name),
            )
        };

        if type_name_text == "Sparse" && function_name_text == "new" {
            if !arguments.is_empty() {
                return Err(ResolveError::WrongNumberOfArguments(
                    self.to_node(function_name).span,
                    arguments.len(),
                    0,
                ));
            }
            let resolved_generic_type_parameters = self.resolve_types(&type_name.generic_params)?;
            if resolved_generic_type_parameters.len() != 1 {
                return Err(ResolveError::WrongNumberOfTypeArguments(
                    resolved_generic_type_parameters.len(),
                    1,
                ));
            }

            let rust_type_ref = Rc::new(ResolvedRustType {
                type_name: type_name_text,
                number: SPARSE_TYPE_ID, // TODO: FIX hardcoded number
            });

            let rust_type_base = ResolvedType::RustType(rust_type_ref.clone());

            let generic_specific_type = ResolvedType::Generic(
                Box::from(rust_type_base),
                resolved_generic_type_parameters.clone(),
            );

            let value_item_type = resolved_generic_type_parameters[0].clone();

            return Ok(Some(ResolvedExpression::SparseNew(
                self.to_node(&type_name.name.0).span,
                rust_type_ref,
                value_item_type,
                generic_specific_type,
            )));
        }

        Ok(None)
    }

    pub(crate) fn member_or_field_call(
        &mut self,
        ast_member_expression: &Expression,
        ast_identifier: &Node,
        ast_arguments: &[Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        if let Some(found_internal) = self.check_for_internal_member_call(
            ast_member_expression,
            ast_identifier,
            ast_arguments,
        )? {
            return Ok(found_internal);
        }

        let resolved_expression = self.resolve_expression(ast_member_expression)?;
        let resolved_type = resolved_expression.resolution();
        let resolved_arguments = self.resolve_expressions(ast_arguments)?;
        let field_or_member_name_str = self.get_text(ast_identifier).to_string();

        // the resolved expression must be a struct either way
        let expr = if let ResolvedType::Struct(struct_type_ref) = resolved_type {
            let borrow_struct = struct_type_ref.borrow();
            if let Some(found_function) = borrow_struct.functions.get(&field_or_member_name_str) {
                let member_signature = found_function.signature();
                if member_signature.first_parameter_is_self {
                    ResolvedExpression::MemberCall(self.resolve_member_call(
                        &resolved_expression,
                        ast_member_expression,
                        ast_identifier,
                        ast_arguments,
                    )?)
                } else {
                    ResolvedExpression::FunctionCall(
                        member_signature.clone(),
                        Box::from(resolved_expression),
                        resolved_arguments,
                    )
                }
            } else if let Some(found_field_ref) = borrow_struct
                .anon_struct_type
                .defined_fields
                .get(&field_or_member_name_str)
            {
                let (base_expr, access_chain, _resulting_type) =
                    self.resolve_field_access_helper(ast_member_expression, ast_identifier)?;
                let index = borrow_struct
                    .anon_struct_type
                    .defined_fields
                    .get_index(&field_or_member_name_str)
                    .expect("field name has been checked previously");
                let field = ResolvedStructTypeField {
                    struct_type_ref: struct_type_ref.clone(),
                    index,
                    resolved_type: found_field_ref.field_type.clone(),
                    field_name: ResolvedLocalIdentifier(self.to_node(ast_identifier)),
                };
                let field_access_expr = ResolvedExpression::FieldAccess(
                    Box::from(base_expr),
                    ResolvedStructTypeFieldRef::from(field),
                    access_chain,
                );
                if let ResolvedType::Function(found_function_signature) =
                    &found_field_ref.field_type
                {
                    ResolvedExpression::FunctionCall(
                        found_function_signature.clone(),
                        Box::from(field_access_expr),
                        resolved_arguments,
                    )
                } else {
                    return Err(ResolveError::WasNotStructType(self.to_node(ast_identifier)));
                }
            } else {
                return Err(ResolveError::WasNotStructType(self.to_node(ast_identifier)));
            }
        } else {
            return Err(ResolveError::WasNotStructType(self.to_node(ast_identifier)));
        };
        Ok(expr)
    }
}
