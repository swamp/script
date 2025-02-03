/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::{ResolveError, ResolveErrorKind};
use crate::Resolver;
use seq_set::SeqSet;
use swamp_script_ast::{FieldExpression, Node, QualifiedTypeIdentifier};
use swamp_script_semantic::{
    ResolvedAnonymousStructType, ResolvedArgumentExpressionOrLocation, ResolvedExpression,
    ResolvedExpressionKind, ResolvedFunctionRef, ResolvedLocationAccess,
    ResolvedLocationAccessKind, ResolvedMutOrImmutableExpression, ResolvedNode,
    ResolvedSingleLocationExpression, ResolvedSingleLocationExpressionKind,
    ResolvedSingleMutLocationExpression, ResolvedStructInstantiation, ResolvedStructTypeRef,
    ResolvedType,
};

impl<'a> Resolver<'a> {
    fn resolve_struct_init_calling_default(
        &mut self,
        function: &ResolvedFunctionRef,
        struct_to_instantiate: ResolvedStructTypeRef,
        source_order_expressions: Vec<(usize, ResolvedNode, ResolvedExpression)>,
        node: &Node,
    ) -> Result<ResolvedExpression, ResolveError> {
        let mut expressions = Vec::new();

        self.push_block_scope("struct_instantiation");

        let temp_var = self.create_local_variable_generated(
            "__generated",
            true,
            &ResolvedType::Struct(struct_to_instantiate.clone()),
        )?;

        // temp_var = StructType::default()
        let return_type = *function.signature().return_type.clone();

        let default_call_kind = self.create_default_static_call(node, &struct_to_instantiate)?;

        let static_call = self.create_expr(default_call_kind, return_type, node);

        let expr = self.create_expr(
            ResolvedExpressionKind::VariableDefinition(
                temp_var.clone(),
                Box::new(ResolvedMutOrImmutableExpression {
                    expression_or_location: ResolvedArgumentExpressionOrLocation::Expression(
                        static_call,
                    ),
                    is_mutable: None,
                }),
            ),
            ResolvedType::Unit,
            node,
        );
        expressions.push(expr);

        // overwrite fields in temp_var with assignments
        for (field_target_index, resolved_field_name_node, field_source_expression) in
            source_order_expressions
        {
            let node = field_source_expression.node.clone();

            let field_expression_type = field_source_expression.ty.clone();

            let kind = ResolvedLocationAccessKind::FieldIndex(
                struct_to_instantiate.clone(),
                field_target_index,
            );

            let single_chain = vec![ResolvedLocationAccess {
                node: resolved_field_name_node,
                ty: field_expression_type.clone(),
                kind,
            }];

            let created_location = ResolvedSingleLocationExpression {
                kind: ResolvedSingleLocationExpressionKind::MutStructFieldRef(
                    struct_to_instantiate.clone(),
                    field_target_index,
                ),
                node: node.clone(),
                ty: field_expression_type,
                starting_variable: temp_var.clone(),
                access_chain: single_chain,
            };

            let created_mut_location = ResolvedSingleMutLocationExpression(created_location);

            let overwrite_expression = self.create_expr_resolved(
                ResolvedExpressionKind::Assignment(
                    Box::from(created_mut_location),
                    Box::new(field_source_expression),
                ),
                ResolvedType::Unit,
                &node,
            );

            expressions.push(overwrite_expression);
        }

        let ty = temp_var.resolved_type.clone();
        let access_variable = self.create_expr(
            ResolvedExpressionKind::VariableAccess(temp_var),
            ty.clone(),
            &node,
        );

        expressions.push(access_variable); // make sure the block returns the overwritten temp_var

        self.pop_block_scope("struct instantiation");

        let block = self.create_expr(ResolvedExpressionKind::Block(expressions), ty, &node);
        Ok(block)
    }

    fn resolve_struct_init_field_by_field(
        &mut self,
        struct_to_instantiate: ResolvedStructTypeRef,
        mut source_order_expressions: Vec<(usize, ResolvedExpression)>,
        missing_fields: SeqSet<String>,
        node: &Node,
    ) -> Result<ResolvedExpression, ResolveError> {
        {
            let borrowed_anon_type = &struct_to_instantiate.borrow().anon_struct_type;

            for missing_field_name in missing_fields {
                let field = borrowed_anon_type
                    .defined_fields
                    .get(&missing_field_name)
                    .expect("should have been verified by helper function");
                let field_index = borrowed_anon_type
                    .defined_fields
                    .get_index(&missing_field_name)
                    .expect("should have been verified earlier");

                let expression = self.create_default_value_for_type(&node, &field.field_type)?; // expression is usually a literal

                source_order_expressions.push((field_index, expression));
            }
        }

        let ty = ResolvedType::Struct(struct_to_instantiate.clone());

        Ok(self.create_expr(
            ResolvedExpressionKind::StructInstantiation(ResolvedStructInstantiation {
                source_order_expressions,
                struct_type_ref: struct_to_instantiate,
            }),
            ty,
            &node,
        ))
    }

    pub(crate) fn resolve_struct_instantiation(
        &mut self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
        ast_fields: &Vec<FieldExpression>,
        has_rest: bool,
    ) -> Result<ResolvedExpression, ResolveError> {
        let struct_to_instantiate = self.get_struct_type(qualified_type_identifier)?;

        let (source_order_expressions, missing_fields) = self
            .resolve_anon_struct_instantiation_helper(
                &struct_to_instantiate.borrow().anon_struct_type,
                ast_fields,
            )?;

        if has_rest {
            if let Some(function) = struct_to_instantiate
                .clone()
                .borrow()
                .functions
                .get(&"default".to_string())
            {
                self.resolve_struct_init_calling_default(
                    function,
                    struct_to_instantiate,
                    source_order_expressions,
                    &qualified_type_identifier.name.0,
                )
            } else {
                let mapped: Vec<(usize, ResolvedExpression)> = source_order_expressions
                    .into_iter()
                    .map(|(a, _b, c)| (a, c))
                    .collect::<Vec<_>>();
                self.resolve_struct_init_field_by_field(
                    struct_to_instantiate,
                    mapped,
                    missing_fields,
                    &qualified_type_identifier.name.0,
                )
            }
        } else if missing_fields.is_empty() {
            let ty = ResolvedType::Struct(struct_to_instantiate.clone());
            let node = qualified_type_identifier.name.0.clone();
            let mapped: Vec<(usize, ResolvedExpression)> = source_order_expressions
                .into_iter()
                .map(|(a, _b, c)| (a, c))
                .collect::<Vec<_>>();
            Ok(self.create_expr(
                ResolvedExpressionKind::StructInstantiation(ResolvedStructInstantiation {
                    source_order_expressions: mapped,
                    struct_type_ref: struct_to_instantiate,
                }),
                ty,
                &node,
            ))
        } else {
            let node = qualified_type_identifier.name.0.clone();
            Err(self.create_err(
                ResolveErrorKind::MissingFieldInStructInstantiation(
                    missing_fields.to_vec(),
                    struct_to_instantiate.borrow().anon_struct_type.clone(),
                ),
                &node,
            ))
        }
    }

    fn resolve_anon_struct_instantiation_helper(
        &mut self,
        struct_to_instantiate: &ResolvedAnonymousStructType,
        ast_fields: &Vec<FieldExpression>,
    ) -> Result<
        (
            Vec<(usize, ResolvedNode, ResolvedExpression)>,
            SeqSet<String>,
        ),
        ResolveError,
    > {
        let mut missing_fields: SeqSet<String> = struct_to_instantiate
            .defined_fields
            .keys()
            .cloned()
            .collect();

        let mut source_order_expressions = Vec::new();

        for field in ast_fields {
            let field_name = self.get_text(&field.field_name.0).to_string();
            let resolved_node = self.to_node(&field.field_name.0);

            // If we can't remove it from missing_fields, it's either a duplicate or unknown field
            if !missing_fields.remove(&field_name) {
                return if struct_to_instantiate
                    .defined_fields
                    .contains_key(&field_name)
                {
                    Err(self.create_err(
                        ResolveErrorKind::DuplicateFieldInStructInstantiation(field_name),
                        &field.field_name.0,
                    ))
                } else {
                    Err(self.create_err(ResolveErrorKind::UnknownStructField, &field.field_name.0))
                };
            }

            let looked_up_field = struct_to_instantiate
                .defined_fields
                .get(&field_name)
                .expect("field existence checked above");

            let field_index_in_definition = struct_to_instantiate
                .defined_fields
                .get_index(&field_name)
                .expect("field_name is checked earlier");

            let resolved_expression =
                self.resolve_expression(&field.expression, Some(&looked_up_field.field_type))?;

            source_order_expressions.push((
                field_index_in_definition,
                resolved_node,
                resolved_expression,
            ));
        }

        Ok((source_order_expressions, missing_fields))
    }

    pub(crate) fn resolve_anon_struct_instantiation(
        &mut self,
        node: &Node,
        struct_to_instantiate: &ResolvedAnonymousStructType,
        ast_fields: &Vec<FieldExpression>,
        allow_rest: bool,
    ) -> Result<Vec<(usize, ResolvedExpression)>, ResolveError> {
        let (source_order_expressions, missing_fields) =
            self.resolve_anon_struct_instantiation_helper(struct_to_instantiate, ast_fields)?;

        let mut mapped: Vec<(usize, ResolvedExpression)> = source_order_expressions
            .into_iter()
            .map(|(a, _b, c)| (a, c))
            .collect::<Vec<_>>();

        if allow_rest {
            // Call `default()` for the missing fields
            for missing_field_name in missing_fields {
                let field = struct_to_instantiate
                    .defined_fields
                    .get(&missing_field_name)
                    .expect("field must exist in struct definition");

                let field_index = struct_to_instantiate
                    .defined_fields
                    .get_index(&missing_field_name)
                    .expect("field must exist in struct definition");

                // Here you would create the default value for the field
                let default_expression =
                    self.create_default_value_for_type(node, &field.field_type)?;

                mapped.push((field_index, default_expression));
            }
        } else if !missing_fields.is_empty() {
            return Err(self.create_err(
                ResolveErrorKind::MissingFieldInStructInstantiation(
                    missing_fields.to_vec(),
                    struct_to_instantiate.clone(),
                ),
                node,
            ));
        }

        Ok(mapped)
    }
}
