/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::{Error, ErrorKind};
use crate::{Analyzer, TypeContext};
use seq_set::SeqSet;
use swamp_script_semantic::{
    AnonymousStructType, ArgumentExpressionOrLocation, Expression, ExpressionKind, FunctionRef,
    LocationAccess, LocationAccessKind, MutOrImmutableExpression, Node, SingleLocationExpression,
    SingleLocationExpressionKind, SingleMutLocationExpression, StructInstantiation, StructTypeRef,
    Type,
};

impl<'a> Analyzer<'a> {
    fn analyze_struct_init_calling_default(
        &mut self,
        function: &FunctionRef,
        struct_to_instantiate: StructTypeRef,
        source_order_expressions: Vec<(usize, Node, Expression)>,
        node: &swamp_script_ast::Node,
    ) -> Result<Expression, Error> {
        let mut expressions = Vec::new();

        self.push_block_scope("struct_instantiation");

        let temp_var = self.create_local_variable_generated(
            "__generated",
            true,
            &Type::Struct(struct_to_instantiate.clone()),
        )?;

        // temp_var = StructType::default()
        let return_type = *function.signature().return_type.clone();

        let default_call_kind = self.create_default_static_call(node, &struct_to_instantiate)?;

        let static_call = self.create_expr(default_call_kind, return_type, node);

        let expr = self.create_expr(
            ExpressionKind::VariableDefinition(
                temp_var.clone(),
                Box::new(MutOrImmutableExpression {
                    expression_or_location: ArgumentExpressionOrLocation::Expression(static_call),
                    is_mutable: None,
                }),
            ),
            Type::Unit,
            node,
        );
        expressions.push(expr);

        // overwrite fields in temp_var with assignments
        for (field_target_index, resolved_field_name_node, field_source_expression) in
            source_order_expressions
        {
            let node = field_source_expression.node.clone();

            let field_expression_type = field_source_expression.ty.clone();

            let kind =
                LocationAccessKind::FieldIndex(struct_to_instantiate.clone(), field_target_index);

            let single_chain = vec![LocationAccess {
                node: resolved_field_name_node,
                ty: field_expression_type.clone(),
                kind,
            }];

            let created_location = SingleLocationExpression {
                kind: SingleLocationExpressionKind::MutStructFieldRef(
                    struct_to_instantiate.clone(),
                    field_target_index,
                ),
                node: node.clone(),
                ty: field_expression_type,
                starting_variable: temp_var.clone(),
                access_chain: single_chain,
            };

            let created_mut_location = SingleMutLocationExpression(created_location);

            let overwrite_expression = self.create_expr_resolved(
                ExpressionKind::Assignment(
                    Box::from(created_mut_location),
                    Box::new(field_source_expression),
                ),
                Type::Unit,
                &node,
            );

            expressions.push(overwrite_expression);
        }

        let ty = temp_var.resolved_type.clone();
        let access_variable =
            self.create_expr(ExpressionKind::VariableAccess(temp_var), ty.clone(), &node);

        expressions.push(access_variable); // make sure the block returns the overwritten temp_var

        self.pop_block_scope("struct instantiation");

        let block = self.create_expr(ExpressionKind::Block(expressions), ty, &node);
        Ok(block)
    }

    fn analyze_struct_init_field_by_field(
        &mut self,
        struct_to_instantiate: StructTypeRef,
        mut source_order_expressions: Vec<(usize, Expression)>,
        missing_fields: SeqSet<String>,
        node: &swamp_script_ast::Node,
    ) -> Result<Expression, Error> {
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

        let ty = Type::Struct(struct_to_instantiate.clone());

        Ok(self.create_expr(
            ExpressionKind::StructInstantiation(StructInstantiation {
                source_order_expressions,
                struct_type_ref: struct_to_instantiate,
            }),
            ty,
            &node,
        ))
    }

    pub(crate) fn analyze_struct_instantiation(
        &mut self,
        qualified_type_identifier: &swamp_script_ast::QualifiedTypeIdentifier,
        ast_fields: &Vec<swamp_script_ast::FieldExpression>,
        has_rest: bool,
    ) -> Result<Expression, Error> {
        let struct_to_instantiate = self.get_struct_type(qualified_type_identifier)?;

        let (source_order_expressions, missing_fields) = self
            .analyze_anon_struct_instantiation_helper(
                &struct_to_instantiate.borrow().anon_struct_type,
                ast_fields,
            )?;

        if has_rest {
            match struct_to_instantiate
                .clone()
                .borrow()
                .functions
                .get(&"default".to_string())
            { Some(function) => {
                self.analyze_struct_init_calling_default(
                    function,
                    struct_to_instantiate,
                    source_order_expressions,
                    &qualified_type_identifier.name.0,
                )
            } _ => {
                let mapped: Vec<(usize, Expression)> = source_order_expressions
                    .into_iter()
                    .map(|(a, _b, c)| (a, c))
                    .collect::<Vec<_>>();
                self.analyze_struct_init_field_by_field(
                    struct_to_instantiate,
                    mapped,
                    missing_fields,
                    &qualified_type_identifier.name.0,
                )
            }}
        } else if missing_fields.is_empty() {
            let ty = Type::Struct(struct_to_instantiate.clone());
            let node = qualified_type_identifier.name.0.clone();
            let mapped: Vec<(usize, Expression)> = source_order_expressions
                .into_iter()
                .map(|(a, _b, c)| (a, c))
                .collect::<Vec<_>>();
            Ok(self.create_expr(
                ExpressionKind::StructInstantiation(StructInstantiation {
                    source_order_expressions: mapped,
                    struct_type_ref: struct_to_instantiate,
                }),
                ty,
                &node,
            ))
        } else {
            let node = qualified_type_identifier.name.0.clone();
            Err(self.create_err(
                ErrorKind::MissingFieldInStructInstantiation(
                    missing_fields.to_vec(),
                    struct_to_instantiate.borrow().anon_struct_type.clone(),
                ),
                &node,
            ))
        }
    }

    fn analyze_anon_struct_instantiation_helper(
        &mut self,
        struct_to_instantiate: &AnonymousStructType,
        ast_fields: &Vec<swamp_script_ast::FieldExpression>,
    ) -> Result<(Vec<(usize, Node, Expression)>, SeqSet<String>), Error> {
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
                        ErrorKind::DuplicateFieldInStructInstantiation(field_name),
                        &field.field_name.0,
                    ))
                } else {
                    Err(self.create_err(ErrorKind::UnknownStructField, &field.field_name.0))
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

            let field_type_context = TypeContext::new_argument(&looked_up_field.field_type);
            let resolved_expression =
                self.analyze_expression(&field.expression, &field_type_context)?;

            source_order_expressions.push((
                field_index_in_definition,
                resolved_node,
                resolved_expression,
            ));
        }

        Ok((source_order_expressions, missing_fields))
    }

    pub(crate) fn analyze_anon_struct_instantiation(
        &mut self,
        node: &swamp_script_ast::Node,
        struct_to_instantiate: &AnonymousStructType,
        ast_fields: &Vec<swamp_script_ast::FieldExpression>,
        allow_rest: bool,
    ) -> Result<Vec<(usize, Expression)>, Error> {
        let (source_order_expressions, missing_fields) =
            self.analyze_anon_struct_instantiation_helper(struct_to_instantiate, ast_fields)?;

        let mut mapped: Vec<(usize, Expression)> = source_order_expressions
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
                ErrorKind::MissingFieldInStructInstantiation(
                    missing_fields.to_vec(),
                    struct_to_instantiate.clone(),
                ),
                node,
            ));
        }

        Ok(mapped)
    }
}
