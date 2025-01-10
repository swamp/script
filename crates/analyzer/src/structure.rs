/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::ResolveError;
use crate::Resolver;
use seq_set::SeqSet;
use swamp_script_ast::{FieldExpression, Node, QualifiedTypeIdentifier};
use swamp_script_semantic::{
    ResolvedAccess, ResolvedAnonymousStructType, ResolvedExpression, ResolvedNode,
    ResolvedStaticCall, ResolvedStructInstantiation, ResolvedType, ResolvedVariableAssignment,
};

impl<'a> Resolver<'a> {
    pub(crate) fn resolve_struct_instantiation(
        &mut self,
        qualified_type_identifier: &QualifiedTypeIdentifier,
        ast_fields: &Vec<FieldExpression>,
        has_rest: bool,
    ) -> Result<ResolvedExpression, ResolveError> {
        let struct_to_instantiate = self.get_struct_type(qualified_type_identifier)?;

        let (mut source_order_expressions, missing_fields) = self
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
                let mut expressions = Vec::new();

                self.push_block_scope("struct_instantiation");

                let temp_var = self.create_local_variable_generated(
                    "__generated",
                    true,
                    &ResolvedType::Struct(struct_to_instantiate.clone()),
                )?;

                // temp_var = StructType::default()
                expressions.push(ResolvedExpression::InitializeVariable(
                    ResolvedVariableAssignment {
                        variable_refs: vec![temp_var.clone()],
                        expression: Box::new(ResolvedExpression::StaticCall(ResolvedStaticCall {
                            function: function.clone(),
                            arguments: vec![],
                        })),
                    },
                ));

                // overwrite fields with assignments
                for (field_target_index, field_expressions) in source_order_expressions {
                    let overwrite_expression = ResolvedExpression::StructFieldAssignment(
                        Box::new(ResolvedExpression::VariableAccess(temp_var.clone())),
                        vec![ResolvedAccess::FieldIndex(
                            ResolvedNode::default(),
                            field_target_index,
                        )],
                        Box::new(field_expressions),
                    );
                    expressions.push(overwrite_expression);
                }

                expressions.push(ResolvedExpression::VariableAccess(temp_var)); // make sure the block returns the overwritten temp_var

                self.pop_block_scope("struct instantiation");
                Ok(ResolvedExpression::Block(expressions))
            } else {
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

                        let expression = Self::create_default_value_for_type(&field.field_type)?; // expression is usually a literal

                        source_order_expressions.push((field_index, expression));
                    }
                }

                Ok(ResolvedExpression::StructInstantiation(
                    ResolvedStructInstantiation {
                        source_order_expressions,
                        struct_type_ref: struct_to_instantiate,
                    },
                ))
            }
        } else if missing_fields.is_empty() {
            Ok(ResolvedExpression::StructInstantiation(
                ResolvedStructInstantiation {
                    source_order_expressions,
                    struct_type_ref: struct_to_instantiate,
                },
            ))
        } else {
            Err(ResolveError::MissingFieldInStructInstantiation(
                self.to_node(&qualified_type_identifier.name.0).span,
                missing_fields.to_vec(),
                struct_to_instantiate.borrow().anon_struct_type.clone(),
            ))
        }
    }

    fn resolve_anon_struct_instantiation_helper(
        &mut self,
        struct_to_instantiate: &ResolvedAnonymousStructType,
        ast_fields: &Vec<FieldExpression>,
    ) -> Result<(Vec<(usize, ResolvedExpression)>, SeqSet<String>), ResolveError> {
        let mut missing_fields: SeqSet<String> = struct_to_instantiate
            .defined_fields
            .keys()
            .cloned()
            .collect();

        let mut source_order_expressions = Vec::new();

        for field in ast_fields {
            let field_name = self.get_text(&field.field_name.0).to_string();

            // If we can't remove it from missing_fields, it's either a duplicate or unknown field
            if !missing_fields.remove(&field_name) {
                return if struct_to_instantiate
                    .defined_fields
                    .contains_key(&field_name)
                {
                    Err(ResolveError::DuplicateFieldInStructInstantiation(
                        field_name,
                    ))
                } else {
                    Err(ResolveError::UnknownStructField(
                        self.to_node(&field.field_name.0),
                    ))
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

            let resolved_expression = self.resolve_expression_expecting_type(
                &field.expression,
                &looked_up_field.field_type,
                true,
            )?;
            /*
            let expression_type = resolved_expression.resolution();

            if !looked_up_field.field_type.same_type(&expression_type) {
                error!("types: {looked_up_field:?} expr: {expression_type:?}");
                return Err(ResolveError::ExpressionIsOfWrongFieldType(
                    self.to_node(&field.field_name.0).span,
                    looked_up_field.field_type.clone(),
                    expression_type,
                ));
            }

             */

            source_order_expressions.push((field_index_in_definition, resolved_expression));
        }

        Ok((source_order_expressions, missing_fields))
    }

    pub(crate) fn resolve_anon_struct_instantiation(
        &mut self,
        node: Node,
        struct_to_instantiate: &ResolvedAnonymousStructType,
        ast_fields: &Vec<FieldExpression>,
        allow_rest: bool,
    ) -> Result<Vec<(usize, ResolvedExpression)>, ResolveError> {
        let (mut source_order_expressions, missing_fields) =
            self.resolve_anon_struct_instantiation_helper(struct_to_instantiate, ast_fields)?;
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
                let default_expression = Self::create_default_value_for_type(&field.field_type)?;

                source_order_expressions.push((field_index, default_expression));
            }
        } else if !missing_fields.is_empty() {
            return Err(ResolveError::MissingFieldInStructInstantiation(
                self.to_node(&node).span,
                missing_fields.to_vec(),
                struct_to_instantiate.clone(),
            ));
        }

        Ok(source_order_expressions)
    }
}
