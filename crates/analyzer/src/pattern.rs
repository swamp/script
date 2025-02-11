/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::{ResolveError, ResolveErrorKind};
use crate::Resolver;
use swamp_script_ast::{GuardClause, Node, NormalPattern, Pattern, PatternElement};
use swamp_script_semantic::{
    ResolvedEnumVariantType, ResolvedEnumVariantTypeRef, ResolvedNormalPattern, ResolvedPattern,
    ResolvedPatternElement, ResolvedType,
};
use tracing::info;

impl<'a> Resolver<'a> {
    fn find_variant_in_pattern(
        &self,
        expression_type: &ResolvedType,
        ast_name: &Node,
    ) -> Result<ResolvedEnumVariantTypeRef, ResolveError> {
        let enum_type_ref = match expression_type {
            ResolvedType::Enum(enum_type_ref) => enum_type_ref,
            _ => Err(self.create_err(ResolveErrorKind::ExpectedEnumInPattern, ast_name))?,
        };

        let variant_name = self.get_text(ast_name).to_string();

        enum_type_ref
            .borrow()
            .get_variant(&variant_name)
            .map_or_else(
                || {
                    Err(self.create_err(
                        ResolveErrorKind::UnknownEnumVariantTypeInPattern,
                        ast_name,
                    ))
                },
                |found_variant| Ok(found_variant.clone()),
            )
    }

    pub(crate) fn resolve_pattern(
        &mut self,
        ast_pattern: &Pattern,
        expected_condition_type: &ResolvedType,
    ) -> Result<(ResolvedPattern, bool), ResolveError> {
        match ast_pattern {
            Pattern::Wildcard(node) => Ok((ResolvedPattern::Wildcard(self.to_node(node)), false)),
            Pattern::NormalPattern(node, normal_pattern, maybe_guard) => {
                let (normal_pattern, was_pushed) =
                    self.resolve_normal_pattern(node, normal_pattern, expected_condition_type)?;
                let resolved_guard = if let Some(guard_clause) = maybe_guard {
                    match guard_clause {
                        GuardClause::Wildcard(_) => None,
                        GuardClause::Expression(clause_expr) => {
                            Some(self.resolve_bool_expression(&clause_expr)?)
                        }
                    }
                } else {
                    None
                };
                Ok((
                    ResolvedPattern::Normal(normal_pattern, resolved_guard),
                    was_pushed,
                ))
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn resolve_normal_pattern(
        &mut self,
        node: &Node,
        ast_normal_pattern: &NormalPattern,
        expected_condition_type: &ResolvedType,
    ) -> Result<(ResolvedNormalPattern, bool), ResolveError> {
        match ast_normal_pattern {
            NormalPattern::PatternList(elements) => {
                let mut resolved_elements = Vec::new();
                let mut scope_is_pushed = false;
                for element in elements {
                    match element {
                        PatternElement::Variable(var) => {
                            if !scope_is_pushed {
                                self.push_block_scope("pattern_list one variable");
                                scope_is_pushed = true;
                            }
                            let variable_ref =
                                self.create_local_variable(var, &None, expected_condition_type)?;
                            resolved_elements.push(ResolvedPatternElement::Variable(variable_ref));
                        }
                        PatternElement::Expression(expr) => {
                            return Err(self.create_err(
                                ResolveErrorKind::ExpressionsNotAllowedInLetPattern,
                                &expr.node,
                            ));
                        }
                        PatternElement::Wildcard(node) => {
                            resolved_elements
                                .push(ResolvedPatternElement::Wildcard(self.to_node(node)));
                        }
                    }
                }
                Ok((
                    ResolvedNormalPattern::PatternList(resolved_elements),
                    scope_is_pushed,
                ))
            }

            NormalPattern::EnumPattern(variant_name, maybe_elements) => {
                let mut scope_was_pushed = false;
                let enum_variant_type_ref =
                    self.find_variant_in_pattern(expected_condition_type, variant_name)?;

                if let Some(elements) = maybe_elements {
                    let mut resolved_elements = Vec::new();
                    match &*enum_variant_type_ref {
                        ResolvedEnumVariantType::Tuple(tuple_type) => {
                            // For tuples, elements must be in order but can be partial
                            if elements.len() > tuple_type.fields_in_order.len() {
                                return Err(self.create_err(
                                    ResolveErrorKind::TooManyTupleFields {
                                        max: tuple_type.fields_in_order.len(),
                                        got: elements.len(),
                                    },
                                    variant_name,
                                ));
                            }

                            if !scope_was_pushed {
                                self.push_block_scope("enum tuple");
                                scope_was_pushed = true;
                            }

                            // Only zip with as many fields as we have elements
                            for (element, field_type) in
                                elements.iter().zip(&tuple_type.fields_in_order)
                            {
                                match element {
                                    PatternElement::Variable(var) => {
                                        info!(?var, "ENUM TUPLE found variable to handle");
                                        let variable_ref =
                                            self.create_local_variable(var, &None, field_type)?;
                                        resolved_elements
                                            .push(ResolvedPatternElement::Variable(variable_ref));
                                    }
                                    PatternElement::Wildcard(node) => {
                                        resolved_elements.push(ResolvedPatternElement::Wildcard(
                                            self.to_node(node),
                                        ));
                                    }
                                    PatternElement::Expression(expr) => {
                                        return Err(self.create_err(
                                            ResolveErrorKind::ExpressionsNotAllowedInLetPattern,
                                            &expr.node,
                                        ));
                                    }
                                }
                            }
                        }
                        ResolvedEnumVariantType::Struct(struct_type) => {
                            if !scope_was_pushed {
                                self.push_block_scope("enum struct");
                                scope_was_pushed = true;
                            }
                            // For structs, can match any subset of fields in any order
                            for element in elements {
                                match element {
                                    PatternElement::Variable(var) => {
                                        let var_name_str = self.get_text(var).to_string();
                                        // Check if the field exists
                                        let field_index = struct_type
                                            .anon_struct
                                            .defined_fields
                                            .get_index(&var_name_str)
                                            .ok_or_else(|| {
                                                self.create_err(ResolveErrorKind::UnknownField, var)
                                            })?;

                                        let field_type = struct_type
                                            .anon_struct
                                            .defined_fields
                                            .get(&var_name_str)
                                            .ok_or_else(|| {
                                                self.create_err(ResolveErrorKind::UnknownField, var)
                                            })?;

                                        let variable_ref = self.create_local_variable(
                                            var,
                                            &None,
                                            &field_type.field_type,
                                        )?;

                                        resolved_elements.push(
                                            ResolvedPatternElement::VariableWithFieldIndex(
                                                variable_ref,
                                                field_index,
                                            ),
                                        );
                                    }
                                    PatternElement::Wildcard(node) => {
                                        resolved_elements.push(ResolvedPatternElement::Wildcard(
                                            self.to_node(node),
                                        ));
                                    }
                                    PatternElement::Expression(expr) => {
                                        return Err(self.create_err(
                                            ResolveErrorKind::ExpressionsNotAllowedInLetPattern,
                                            &expr.node,
                                        ));
                                    }
                                }
                            }
                        }
                        ResolvedEnumVariantType::Nothing(_) => {
                            if !elements.is_empty() {
                                return Err(self.create_err(
                                    ResolveErrorKind::EnumVariantHasNoFields,
                                    &variant_name,
                                ));
                            }
                        }
                    }

                    Ok((
                        ResolvedNormalPattern::EnumPattern(
                            enum_variant_type_ref,
                            Some(resolved_elements),
                        ),
                        scope_was_pushed,
                    ))
                } else {
                    Ok((
                        ResolvedNormalPattern::EnumPattern(enum_variant_type_ref, None),
                        false,
                    ))
                }
            }

            NormalPattern::Literal(ast_literal) => Ok((
                self.resolve_pattern_literal(node, ast_literal, expected_condition_type)?,
                false,
            )),
        }
    }
}
