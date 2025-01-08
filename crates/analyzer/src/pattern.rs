use crate::err::ResolveError;
use crate::Resolver;
use swamp_script_ast::{Node, Pattern, PatternElement};
use swamp_script_semantic::{
    ResolvedEnumVariantContainerType, ResolvedEnumVariantTypeRef, ResolvedPattern,
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
            _ => Err(ResolveError::ExpectedEnumInPattern(self.to_node(ast_name)))?,
        };

        let enum_name = &enum_type_ref.assigned_name;
        let variant_name = self.get_text(ast_name).to_string();

        self.shared
            .lookup
            .get_enum_variant_type(&enum_type_ref.module_path, &enum_name, &variant_name)
            .map_or_else(
                || {
                    Err(ResolveError::UnknownEnumVariantTypeInPattern(
                        self.to_node(ast_name),
                    ))
                },
                Ok,
            )
    }

    pub(crate) fn resolve_pattern(
        &mut self,
        ast_pattern: &Pattern,
        expression_type: &ResolvedType,
    ) -> Result<(ResolvedPattern, bool), ResolveError> {
        match ast_pattern {
            Pattern::PatternList(elements) => {
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
                                self.create_local_variable(var, &None, expression_type)?;
                            resolved_elements.push(ResolvedPatternElement::Variable(variable_ref));
                        }
                        PatternElement::Expression(_expr) => {
                            return Err(ResolveError::ExpressionsNotAllowedInLetPattern);
                        }
                        PatternElement::Wildcard(node) => {
                            resolved_elements
                                .push(ResolvedPatternElement::Wildcard(self.to_node(node)));
                        }
                    }
                }
                Ok((
                    ResolvedPattern::PatternList(resolved_elements),
                    scope_is_pushed,
                ))
            }

            Pattern::EnumPattern(variant_name, maybe_elements) => {
                let mut scope_was_pushed = false;
                let enum_variant_type_ref =
                    self.find_variant_in_pattern(expression_type, variant_name)?;

                if let Some(elements) = maybe_elements {
                    let mut resolved_elements = Vec::new();
                    match &enum_variant_type_ref.data {
                        ResolvedEnumVariantContainerType::Tuple(tuple_type) => {
                            // For tuples, elements must be in order but can be partial
                            if elements.len() > tuple_type.fields_in_order.len() {
                                return Err(ResolveError::TooManyTupleFields {
                                    max: tuple_type.fields_in_order.len(),
                                    got: elements.len(),
                                });
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
                                    PatternElement::Expression(_) => {
                                        return Err(
                                            ResolveError::ExpressionsNotAllowedInLetPattern,
                                        );
                                    }
                                }
                            }
                        }
                        ResolvedEnumVariantContainerType::Struct(struct_type) => {
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
                                                ResolveError::UnknownField(self.to_node(var))
                                            })?;

                                        let field_type = struct_type
                                            .anon_struct
                                            .defined_fields
                                            .get(&var_name_str)
                                            .ok_or_else(|| {
                                                ResolveError::UnknownField(self.to_node(var))
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
                                    PatternElement::Expression(_) => {
                                        return Err(
                                            ResolveError::ExpressionsNotAllowedInLetPattern,
                                        );
                                    }
                                }
                            }
                        }
                        ResolvedEnumVariantContainerType::Nothing => {
                            if !elements.is_empty() {
                                return Err(ResolveError::EnumVariantHasNoFields(
                                    self.to_node(variant_name),
                                ));
                            }
                        }
                    }

                    Ok((
                        ResolvedPattern::EnumPattern(
                            enum_variant_type_ref,
                            Some(resolved_elements),
                        ),
                        scope_was_pushed,
                    ))
                } else {
                    Ok((
                        ResolvedPattern::EnumPattern(enum_variant_type_ref, None),
                        false,
                    ))
                }
            }

            Pattern::Literal(ast_literal) => {
                Ok((self.resolve_pattern_literal(ast_literal)?, false))
            }
        }
    }
}
