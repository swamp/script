/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::ResolveError;
use crate::Resolver;
use std::rc::Rc;
use swamp_script_ast::{EnumVariantLiteral, Expression, Literal};
use swamp_script_semantic::{
    Fp, ResolvedEnumLiteralData, ResolvedEnumVariantContainerType, ResolvedExpression,
    ResolvedLiteral, ResolvedMapType, ResolvedTupleType, ResolvedTupleTypeRef, ResolvedType,
};

impl<'a> Resolver<'a> {
    pub(crate) fn resolve_literal(
        &mut self,
        ast_literal: &Literal,
        expected_type: &ResolvedType,
    ) -> Result<ResolvedLiteral, ResolveError> {
        let resolved_literal = match ast_literal {
            Literal::Int(int_node) => {
                let integer_text = self.get_text(int_node);
                ResolvedLiteral::IntLiteral(
                    Self::str_to_int(integer_text).map_err(ResolveError::IntConversionError)?,
                    self.to_node(int_node),
                )
            }
            Literal::Float(float_node) => {
                let float_str = self.get_text(float_node);
                let float =
                    Self::str_to_float(float_str).map_err(ResolveError::FloatConversionError)?;
                ResolvedLiteral::FloatLiteral(Fp::from(float), self.to_node(float_node))
            }
            Literal::String(string_node, processed_string) => ResolvedLiteral::StringLiteral(
                processed_string.to_string(),
                self.to_node(string_node),
            ),
            Literal::Bool(bool_node) => {
                let bool_str = self.get_text(bool_node);
                let bool_val = if bool_str == "false" {
                    false
                } else if bool_str == "true" {
                    true
                } else {
                    return Err(ResolveError::BoolConversionError);
                };
                ResolvedLiteral::BoolLiteral(bool_val, self.to_node(bool_node))
            }
            Literal::EnumVariant(ref enum_literal) => {
                let (enum_name, variant_name) = match enum_literal {
                    EnumVariantLiteral::Simple(enum_name, variant_name) => {
                        (enum_name, variant_name)
                    }
                    EnumVariantLiteral::Tuple(enum_name, variant_name, _) => {
                        (enum_name, variant_name)
                    }
                    EnumVariantLiteral::Struct(enum_name, variant_name, _) => {
                        (enum_name, variant_name)
                    }
                };

                // Handle enum variant literals in patterns
                let variant_ref = self.resolve_enum_variant_ref(enum_name, variant_name)?;

                let resolved_data = match enum_literal {
                    EnumVariantLiteral::Simple(_, _) => ResolvedEnumLiteralData::Nothing,
                    EnumVariantLiteral::Tuple(_node, _variant, expressions) => {
                        let resolved = self
                            .resolve_expressions(&ResolvedType::Any, expressions)
                            .expect("enum tuple expressions should resolve");
                        ResolvedEnumLiteralData::Tuple(resolved)
                    }
                    EnumVariantLiteral::Struct(
                        _qualified_type_identifier,
                        variant,
                        anonym_struct_field_and_expressions,
                    ) => {
                        if let ResolvedEnumVariantContainerType::Struct(
                            resolved_variant_struct_ref,
                        ) = &variant_ref.data
                        {
                            if anonym_struct_field_and_expressions.len()
                                != resolved_variant_struct_ref.anon_struct.defined_fields.len()
                            {
                                return Err(ResolveError::WrongNumberOfArguments(
                                    self.to_node(&variant.0).span,
                                    anonym_struct_field_and_expressions.len(),
                                    resolved_variant_struct_ref.anon_struct.defined_fields.len(),
                                ));
                            }

                            let resolved = self.resolve_anon_struct_instantiation(
                                &variant.0.clone(),
                                &resolved_variant_struct_ref.anon_struct,
                                anonym_struct_field_and_expressions,
                                false,
                            )?;

                            ResolvedEnumLiteralData::Struct(resolved)
                        } else {
                            return Err(ResolveError::WrongEnumVariantContainer(
                                variant_ref.clone(),
                            ));
                        }
                    }
                };

                ResolvedLiteral::EnumVariantLiteral(variant_ref, resolved_data)
            }

            Literal::Array(items, node) => {
                match expected_type {
                    ResolvedType::Map(map_type_ref) => {
                        //                        let (array_type_ref, resolved_items) = self.resolve_map_type_helper(node, items, expected_type)?;
                        ResolvedLiteral::Map(map_type_ref.clone(), vec![])
                    }
                    ResolvedType::Array(_something) => {
                        let (array_type_ref, resolved_items) =
                            self.resolve_array_type_helper(node, items, expected_type)?;
                        ResolvedLiteral::Array(array_type_ref, resolved_items, self.to_node(node))
                    }
                    _ => {
                        return Err(ResolveError::IncompatibleTypes(
                            self.to_node(node).span,
                            expected_type.clone(),
                        ))
                    }
                }
            }

            Literal::Map(entries) => self.resolve_map_literal(entries)?,

            Literal::Tuple(expressions) => {
                let (tuple_type_ref, resolved_items) = self.resolve_tuple_literal(expressions)?;
                ResolvedLiteral::TupleLiteral(tuple_type_ref, resolved_items)
            }
            Literal::Unit(node) => ResolvedLiteral::UnitLiteral(self.to_node(node)),
            Literal::None(none_node) => ResolvedLiteral::NoneLiteral(self.to_node(none_node)),
        };

        Ok(resolved_literal)
    }

    fn resolve_tuple_literal(
        &mut self,
        items: &[Expression],
    ) -> Result<(ResolvedTupleTypeRef, Vec<ResolvedExpression>), ResolveError> {
        let expressions = self.resolve_expressions(&ResolvedType::Any, items)?;
        let mut tuple_types = Vec::new();
        for expr in &expressions {
            let item_type = expr.resolution();
            tuple_types.push(item_type);
        }

        let tuple_type = ResolvedTupleType(tuple_types);

        let tuple_type_ref = Rc::new(tuple_type);

        Ok((tuple_type_ref, expressions))
    }

    fn resolve_map_literal(
        &mut self,
        entries: &[(Expression, Expression)],
    ) -> Result<ResolvedLiteral, ResolveError> {
        if entries.is_empty() {
            return Err(ResolveError::EmptyMapLiteral);
        }

        // Resolve first entry to determine map types
        let (first_key, first_value) = &entries[0];
        let resolved_first_key = self.resolve_expression(first_key, &ResolvedType::Any)?;
        let resolved_first_value = self.resolve_expression(first_value, &ResolvedType::Any)?;
        let key_type = resolved_first_key.resolution();
        let value_type = resolved_first_value.resolution();

        // Check all entries match the types
        let mut resolved_entries = Vec::new();
        resolved_entries.push((resolved_first_key, resolved_first_value));

        for (key, value) in entries.iter().skip(1) {
            let resolved_key = self.resolve_expression(key, &ResolvedType::Any)?;
            let resolved_value = self.resolve_expression(value, &ResolvedType::Any)?;

            if !resolved_key.resolution().same_type(&key_type) {
                return Err(ResolveError::MapKeyTypeMismatch {
                    expected: key_type,
                    found: resolved_key.resolution(),
                });
            }

            if !resolved_value.resolution().same_type(&value_type) {
                return Err(ResolveError::MapValueTypeMismatch {
                    expected: value_type,
                    found: resolved_value.resolution(),
                });
            }

            resolved_entries.push((resolved_key, resolved_value));
        }

        let resolved_map_type = ResolvedMapType {
            key_type,
            value_type,
        };

        let resolved_map_type_ref = Rc::new(resolved_map_type);

        Ok(ResolvedLiteral::Map(
            resolved_map_type_ref,
            resolved_entries,
        ))
    }
}
