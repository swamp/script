/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::{ResolveError, ResolveErrorKind};
use crate::Resolver;
use std::rc::Rc;
use swamp_script_ast::{EnumVariantLiteral, Expression, LiteralKind, Node};
use swamp_script_semantic::{
    Fp, ResolvedEnumLiteralData, ResolvedEnumVariantType, ResolvedExpression, ResolvedLiteral,
    ResolvedMapType, ResolvedMapTypeRef, ResolvedNode, ResolvedTupleType, ResolvedTupleTypeRef,
    ResolvedType,
};
use tracing::error;

impl<'a> Resolver<'a> {
    #[allow(clippy::too_many_lines)]
    pub(crate) fn resolve_literal(
        &mut self,
        ast_node: &Node,
        ast_literal_kind: &LiteralKind,
        expected_type: Option<&ResolvedType>,
    ) -> Result<(ResolvedLiteral, ResolvedType), ResolveError> {
        let node_text = self.get_text(&ast_node);
        let resolved_literal = match &ast_literal_kind {
            LiteralKind::Int => (
                ResolvedLiteral::IntLiteral(Self::str_to_int(node_text).map_err(
                    |int_conversion_err| {
                        self.create_err(
                            ResolveErrorKind::IntConversionError(int_conversion_err),
                            ast_node,
                        )
                    },
                )?),
                ResolvedType::Int,
            ),
            LiteralKind::Float => {
                let float = Self::str_to_float(node_text).map_err(|float_conversion_err| {
                    self.create_err(
                        ResolveErrorKind::FloatConversionError(float_conversion_err),
                        ast_node,
                    )
                })?;
                (
                    ResolvedLiteral::FloatLiteral(Fp::from(float)),
                    ResolvedType::Float,
                )
            }
            LiteralKind::String(processed_string) => (
                ResolvedLiteral::StringLiteral(processed_string.to_string()),
                ResolvedType::String,
            ),
            LiteralKind::Bool => {
                let bool_val = if node_text == "false" {
                    false
                } else if node_text == "true" {
                    true
                } else {
                    return Err(self.create_err(ResolveErrorKind::BoolConversionError, ast_node));
                };
                (ResolvedLiteral::BoolLiteral(bool_val), ResolvedType::Bool)
            }
            LiteralKind::EnumVariant(ref enum_literal) => {
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

                let enum_type_ref = self.resolve_enum_ref(enum_name)?;
                let enum_type = ResolvedType::Enum(enum_type_ref);

                // Handle enum variant literals in patterns
                let variant_ref = self.resolve_enum_variant_ref(enum_name, variant_name)?;

                let resolved_data = match enum_literal {
                    EnumVariantLiteral::Simple(_, _) => ResolvedEnumLiteralData::Nothing,
                    EnumVariantLiteral::Tuple(_node, _variant, expressions) => {
                        let resolved = self
                            .resolve_expressions(None, expressions)
                            .expect("enum tuple expressions should resolve");
                        ResolvedEnumLiteralData::Tuple(resolved)
                    }
                    EnumVariantLiteral::Struct(
                        _qualified_type_identifier,
                        variant,
                        anonym_struct_field_and_expressions,
                    ) => {
                        if let ResolvedEnumVariantType::Struct(resolved_variant_struct_ref) =
                            &*variant_ref
                        {
                            if anonym_struct_field_and_expressions.len()
                                != resolved_variant_struct_ref.anon_struct.defined_fields.len()
                            {
                                return Err(self.create_err(
                                    ResolveErrorKind::WrongNumberOfArguments(
                                        anonym_struct_field_and_expressions.len(),
                                        resolved_variant_struct_ref
                                            .anon_struct
                                            .defined_fields
                                            .len(),
                                    ),
                                    &variant.0,
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
                            return Err(self.create_err(
                                ResolveErrorKind::WrongEnumVariantContainer(variant_ref.clone()),
                                &variant.0,
                            ));
                        }
                    }
                };

                (
                    ResolvedLiteral::EnumVariantLiteral(variant_ref, resolved_data),
                    enum_type,
                )
            }

            LiteralKind::Array(items) => {
                if items.len() == 0 {
                    if let Some(found_expected_type) = expected_type {
                        match found_expected_type {
                            ResolvedType::Map(map_type_ref) => (
                                ResolvedLiteral::Map(map_type_ref.clone(), vec![]),
                                found_expected_type.clone(),
                            ),
                            ResolvedType::Array(array_type_ref) => (
                                ResolvedLiteral::Array(array_type_ref.clone(), vec![]),
                                found_expected_type.clone(),
                            ),
                            _ => {
                                return Err(self.create_err(
                                    ResolveErrorKind::EmptyArrayCanOnlyBeMapOrArray,
                                    &ast_node,
                                ))
                            }
                        }
                    } else {
                        return Err(self.create_err(
                            ResolveErrorKind::EmptyArrayCanOnlyBeMapOrArray,
                            &ast_node,
                        ));
                    }
                } else {
                    let (array_type_ref, resolved_items) =
                        self.resolve_array_type_helper(ast_node, &items, expected_type)?;
                    (
                        ResolvedLiteral::Array(array_type_ref.clone(), resolved_items),
                        ResolvedType::Array(array_type_ref),
                    )
                }
            }

            LiteralKind::Map(entries) => {
                let (map_literal, map_type_ref) = self.resolve_map_literal(ast_node, &entries)?;

                (map_literal, ResolvedType::Map(map_type_ref.clone()))
            }

            LiteralKind::Tuple(expressions) => {
                let (tuple_type_ref, resolved_items) = self.resolve_tuple_literal(&expressions)?;
                (
                    ResolvedLiteral::TupleLiteral(tuple_type_ref.clone(), resolved_items),
                    ResolvedType::Tuple(tuple_type_ref.clone()),
                )
            }
            LiteralKind::None => {
                if let Some(found_expected_type) = expected_type {
                    if let ResolvedType::Optional(_some_type) = found_expected_type {
                        return Ok((ResolvedLiteral::NoneLiteral, found_expected_type.clone()));
                    }
                }
                return Err(self.create_err(ResolveErrorKind::NoneNeedsExpectedTypeHint, &ast_node));
            }
        };

        Ok(resolved_literal)
    }

    fn resolve_tuple_literal(
        &mut self,
        items: &[Expression],
    ) -> Result<(ResolvedTupleTypeRef, Vec<ResolvedExpression>), ResolveError> {
        let expressions = self.resolve_expressions(None, items)?;
        let mut tuple_types = Vec::new();
        for expr in &expressions {
            let item_type = expr.ty.clone();
            tuple_types.push(item_type);
        }

        let tuple_type = ResolvedTupleType(tuple_types);

        let tuple_type_ref = Rc::new(tuple_type);

        Ok((tuple_type_ref, expressions))
    }

    fn resolve_map_literal(
        &mut self,
        node: &Node,
        entries: &[(Expression, Expression)],
    ) -> Result<(ResolvedLiteral, ResolvedMapTypeRef), ResolveError> {
        if entries.is_empty() {
            return Err(self.create_err(ResolveErrorKind::EmptyMapLiteral, node));
        }

        // Resolve first entry to determine map types
        let (first_key, first_value) = &entries[0];
        let resolved_first_key = self.resolve_expression(first_key, None)?;
        let resolved_first_value = self.resolve_expression(first_value, None)?;
        let key_type = resolved_first_key.ty.clone();
        let value_type = resolved_first_value.ty.clone();

        // Check all entries match the types
        let mut resolved_entries = Vec::new();
        resolved_entries.push((resolved_first_key, resolved_first_value));

        for (key, value) in entries.iter().skip(1) {
            let resolved_key = self.resolve_expression(key, None)?;
            let resolved_value = self.resolve_expression(value, None)?;

            if !resolved_key.ty.same_type(&key_type) {
                return Err(self.create_err(
                    ResolveErrorKind::MapKeyTypeMismatch {
                        expected: key_type,
                        found: resolved_key.ty.clone(),
                    },
                    node,
                ));
            }

            if !resolved_value.ty.same_type(&value_type) {
                return Err(self.create_err(
                    ResolveErrorKind::MapValueTypeMismatch {
                        expected: value_type,
                        found: resolved_value.ty.clone(),
                    },
                    node,
                ));
            }

            resolved_entries.push((resolved_key, resolved_value));
        }

        let resolved_map_type = ResolvedMapType {
            key_type,
            value_type,
        };

        let resolved_map_type_ref = Rc::new(resolved_map_type);

        let literal = ResolvedLiteral::Map(resolved_map_type_ref.clone(), resolved_entries);
        Ok((literal, resolved_map_type_ref))
    }

    #[must_use]
    pub fn create_err(&self, kind: ResolveErrorKind, ast_node: &Node) -> ResolveError {
        error!(?kind, "error created");
        ResolveError {
            node: self.to_node(ast_node),
            kind,
        }
    }

    #[must_use]
    pub fn create_err_resolved(
        &self,
        kind: ResolveErrorKind,
        resolved_node: &ResolvedNode,
    ) -> ResolveError {
        ResolveError {
            node: resolved_node.clone(),
            kind,
        }
    }
}
