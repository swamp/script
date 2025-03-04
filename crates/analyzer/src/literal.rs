/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::{Error, ErrorKind};
use crate::{Analyzer, TypeContext};
use std::rc::Rc;
use swamp_script_semantic::{
    EnumLiteralData, EnumVariantType, Expression, Fp, Literal, MapType, MapTypeRef, Node,
    TupleType, TupleTypeRef, Type,
};
use tracing::error;

impl Analyzer<'_> {
    #[allow(clippy::too_many_lines)]
    pub(crate) fn analyze_literal(
        &mut self,
        ast_node: &swamp_script_ast::Node,
        ast_literal_kind: &swamp_script_ast::LiteralKind,
        context: &TypeContext,
    ) -> Result<(Literal, Type), Error> {
        let node_text = self.get_text(ast_node);
        let resolved_literal = match &ast_literal_kind {
            swamp_script_ast::LiteralKind::Int => (
                Literal::IntLiteral(Self::str_to_int(node_text).map_err(|int_conversion_err| {
                    self.create_err(ErrorKind::IntConversionError(int_conversion_err), ast_node)
                })?),
                Type::Int,
            ),
            swamp_script_ast::LiteralKind::Float => {
                let float = Self::str_to_float(node_text).map_err(|float_conversion_err| {
                    self.create_err(
                        ErrorKind::FloatConversionError(float_conversion_err),
                        ast_node,
                    )
                })?;
                (Literal::FloatLiteral(Fp::from(float)), Type::Float)
            }
            swamp_script_ast::LiteralKind::String(processed_string) => (
                Literal::StringLiteral(processed_string.to_string()),
                Type::String,
            ),
            swamp_script_ast::LiteralKind::Bool => {
                let bool_val = if node_text == "false" {
                    false
                } else if node_text == "true" {
                    true
                } else {
                    return Err(self.create_err(ErrorKind::BoolConversionError, ast_node));
                };
                (Literal::BoolLiteral(bool_val), Type::Bool)
            }
            swamp_script_ast::LiteralKind::EnumVariant(enum_literal) => {
                let (enum_name, variant_name) = match enum_literal {
                    swamp_script_ast::EnumVariantLiteral::Simple(enum_name, variant_name) => {
                        (enum_name, variant_name)
                    }
                    swamp_script_ast::EnumVariantLiteral::Tuple(enum_name, variant_name, _) => {
                        (enum_name, variant_name)
                    }
                    swamp_script_ast::EnumVariantLiteral::Struct(enum_name, variant_name, _) => {
                        (enum_name, variant_name)
                    }
                };

                let (symbol_table, name) = self.get_symbol_table_and_name(enum_name)?;
                if let Some(enum_type_ref) = symbol_table.get_enum(&name) {
                    let enum_type = Type::Enum(enum_type_ref.clone());

                    // Handle enum variant literals in patterns
                    let variant_ref = self.analyze_enum_variant_ref(enum_name, variant_name)?;

                    let resolved_data = match enum_literal {
                        swamp_script_ast::EnumVariantLiteral::Simple(_, _) => {
                            EnumLiteralData::Nothing
                        }
                        swamp_script_ast::EnumVariantLiteral::Tuple(
                            _node,
                            _variant,
                            expressions,
                        ) => {
                            let resolved = self.analyze_argument_expressions(None, expressions)?;
                            EnumLiteralData::Tuple(resolved)
                        }
                        swamp_script_ast::EnumVariantLiteral::Struct(
                            _qualified_type_identifier,
                            variant,
                            anonym_struct_field_and_expressions,
                        ) => {
                            if let EnumVariantType::Struct(resolved_variant_struct_ref) =
                                &*variant_ref
                            {
                                if anonym_struct_field_and_expressions.len()
                                    != resolved_variant_struct_ref
                                        .anon_struct
                                        .field_name_sorted_fields
                                        .len()
                                {
                                    return Err(self.create_err(
                                        ErrorKind::WrongNumberOfArguments(
                                            anonym_struct_field_and_expressions.len(),
                                            resolved_variant_struct_ref
                                                .anon_struct
                                                .field_name_sorted_fields
                                                .len(),
                                        ),
                                        &variant.0,
                                    ));
                                }

                                let resolved = self.analyze_anon_struct_instantiation(
                                    &variant.0.clone(),
                                    &resolved_variant_struct_ref.anon_struct,
                                    anonym_struct_field_and_expressions,
                                    false,
                                )?;

                                EnumLiteralData::Struct(resolved)
                            } else {
                                return Err(self.create_err(
                                    ErrorKind::WrongEnumVariantContainer(variant_ref.clone()),
                                    &variant.0,
                                ));
                            }
                        }
                    };

                    return Ok((
                        Literal::EnumVariantLiteral(variant_ref, resolved_data),
                        enum_type,
                    ));
                }
                return Err(self.create_err(ErrorKind::UnknownEnumType, ast_node));
            }

            swamp_script_ast::LiteralKind::Array(items) => {
                if items.len() == 0 {
                    if let Some(found_expected_type) = context.expected_type {
                        match found_expected_type {
                            Type::Map(map_type_ref) => (
                                Literal::Map(map_type_ref.clone(), vec![]),
                                found_expected_type.clone(),
                            ),
                            Type::Array(array_type_ref) => (
                                Literal::Array(array_type_ref.clone(), vec![]),
                                found_expected_type.clone(),
                            ),
                            _ => {
                                return Err(self.create_err(
                                    ErrorKind::EmptyArrayCanOnlyBeMapOrArray,
                                    ast_node,
                                ));
                            }
                        }
                    } else {
                        return Err(
                            self.create_err(ErrorKind::EmptyArrayCanOnlyBeMapOrArray, ast_node)
                        );
                    }
                } else {
                    let (array_type_ref, resolved_items) =
                        self.analyze_array_type_helper(ast_node, items, context.expected_type)?;
                    (
                        Literal::Array(array_type_ref.clone(), resolved_items),
                        Type::Array(array_type_ref),
                    )
                }
            }

            swamp_script_ast::LiteralKind::Map(entries) => {
                let (map_literal, map_type_ref) = self.analyze_map_literal(ast_node, &entries)?;

                (map_literal, Type::Map(map_type_ref))
            }

            swamp_script_ast::LiteralKind::Tuple(expressions) => {
                let (tuple_type_ref, resolved_items) = self.analyze_tuple_literal(&expressions)?;
                (
                    Literal::TupleLiteral(tuple_type_ref.clone(), resolved_items),
                    Type::Tuple(tuple_type_ref),
                )
            }
            swamp_script_ast::LiteralKind::None => {
                if let Some(found_expected_type) = context.expected_type {
                    if let Type::Optional(_some_type) = found_expected_type {
                        return Ok((Literal::NoneLiteral, found_expected_type.clone()));
                    }
                }
                return Err(self.create_err(ErrorKind::NoneNeedsExpectedTypeHint, &ast_node));
            }
        };

        Ok(resolved_literal)
    }

    fn analyze_tuple_literal(
        &mut self,
        items: &[swamp_script_ast::Expression],
    ) -> Result<(TupleTypeRef, Vec<Expression>), Error> {
        let expressions = self.analyze_argument_expressions(None, items)?;
        let mut tuple_types = Vec::new();
        for expr in &expressions {
            let item_type = expr.ty.clone();
            tuple_types.push(item_type);
        }

        let tuple_type = TupleType(tuple_types);

        let tuple_type_ref = Rc::new(tuple_type);

        Ok((tuple_type_ref, expressions))
    }

    fn analyze_map_literal(
        &mut self,
        node: &swamp_script_ast::Node,
        entries: &[(swamp_script_ast::Expression, swamp_script_ast::Expression)],
    ) -> Result<(Literal, MapTypeRef), Error> {
        if entries.is_empty() {
            return Err(self.create_err(ErrorKind::EmptyMapLiteral, node));
        }

        // Resolve first entry to determine map types
        let (first_key, first_value) = &entries[0];
        let anything_context = TypeContext::new_anything_argument();
        let resolved_first_key = self.analyze_expression(first_key, &anything_context)?;
        let resolved_first_value = self.analyze_expression(first_value, &anything_context)?;
        let key_type = resolved_first_key.ty.clone();
        let value_type = resolved_first_value.ty.clone();

        let key_context = TypeContext::new_argument(&key_type);
        let value_context = TypeContext::new_argument(&value_type);

        // Check all entries match the types
        let mut resolved_entries = Vec::new();
        resolved_entries.push((resolved_first_key, resolved_first_value));

        for (key, value) in entries.iter().skip(1) {
            let resolved_key = self.analyze_expression(key, &key_context)?;
            let resolved_value = self.analyze_expression(value, &value_context)?;

            if !resolved_key.ty.compatible_with(&key_type) {
                return Err(self.create_err(
                    ErrorKind::MapKeyTypeMismatch {
                        expected: key_type,
                        found: resolved_key.ty,
                    },
                    node,
                ));
            }

            if !resolved_value.ty.compatible_with(&value_type) {
                return Err(self.create_err(
                    ErrorKind::MapValueTypeMismatch {
                        expected: value_type,
                        found: resolved_value.ty,
                    },
                    node,
                ));
            }

            resolved_entries.push((resolved_key, resolved_value));
        }

        let resolved_map_type = MapType {
            key_type,
            value_type,
        };

        let resolved_map_type_ref = Rc::new(resolved_map_type);

        let literal = Literal::Map(resolved_map_type_ref.clone(), resolved_entries);
        Ok((literal, resolved_map_type_ref))
    }

    #[must_use]
    pub fn create_err(&self, kind: ErrorKind, ast_node: &swamp_script_ast::Node) -> Error {
        error!(?kind, "error created");
        Error {
            node: self.to_node(ast_node),
            kind,
        }
    }

    #[must_use]
    pub fn create_err_resolved(&self, kind: ErrorKind, resolved_node: &Node) -> Error {
        Error {
            node: resolved_node.clone(),
            kind,
        }
    }
}
