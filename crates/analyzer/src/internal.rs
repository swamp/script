/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::{ResolveError, ResolveErrorKind};
use crate::{Resolver, SPARSE_TYPE_ID};
use swamp_script_ast::{Expression, Node};
use swamp_script_semantic::{
    ResolvedArrayTypeRef, ResolvedMapTypeRef, ResolvedPostfix, ResolvedPostfixKind,
    ResolvedTupleTypeRef,
};
use swamp_script_semantic::{ResolvedExpression, ResolvedType};

impl<'a> Resolver<'a> {
    pub(crate) fn check_for_internal_member_call(
        &mut self,
        ty: &ResolvedType,
        is_mutable: bool,
        ast_member_function_name: &Node,
        ast_arguments: &[&Expression],
    ) -> Result<Option<ResolvedPostfix>, ResolveError> {
        match &ty {
            ResolvedType::Array(array_type_ref) => {
                let resolved = self.resolve_array_member_call(
                    array_type_ref,
                    is_mutable,
                    ast_member_function_name,
                    ast_arguments,
                )?;
                return Ok(Some(resolved));
            }

            ResolvedType::Map(map_type_ref) => {
                //if let ResolvedExpressionKind::VariableAccess(var_ref) = resolved_expr {
                let resolved = self.resolve_map_member_call(
                    &map_type_ref,
                    is_mutable,
                    ast_member_function_name,
                    ast_arguments,
                )?;
                Ok(Some(resolved))
                //}
            }

            ResolvedType::Float => {
                let resolved =
                    self.resolve_float_member_call(ast_member_function_name, ast_arguments)?;
                Ok(Some(resolved))
            }
            ResolvedType::Int => {
                let resolved =
                    self.resolve_int_member_call(ast_member_function_name, ast_arguments)?;
                Ok(Some(resolved))
            }
            ResolvedType::String => {
                let resolved =
                    self.resolve_string_member_call(ast_member_function_name, ast_arguments)?;
                Ok(Some(resolved))
            }
            ResolvedType::Tuple(tuple_type) => {
                let found = self.resolve_tuple_member_call(
                    &tuple_type,
                    ast_member_function_name,
                    ast_arguments,
                )?;
                Ok(Some(found))
            }
            _ => self.check_for_internal_member_call_extra(
                ty,
                ast_member_function_name,
                ast_arguments,
            ),
        }
    }
    fn resolve_tuple_member_call(
        &mut self,
        tuple_type: &ResolvedTupleTypeRef,
        ast_member_function_name: &Node,
        arguments: &[&Expression],
    ) -> Result<ResolvedPostfix, ResolveError> {
        //let resolved_node = self.to_node(ast_member_function_name);
        if tuple_type.0.len() != 2 {
            return Err(self.create_err(
                ResolveErrorKind::WrongNumberOfArguments(2, tuple_type.0.len()),
                ast_member_function_name,
            ));
        }

        let member_function_name_str = self.get_text(ast_member_function_name);

        let resolved_expr = match (&tuple_type.0[0], &tuple_type.0[1]) {
            (ResolvedType::Float, ResolvedType::Float) => match member_function_name_str {
                "magnitude" => {
                    if !arguments.is_empty() {
                        return Err(self.create_err(
                            ResolveErrorKind::WrongNumberOfArguments(arguments.len(), 0),
                            ast_member_function_name,
                        ));
                    }
                    self.create_postfix(
                        ResolvedPostfixKind::Tuple2FloatMagnitude,
                        &ResolvedType::Float,
                        ast_member_function_name,
                    )
                }
                _ => {
                    return Err(self.create_err(
                        ResolveErrorKind::UnknownMemberFunction,
                        ast_member_function_name,
                    ))
                }
            },
            _ => {
                return Err(self.create_err(
                    ResolveErrorKind::WrongNumberOfArguments(99, tuple_type.0.len()),
                    ast_member_function_name,
                ))
            }
        };

        Ok(resolved_expr)
    }

    fn check_mutable(&mut self, is_mutable: bool, node: &Node) -> Result<(), ResolveError> {
        if !is_mutable {
            Err(self.create_err(ResolveErrorKind::ExpectedMutableLocation, node))
        } else {
            Ok(())
        }
    }

    fn create_postfix(
        &mut self,
        kind: ResolvedPostfixKind,
        ty: &ResolvedType,
        node: &Node,
    ) -> ResolvedPostfix {
        let resolved_node = self.to_node(node);

        ResolvedPostfix {
            node: resolved_node,
            ty: ty.clone(),
            kind,
        }
    }

    fn resolve_map_member_call(
        &mut self,
        map_type: &ResolvedMapTypeRef,
        is_mutable: bool,
        ast_member_function_name: &Node,
        ast_arguments: &[&Expression],
    ) -> Result<ResolvedPostfix, ResolveError> {
        let member_function_name_str = self.get_text(ast_member_function_name);

        let expr = match member_function_name_str {
            "remove" => {
                self.check_mutable(is_mutable, &ast_member_function_name)?;

                if ast_arguments.len() != 1 {
                    return Err(self.create_err(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 1),
                        ast_member_function_name,
                    ));
                }

                let key_expr =
                    self.resolve_expression(&ast_arguments[0], Some(&map_type.key_type))?;

                self.create_postfix(
                    ResolvedPostfixKind::MapRemove(Box::new(key_expr), map_type.clone()),
                    &ResolvedType::Unit,
                    ast_member_function_name,
                )
            }

            "has" => {
                if ast_arguments.len() != 1 {
                    return Err(self.create_err(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 1),
                        ast_member_function_name,
                    ));
                }

                let key_expr =
                    self.resolve_expression(&ast_arguments[0], Some(&map_type.key_type))?;

                self.create_postfix(
                    ResolvedPostfixKind::MapHas(Box::new(key_expr)),
                    &ResolvedType::Bool,
                    ast_member_function_name,
                )
            }
            _ => {
                return Err(self.create_err(
                    ResolveErrorKind::UnknownMemberFunction,
                    ast_member_function_name,
                ))
            }
        };
        Ok(expr)
    }

    fn resolve_array_member_call(
        &mut self,
        _array_type_ref: &ResolvedArrayTypeRef,
        is_mutable: bool,
        ast_member_function_name: &Node,
        ast_arguments: &[&Expression],
    ) -> Result<ResolvedPostfix, ResolveError> {
        let member_function_name_str = self.get_text(ast_member_function_name);
        let resolved_postfix = match member_function_name_str {
            "remove" => {
                self.check_mutable(is_mutable, &ast_member_function_name)?;

                if ast_arguments.len() != 1 {
                    return Err(self.create_err(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 1),
                        ast_member_function_name,
                    ));
                }
                let index_expr = self.resolve_usize_index(&ast_arguments[0])?;

                self.create_postfix(
                    ResolvedPostfixKind::ArrayRemoveIndex(Box::new(index_expr)),
                    &ResolvedType::Unit,
                    ast_member_function_name,
                )
            }

            "clear" => {
                self.check_mutable(is_mutable, &ast_member_function_name)?;

                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        ast_member_function_name,
                    ));
                }

                self.create_postfix(
                    ResolvedPostfixKind::ArrayClear,
                    &ResolvedType::Unit,
                    ast_member_function_name,
                )
            }
            _ => {
                return Err(self.create_err(
                    ResolveErrorKind::UnknownMemberFunction,
                    &ast_member_function_name,
                ))
            }
        };

        Ok(resolved_postfix)
    }

    fn resolve_single_float_expression(
        &mut self,
        node: &Node,
        ast_arguments: &[&Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        if ast_arguments.len() != 1 {
            return Err(self.create_err(
                ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 1),
                &node,
            ));
        }
        let float_expression = { self.resolve_immutable(&ast_arguments[0], &ResolvedType::Float)? };

        Ok(float_expression)
    }

    fn resolve_single_int_expression(
        &mut self,
        node: &Node,
        ast_arguments: &[&Expression],
    ) -> Result<ResolvedExpression, ResolveError> {
        if ast_arguments.len() != 1 {
            return Err(self.create_err(
                ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 1),
                &node,
            ));
        }
        let expr2 = self.resolve_immutable(&ast_arguments[0], &ResolvedType::Int)?;

        Ok(expr2)
    }

    fn resolve_two_float_expressions(
        &mut self,
        node: &Node,
        ast_arguments: &[&Expression],
    ) -> Result<(ResolvedExpression, ResolvedExpression), ResolveError> {
        if ast_arguments.len() != 2 {
            return Err(self.create_err(
                ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 2),
                &node,
            ));
        }
        let expr2 = self.resolve_immutable(&ast_arguments[0], &ResolvedType::Float)?;
        let expr3 = self.resolve_immutable(&ast_arguments[1], &ResolvedType::Float)?;

        Ok((expr2, expr3))
    }

    #[allow(clippy::too_many_lines)]
    fn resolve_float_member_call(
        &mut self,
        ast_member_function_name: &Node,
        ast_arguments: &[&Expression],
    ) -> Result<ResolvedPostfix, ResolveError> {
        let function_name_str = self.get_text(ast_member_function_name);
        let node = ast_member_function_name;

        let mut resulting_type = &ResolvedType::Float;
        let kind = match function_name_str {
            "round" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &ast_member_function_name,
                    ));
                }
                ResolvedPostfixKind::FloatRound
            }
            "floor" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &ast_member_function_name,
                    ));
                }
                resulting_type = &ResolvedType::Int;
                ResolvedPostfixKind::FloatFloor
            }
            "sqrt" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &ast_member_function_name,
                    ));
                }
                ResolvedPostfixKind::FloatSqrt
            }
            "sign" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &ast_member_function_name,
                    ));
                }
                ResolvedPostfixKind::FloatSign
            }
            "abs" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &ast_member_function_name,
                    ));
                }
                ResolvedPostfixKind::FloatAbs
            }
            "rnd" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &ast_member_function_name,
                    ));
                }
                resulting_type = &ResolvedType::Int;
                ResolvedPostfixKind::FloatRnd
            }
            "cos" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &ast_member_function_name,
                    ));
                }
                ResolvedPostfixKind::FloatCos
            }
            "sin" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &ast_member_function_name,
                    ));
                }
                ResolvedPostfixKind::FloatSin
            }
            "acos" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &ast_member_function_name,
                    ));
                }
                ResolvedPostfixKind::FloatAcos
            }
            "asin" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &ast_member_function_name,
                    ));
                }
                ResolvedPostfixKind::FloatAsin
            }
            "atan2" => {
                let float_argument = self.resolve_single_float_expression(node, ast_arguments)?;
                ResolvedPostfixKind::FloatAtan2(Box::new(float_argument))
            }
            "min" => {
                let float_argument = self.resolve_single_float_expression(node, ast_arguments)?;
                ResolvedPostfixKind::FloatMin(Box::new(float_argument))
            }
            "max" => {
                let float_argument = self.resolve_single_float_expression(node, ast_arguments)?;
                ResolvedPostfixKind::FloatMax(Box::new(float_argument))
            }
            "clamp" => {
                let (min, max) = self.resolve_two_float_expressions(node, ast_arguments)?;
                ResolvedPostfixKind::FloatClamp(Box::new(min), Box::new(max))
            }
            _ => {
                return Err(self.create_err(
                    ResolveErrorKind::UnknownMemberFunction,
                    &ast_member_function_name,
                ))
            }
        };

        Ok(self.create_postfix(kind, resulting_type, &ast_member_function_name))
    }

    fn resolve_string_member_call(
        &mut self,
        ast_member_function_name: &Node,
        ast_arguments: &[&Expression],
    ) -> Result<ResolvedPostfix, ResolveError> {
        let function_name_str = self.get_text(ast_member_function_name);
        let node = self.to_node(&ast_member_function_name);

        match function_name_str {
            "len" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err_resolved(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &node,
                    ));
                }
                Ok(self.create_postfix(
                    ResolvedPostfixKind::StringLen,
                    &ResolvedType::Int,
                    ast_member_function_name,
                ))
            }
            _ => Err(self.create_err(
                ResolveErrorKind::UnknownMemberFunction,
                ast_member_function_name,
            )),
        }
    }

    fn resolve_int_member_call(
        &mut self,
        ast_member_function_name: &Node,
        ast_arguments: &[&Expression],
    ) -> Result<ResolvedPostfix, ResolveError> {
        let function_name_str = self.get_text(ast_member_function_name);
        let node = self.to_node(&ast_member_function_name);

        let (kind, resolved_type) = match function_name_str {
            "abs" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err_resolved(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &node,
                    ));
                }
                (ResolvedPostfixKind::IntAbs, ResolvedType::Int)
            }
            "rnd" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err_resolved(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &node,
                    ));
                }
                (ResolvedPostfixKind::IntRnd, ResolvedType::Int)
            }

            "max" => {
                let int_argument =
                    self.resolve_single_int_expression(&ast_member_function_name, ast_arguments)?;
                (
                    ResolvedPostfixKind::IntMax(Box::from(int_argument)),
                    ResolvedType::Int,
                )
            }

            "min" => {
                let int_argument =
                    self.resolve_single_int_expression(ast_member_function_name, ast_arguments)?;
                (
                    ResolvedPostfixKind::IntMin(Box::from(int_argument)),
                    ResolvedType::Int,
                )
            }

            "to_float" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err_resolved(
                        ResolveErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &node,
                    ));
                }
                (ResolvedPostfixKind::IntToFloat, ResolvedType::Float)
            }
            _ => {
                return Err(self.create_err(
                    ResolveErrorKind::UnknownMemberFunction,
                    &ast_member_function_name,
                ))
            }
        };

        Ok(self.create_postfix(kind, &resolved_type, ast_member_function_name))
    }

    fn check_for_internal_member_call_extra(
        &mut self,
        ty: &ResolvedType,
        ast_member_function_name: &Node,
        ast_arguments: &[&Expression],
    ) -> Result<Option<ResolvedPostfix>, ResolveError> {
        // TODO: Early out
        if let ResolvedType::Generic(generic_type, parameters) = ty.clone() {
            if let ResolvedType::RustType(rust_type_ref) = *generic_type {
                if rust_type_ref.as_ref().number == SPARSE_TYPE_ID {
                    if parameters.len() != 1 {
                        return Err(self.create_err(
                            ResolveErrorKind::WrongNumberOfTypeArguments(parameters.len(), 1),
                            ast_member_function_name,
                        ));
                    }
                    let sparse_id_type = self
                        .shared
                        .lookup
                        .get_rust_type(&["std".to_string()], "SparseId")
                        .expect("should have SparseId");
                    let key_type = ResolvedType::RustType(sparse_id_type);
                    let value_type = &parameters[0];
                    let function_name_str = self.get_text(ast_member_function_name);

                    // TODO: Remove hack
                    let (kind, resolved_type) = match function_name_str {
                        "add" => {
                            if ast_arguments.len() != 1 {
                                return Err(self.create_err(
                                    ResolveErrorKind::WrongNumberOfTypeArguments(
                                        parameters.len(),
                                        1,
                                    ),
                                    ast_member_function_name,
                                ));
                            }
                            let value = self.resolve_immutable(&ast_arguments[0], value_type)?;
                            (ResolvedPostfixKind::SparseAdd(Box::new(value)), key_type)
                        }
                        "remove" => {
                            if ast_arguments.len() != 1 {
                                return Err(self.create_err(
                                    ResolveErrorKind::WrongNumberOfTypeArguments(
                                        parameters.len(),
                                        1,
                                    ),
                                    ast_member_function_name,
                                ));
                            }
                            let sparse_slot_id_expression =
                                self.resolve_immutable(&ast_arguments[0], &key_type)?;
                            (
                                ResolvedPostfixKind::SparseRemove(Box::new(
                                    sparse_slot_id_expression,
                                )),
                                ResolvedType::Unit, //ResolvedType::Optional(value_type),
                            )
                        }
                        _ => return Ok(None),
                    };

                    return Ok(Some(self.create_postfix(
                        kind,
                        &resolved_type,
                        ast_member_function_name,
                    )));
                }
            }
        }

        Ok(None)
    }
}
