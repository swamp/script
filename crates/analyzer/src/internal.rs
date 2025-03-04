/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::{Error, ErrorKind};
use crate::{Analyzer, SPARSE_TYPE_ID, TypeContext};
use swamp_script_semantic::prelude::IntrinsicFunction;
use swamp_script_semantic::{ArrayTypeRef, MapTypeRef, Postfix, PostfixKind, TupleTypeRef};
use swamp_script_semantic::{Expression, Type};

impl Analyzer<'_> {
    pub(crate) fn check_for_internal_member_call(
        &mut self,
        ty: &Type,
        is_mutable: bool,
        ast_member_function_name: &swamp_script_ast::Node,
        ast_arguments: &[&swamp_script_ast::Expression],
    ) -> Result<Option<Postfix>, Error> {
        match &ty {
            Type::Array(array_type_ref) => {
                let resolved = self.analyze_array_member_call(
                    array_type_ref,
                    is_mutable,
                    ast_member_function_name,
                    ast_arguments,
                )?;
                Ok(Some(resolved))
            }

            Type::Map(map_type_ref) => {
                let resolved = self.analyze_map_member_call(
                    map_type_ref,
                    is_mutable,
                    ast_member_function_name,
                    ast_arguments,
                )?;
                Ok(Some(resolved))
            }

            Type::Float => {
                let resolved =
                    self.analyze_float_member_call(ast_member_function_name, ast_arguments)?;
                Ok(Some(resolved))
            }
            Type::Int => {
                let resolved =
                    self.analyze_int_member_call(ast_member_function_name, ast_arguments)?;
                Ok(Some(resolved))
            }
            Type::String => {
                let resolved =
                    self.analyze_string_member_call(ast_member_function_name, ast_arguments)?;
                Ok(Some(resolved))
            }
            Type::Tuple(tuple_type) => {
                let found = self.analyze_tuple_member_call(
                    tuple_type,
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
    fn analyze_tuple_member_call(
        &mut self,
        tuple_type: &TupleTypeRef,
        ast_member_function_name: &swamp_script_ast::Node,
        arguments: &[&swamp_script_ast::Expression],
    ) -> Result<Postfix, Error> {
        if tuple_type.0.len() != 2 {
            return Err(self.create_err(
                ErrorKind::WrongNumberOfArguments(2, tuple_type.0.len()),
                ast_member_function_name,
            ));
        }

        let member_function_name_str = self.get_text(ast_member_function_name);

        let resolved_expr = match (&tuple_type.0[0], &tuple_type.0[1]) {
            (Type::Float, Type::Float) => match member_function_name_str {
                "magnitude" => {
                    if !arguments.is_empty() {
                        return Err(self.create_err(
                            ErrorKind::WrongNumberOfArguments(arguments.len(), 0),
                            ast_member_function_name,
                        ));
                    }
                    self.create_postfix(
                        PostfixKind::IntrinsicCall(IntrinsicFunction::Float2Magnitude, vec![]),
                        &Type::Float,
                        ast_member_function_name,
                    )
                }
                _ => {
                    return Err(
                        self.create_err(ErrorKind::UnknownMemberFunction, ast_member_function_name)
                    );
                }
            },
            _ => {
                return Err(self.create_err(
                    ErrorKind::WrongNumberOfArguments(99, tuple_type.0.len()),
                    ast_member_function_name,
                ));
            }
        };

        Ok(resolved_expr)
    }

    fn check_mutable(
        &mut self,
        is_mutable: bool,
        node: &swamp_script_ast::Node,
    ) -> Result<(), Error> {
        if is_mutable {
            Ok(())
        } else {
            Err(self.create_err(ErrorKind::ExpectedMutableLocation, node))
        }
    }

    fn create_postfix(
        &mut self,
        kind: PostfixKind,
        ty: &Type,
        node: &swamp_script_ast::Node,
    ) -> Postfix {
        let resolved_node = self.to_node(node);

        Postfix {
            node: resolved_node,
            ty: ty.clone(),
            kind,
        }
    }

    fn analyze_map_member_call(
        &mut self,
        map_type: &MapTypeRef,
        is_mutable: bool,
        ast_member_function_name: &swamp_script_ast::Node,
        ast_arguments: &[&swamp_script_ast::Expression],
    ) -> Result<Postfix, Error> {
        let member_function_name_str = self.get_text(ast_member_function_name);
        let key_arg_context = TypeContext::new_argument(&map_type.key_type);
        let expr = match member_function_name_str {
            "remove" => {
                self.check_mutable(is_mutable, ast_member_function_name)?;

                if ast_arguments.len() != 1 {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 1),
                        ast_member_function_name,
                    ));
                }

                let key_expr = self.analyze_expression(ast_arguments[0], &key_arg_context)?;

                self.create_postfix(
                    PostfixKind::IntrinsicCall(IntrinsicFunction::MapRemove, vec![key_expr]),
                    &Type::Unit,
                    ast_member_function_name,
                )
            }

            "has" => {
                if ast_arguments.len() != 1 {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 1),
                        ast_member_function_name,
                    ));
                }

                let key_expr = self.analyze_expression(ast_arguments[0], &key_arg_context)?;

                self.create_postfix(
                    PostfixKind::IntrinsicCall(IntrinsicFunction::MapHas, vec![key_expr]),
                    &Type::Bool,
                    ast_member_function_name,
                )
            }
            _ => {
                return Err(
                    self.create_err(ErrorKind::UnknownMemberFunction, ast_member_function_name)
                );
            }
        };
        Ok(expr)
    }

    fn analyze_array_member_call(
        &mut self,
        array_type_ref: &ArrayTypeRef,
        is_mutable: bool,
        ast_member_function_name: &swamp_script_ast::Node,
        ast_arguments: &[&swamp_script_ast::Expression],
    ) -> Result<Postfix, Error> {
        let member_function_name_str = self.get_text(ast_member_function_name);
        let resolved_postfix = match member_function_name_str {
            "remove" => {
                self.check_mutable(is_mutable, ast_member_function_name)?;

                if ast_arguments.len() != 1 {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 1),
                        ast_member_function_name,
                    ));
                }
                let index_expr = self.analyze_usize_index(ast_arguments[0])?;

                self.create_postfix(
                    PostfixKind::IntrinsicCall(IntrinsicFunction::VecRemoveIndex, vec![index_expr]),
                    &Type::Unit,
                    ast_member_function_name,
                )
            }

            "push" => {
                self.check_mutable(is_mutable, ast_member_function_name)?;

                if ast_arguments.len() != 1 {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 1),
                        ast_member_function_name,
                    ));
                }

                let element_type_context = TypeContext::new_argument(&array_type_ref.item_type);
                let value_expr =
                    self.analyze_expression(ast_arguments[0], &element_type_context)?;

                self.create_postfix(
                    PostfixKind::IntrinsicCall(IntrinsicFunction::VecPush, vec![value_expr]),
                    &Type::Unit,
                    ast_member_function_name,
                )
            }

            "pop" => {
                self.check_mutable(is_mutable, ast_member_function_name)?;

                if ast_arguments.len() != 0 {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        ast_member_function_name,
                    ));
                }

                self.create_postfix(
                    PostfixKind::IntrinsicCall(IntrinsicFunction::VecPop, vec![]),
                    &Type::Optional(Box::from(array_type_ref.item_type.clone())),
                    ast_member_function_name,
                )
            }

            "clear" => {
                self.check_mutable(is_mutable, ast_member_function_name)?;

                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        ast_member_function_name,
                    ));
                }

                self.create_postfix(
                    PostfixKind::IntrinsicCall(IntrinsicFunction::VecClear, vec![]),
                    &Type::Unit,
                    ast_member_function_name,
                )
            }

            "len" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        ast_member_function_name,
                    ));
                }

                self.create_postfix(
                    PostfixKind::IntrinsicCall(IntrinsicFunction::VecLen, vec![]),
                    &Type::Int,
                    ast_member_function_name,
                )
            }

            "is_empty" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        ast_member_function_name,
                    ));
                }

                self.create_postfix(
                    PostfixKind::IntrinsicCall(IntrinsicFunction::VecIsEmpty, vec![]),
                    &Type::Bool,
                    ast_member_function_name,
                )
            }

            _ => {
                return Err(
                    self.create_err(ErrorKind::UnknownMemberFunction, &ast_member_function_name)
                );
            }
        };

        Ok(resolved_postfix)
    }

    fn analyze_single_float_expression(
        &mut self,
        node: &swamp_script_ast::Node,
        ast_arguments: &[&swamp_script_ast::Expression],
    ) -> Result<Expression, Error> {
        if ast_arguments.len() != 1 {
            return Err(self.create_err(
                ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 1),
                node,
            ));
        }
        let float_expression = { self.analyze_immutable_argument(ast_arguments[0], &Type::Float)? };

        Ok(float_expression)
    }

    fn analyze_single_int_expression(
        &mut self,
        node: &swamp_script_ast::Node,
        ast_arguments: &[&swamp_script_ast::Expression],
    ) -> Result<Expression, Error> {
        if ast_arguments.len() != 1 {
            return Err(self.create_err(
                ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 1),
                &node,
            ));
        }
        let expr2 = self.analyze_immutable_argument(ast_arguments[0], &Type::Int)?;

        Ok(expr2)
    }

    fn analyze_two_float_expressions(
        &mut self,
        node: &swamp_script_ast::Node,
        ast_arguments: &[&swamp_script_ast::Expression],
    ) -> Result<(Expression, Expression), Error> {
        if ast_arguments.len() != 2 {
            return Err(self.create_err(
                ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 2),
                &node,
            ));
        }
        let expr2 = self.analyze_immutable_argument(ast_arguments[0], &Type::Float)?;
        let expr3 = self.analyze_immutable_argument(ast_arguments[1], &Type::Float)?;

        Ok((expr2, expr3))
    }

    #[allow(clippy::too_many_lines)]
    fn analyze_float_member_call(
        &mut self,
        ast_member_function_name: &swamp_script_ast::Node,
        ast_arguments: &[&swamp_script_ast::Expression],
    ) -> Result<Postfix, Error> {
        let function_name_str = self.get_text(ast_member_function_name);
        let node = ast_member_function_name;

        let mut resulting_type = &Type::Float;
        let kind = match function_name_str {
            "round" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        ast_member_function_name,
                    ));
                }
                PostfixKind::IntrinsicCall(IntrinsicFunction::FloatRound, vec![])
            }
            "floor" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        ast_member_function_name,
                    ));
                }
                resulting_type = &Type::Int;
                PostfixKind::IntrinsicCall(IntrinsicFunction::FloatFloor, vec![])
            }
            "sqrt" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        ast_member_function_name,
                    ));
                }
                PostfixKind::IntrinsicCall(IntrinsicFunction::FloatSqrt, vec![])
            }
            "sign" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        ast_member_function_name,
                    ));
                }
                PostfixKind::IntrinsicCall(IntrinsicFunction::FloatSign, vec![])
            }
            "abs" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        ast_member_function_name,
                    ));
                }
                PostfixKind::IntrinsicCall(IntrinsicFunction::FloatAbs, vec![])
            }
            "rnd" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        ast_member_function_name,
                    ));
                }
                resulting_type = &Type::Int;
                PostfixKind::IntrinsicCall(IntrinsicFunction::FloatRnd, vec![])
            }
            "cos" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        ast_member_function_name,
                    ));
                }
                PostfixKind::IntrinsicCall(IntrinsicFunction::FloatCos, vec![])
            }
            "sin" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        ast_member_function_name,
                    ));
                }
                PostfixKind::IntrinsicCall(IntrinsicFunction::FloatSin, vec![])
            }
            "acos" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        ast_member_function_name,
                    ));
                }
                PostfixKind::IntrinsicCall(IntrinsicFunction::FloatAcos, vec![])
            }
            "asin" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        ast_member_function_name,
                    ));
                }
                PostfixKind::IntrinsicCall(IntrinsicFunction::FloatAsin, vec![])
            }
            "atan2" => {
                let float_argument = self.analyze_single_float_expression(node, ast_arguments)?;
                PostfixKind::IntrinsicCall(IntrinsicFunction::FloatAtan2, vec![float_argument])
            }
            "min" => {
                let float_argument = self.analyze_single_float_expression(node, ast_arguments)?;
                PostfixKind::IntrinsicCall(IntrinsicFunction::FloatMin, vec![float_argument])
            }
            "max" => {
                let float_argument = self.analyze_single_float_expression(node, ast_arguments)?;
                PostfixKind::IntrinsicCall(IntrinsicFunction::FloatMax, vec![float_argument])
            }
            "clamp" => {
                let (min, max) = self.analyze_two_float_expressions(node, ast_arguments)?;
                PostfixKind::IntrinsicCall(IntrinsicFunction::FloatClamp, vec![min, max])
            }
            _ => {
                return Err(
                    self.create_err(ErrorKind::UnknownMemberFunction, ast_member_function_name)
                );
            }
        };

        Ok(self.create_postfix(kind, resulting_type, ast_member_function_name))
    }

    fn analyze_string_member_call(
        &mut self,
        ast_member_function_name: &swamp_script_ast::Node,
        ast_arguments: &[&swamp_script_ast::Expression],
    ) -> Result<Postfix, Error> {
        let function_name_str = self.get_text(ast_member_function_name);
        let node = self.to_node(ast_member_function_name);

        match function_name_str {
            "len" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err_resolved(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &node,
                    ));
                }
                Ok(self.create_postfix(
                    PostfixKind::IntrinsicCall(IntrinsicFunction::StringLen, vec![]),
                    &Type::Int,
                    ast_member_function_name,
                ))
            }
            _ => Err(self.create_err(ErrorKind::UnknownMemberFunction, ast_member_function_name)),
        }
    }

    fn analyze_int_member_call(
        &mut self,
        ast_member_function_name: &swamp_script_ast::Node,
        ast_arguments: &[&swamp_script_ast::Expression],
    ) -> Result<Postfix, Error> {
        let function_name_str = self.get_text(ast_member_function_name);
        let node = self.to_node(ast_member_function_name);

        let (kind, resolved_type) = match function_name_str {
            "abs" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err_resolved(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &node,
                    ));
                }
                (
                    PostfixKind::IntrinsicCall(IntrinsicFunction::IntAbs, vec![]),
                    Type::Int,
                )
            }
            "rnd" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err_resolved(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &node,
                    ));
                }
                (
                    PostfixKind::IntrinsicCall(IntrinsicFunction::IntRnd, vec![]),
                    Type::Int,
                )
            }

            "max" => {
                let int_argument =
                    self.analyze_single_int_expression(ast_member_function_name, ast_arguments)?;
                (
                    PostfixKind::IntrinsicCall(IntrinsicFunction::IntMax, vec![int_argument]),
                    Type::Int,
                )
            }

            "min" => {
                let int_argument =
                    self.analyze_single_int_expression(ast_member_function_name, ast_arguments)?;
                (
                    PostfixKind::IntrinsicCall(IntrinsicFunction::IntMin, vec![int_argument]),
                    Type::Int,
                )
            }

            "to_float" => {
                if !ast_arguments.is_empty() {
                    return Err(self.create_err_resolved(
                        ErrorKind::WrongNumberOfArguments(ast_arguments.len(), 0),
                        &node,
                    ));
                }
                (
                    PostfixKind::IntrinsicCall(IntrinsicFunction::IntToFloat, vec![]),
                    Type::Float,
                )
            }
            _ => {
                return Err(
                    self.create_err(ErrorKind::UnknownMemberFunction, ast_member_function_name)
                );
            }
        };

        Ok(self.create_postfix(kind, &resolved_type, ast_member_function_name))
    }

    fn check_for_internal_member_call_extra(
        &mut self,
        ty: &Type,
        ast_member_function_name: &swamp_script_ast::Node,
        ast_arguments: &[&swamp_script_ast::Expression],
    ) -> Result<Option<Postfix>, Error> {
        // TODO: Early out
        if let Type::Generic(generic_type, parameters) = ty.clone() {
            if let Type::External(rust_type_ref) = *generic_type {
                if rust_type_ref.as_ref().number == SPARSE_TYPE_ID {
                    if parameters.len() != 1 {
                        return Err(self.create_err(
                            ErrorKind::WrongNumberOfTypeArguments(parameters.len(), 1),
                            ast_member_function_name,
                        ));
                    }
                    let sparse_id_type = self
                        .shared
                        .lookup_table
                        .get_external_type("SparseId")
                        .expect("should have SparseId");
                    let key_type = Type::External(sparse_id_type.clone());
                    let value_type = &parameters[0];
                    let function_name_str = self.get_text(ast_member_function_name);

                    // TODO: Remove hack
                    let (kind, resolved_type) = match function_name_str {
                        "add" => {
                            if ast_arguments.len() != 1 {
                                return Err(self.create_err(
                                    ErrorKind::WrongNumberOfTypeArguments(parameters.len(), 1),
                                    ast_member_function_name,
                                ));
                            }
                            let value =
                                self.analyze_immutable_argument(&ast_arguments[0], value_type)?;
                            (
                                PostfixKind::IntrinsicCall(
                                    IntrinsicFunction::SparseAdd,
                                    vec![value],
                                ),
                                key_type,
                            )
                        }
                        "remove" => {
                            if ast_arguments.len() != 1 {
                                return Err(self.create_err(
                                    ErrorKind::WrongNumberOfTypeArguments(parameters.len(), 1),
                                    ast_member_function_name,
                                ));
                            }
                            let sparse_slot_id_expression =
                                self.analyze_immutable_argument(&ast_arguments[0], &key_type)?;
                            (
                                PostfixKind::IntrinsicCall(
                                    IntrinsicFunction::SparseRemove,
                                    vec![sparse_slot_id_expression],
                                ),
                                Type::Unit, //Type::Optional(value_type),
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
