/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::Analyzer;
use crate::err::{Error, ErrorKind};
use swamp_script_semantic::prelude::IntrinsicFunction;
use swamp_script_semantic::{Postfix, PostfixKind};
use swamp_script_types::prelude::*;

impl Analyzer<'_> {
    pub(crate) fn check_for_internal_member_call(
        &mut self,
        ty: &Type,
        _is_mutable: bool,
        ast_member_function_name: &swamp_script_ast::Node,
        ast_arguments: &[&swamp_script_ast::Expression],
    ) -> Result<Option<Postfix>, Error> {
        match &ty {
            Type::Tuple(tuple_type) => {
                let found = self.analyze_tuple_member_call(
                    tuple_type,
                    ast_member_function_name,
                    ast_arguments,
                )?;
                Ok(Some(found))
            }
            _ => Ok(None),
        }
    }
    fn analyze_tuple_member_call(
        &mut self,
        tuple_type: &[Type],
        ast_member_function_name: &swamp_script_ast::Node,
        arguments: &[&swamp_script_ast::Expression],
    ) -> Result<Postfix, Error> {
        if tuple_type.len() != 2 {
            return Err(self.create_err(
                ErrorKind::WrongNumberOfArguments(2, tuple_type.len()),
                ast_member_function_name,
            ));
        }

        let member_function_name_str = self.get_text(ast_member_function_name);

        let resolved_expr = match (&tuple_type[0], &tuple_type[1]) {
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
                    ErrorKind::WrongNumberOfArguments(99, tuple_type.len()),
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
}
