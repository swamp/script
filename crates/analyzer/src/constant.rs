/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::{Error, ErrorKind};
use crate::Resolver;
use swamp_script_ast::{ConstantIdentifier, ConstantInfo};
use swamp_script_semantic::{Constant, ConstantRef, Definition, Expression, ExpressionKind, Node};

impl<'a> Resolver<'a> {
    fn analyze_constant(
        &mut self,
        constant: &ConstantInfo,
    ) -> Result<(ConstantRef, String, Node), Error> {
        let (constant, name_node, name_text) = {
            let resolved_expr = self.analyze_expression(&constant.expression, None)?;
            let resolved_type = resolved_expr.ty.clone();
            let name_node = self.to_node(&constant.constant_identifier.0);
            let name_text = self.get_text_resolved(&name_node).to_string();
            let constant = Constant {
                name: name_node.clone(),
                assigned_name: name_text.to_string(),
                id: 0xffff,
                expr: resolved_expr,
                resolved_type,
            };
            (constant, name_node, name_text)
        };

        let const_ref = self.shared.lookup.add_constant(constant)?;

        Ok((const_ref, name_text, name_node))
    }

    pub(crate) fn analyze_constant_definition(
        &mut self,
        constant: &ConstantInfo,
    ) -> Result<Definition, Error> {
        let (constant_ref, name, node) = self.analyze_constant(constant)?;

        self.global
            .block_scope_stack
            .last_mut()
            .unwrap()
            .constants
            .insert(name, constant_ref.clone())
            .map_err(|_| {
                self.create_err(ErrorKind::DuplicateFieldName, &constant.expression.node)
            })?;

        Ok(Definition::Constant(node, constant_ref))
    }

    pub(crate) fn analyze_constant_access(
        &self,
        constant_identifier: &ConstantIdentifier,
    ) -> Result<Expression, Error> {
        self.try_find_constant(constant_identifier).map_or_else(
            || Err(self.create_err(ErrorKind::UnknownConstant, &constant_identifier.0)),
            |constant_ref| {
                let ty = constant_ref.resolved_type.clone();
                Ok(self.create_expr(
                    ExpressionKind::ConstantAccess(constant_ref),
                    ty,
                    &constant_identifier.0,
                ))
            },
        )
    }

    #[must_use]
    pub fn try_find_constant(
        &self,
        constant_identifier: &ConstantIdentifier,
    ) -> Option<ConstantRef> {
        let constant_name = self.get_text(&constant_identifier.0);
        for scope in self.scope.block_scope_stack.iter().rev() {
            if let Some(value) = scope.constants.get(&constant_name.to_string()) {
                return Some(value.clone());
            }
        }

        self.shared.lookup.get_constant(&[], constant_name)
    }
}
