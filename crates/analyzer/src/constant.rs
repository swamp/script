/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::{ResolveError, ResolveErrorKind};
use crate::Resolver;
use swamp_script_semantic::{
    ResolvedConstant, ResolvedConstantRef, ResolvedExpression, ResolvedExpressionKind,
};

impl<'a> Resolver<'a> {
    fn resolve_constant(&mut self, constant: &swamp_script_ast::ConstantInfo) -> Result<(), ResolveError> {
        let resolved_expr = self.resolve_expression(&constant.expression, None)?;
        let resolved_type = resolved_expr.ty.clone();
        let name_node = self.to_node(&constant.constant_identifier.0);
        let name_text = self.get_text_resolved(&name_node).to_string();
        let constant = ResolvedConstant {
            name: name_node.clone(),
            assigned_name: name_text,
            id: 0xffff,
            expr: resolved_expr,
            resolved_type,
        };

        let const_ref = self
            .shared
            .definition_table
            .add_constant(constant)
            .map_err(|s| {
                self.create_err_resolved(ResolveErrorKind::SemanticError(s), &name_node)
            })?;

        self.shared
            .lookup_table
            .add_constant_link(const_ref)
            .map_err(|s| {
                self.create_err_resolved(ResolveErrorKind::SemanticError(s), &name_node)
            })?;

        Ok(())
    }

    pub(crate) fn resolve_constant_definition(
        &mut self,
        constant: &swamp_script_ast::ConstantInfo,
    ) -> Result<(), ResolveError> {
        self.resolve_constant(constant)
    }

    pub(crate) fn resolve_constant_access(
        &self,
        constant_identifier: &swamp_script_ast::ConstantIdentifier,
    ) -> Result<ResolvedExpression, ResolveError> {
        self.try_find_constant(constant_identifier).map_or_else(
            || Err(self.create_err(ResolveErrorKind::UnknownConstant, &constant_identifier.0)),
            |constant_ref| {
                let ty = constant_ref.resolved_type.clone();
                Ok(self.create_expr(
                    ResolvedExpressionKind::ConstantAccess(constant_ref.clone()),
                    ty,
                    &constant_identifier.0,
                ))
            },
        )
    }

    pub fn try_find_constant(
        &self,
        constant_identifier: &swamp_script_ast::ConstantIdentifier,
    ) -> Option<&ResolvedConstantRef> {
        let constant_name = self.get_text(&constant_identifier.0);
        self.shared.lookup_table.get_constant(constant_name)
    }
}
