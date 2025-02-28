/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::{Error, ErrorKind};
use crate::Analyzer;
use swamp_script_semantic::{Constant, ConstantRef, Expression, ExpressionKind};

impl<'a> Analyzer<'a> {
    fn analyze_constant(&mut self, constant: &swamp_script_ast::ConstantInfo) -> Result<(), Error> {
        let resolved_expr = self.analyze_expression(&constant.expression, None)?;
        let resolved_type = resolved_expr.ty.clone();
        let name_node = self.to_node(&constant.constant_identifier.0);
        let name_text = self.get_text_resolved(&name_node).to_string();
        let constant = Constant {
            name: name_node.clone(),
            assigned_name: name_text,
            id: self.shared.state.constants_in_dependency_order.len() as u32,
            expr: resolved_expr,
            resolved_type,
        };

        let const_ref = self
            .shared
            .definition_table
            .add_constant(constant)
            .map_err(|s| self.create_err_resolved(ErrorKind::SemanticError(s), &name_node))?;

        self.shared
            .lookup_table
            .add_constant_link(const_ref.clone())
            .map_err(|s| self.create_err_resolved(ErrorKind::SemanticError(s), &name_node))?;

        // This extra storage of the constants in modules is to have them in analyze / dependency order
        self.shared
            .state
            .constants_in_dependency_order
            .push(const_ref);

        Ok(())
    }

    pub(crate) fn analyze_constant_definition(
        &mut self,
        constant: &swamp_script_ast::ConstantInfo,
    ) -> Result<(), Error> {
        self.analyze_constant(constant)
    }

    pub(crate) fn analyze_constant_access(
        &self,
        constant_identifier: &swamp_script_ast::ConstantIdentifier,
    ) -> Result<Expression, Error> {
        self.try_find_constant(constant_identifier).map_or_else(
            || Err(self.create_err(ErrorKind::UnknownConstant, &constant_identifier.0)),
            |constant_ref| {
                let ty = constant_ref.resolved_type.clone();
                Ok(self.create_expr(
                    ExpressionKind::ConstantAccess(constant_ref.clone()),
                    ty,
                    &constant_identifier.0,
                ))
            },
        )
    }

    pub fn try_find_constant(
        &self,
        constant_identifier: &swamp_script_ast::ConstantIdentifier,
    ) -> Option<&ConstantRef> {
        let constant_name = self.get_text(&constant_identifier.0);
        self.shared.lookup_table.get_constant(constant_name)
    }
}
