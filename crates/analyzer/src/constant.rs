/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::ResolveError;
use crate::Resolver;
use swamp_script_ast::{ConstantIdentifier, ConstantInfo};
use swamp_script_semantic::{
    ResolvedConstant, ResolvedConstantRef, ResolvedDefinition, ResolvedExpression, ResolvedNode,
};

impl<'a> Resolver<'a> {
    fn resolve_constant(
        &mut self,
        constant: &ConstantInfo,
    ) -> Result<(ResolvedConstantRef, String, ResolvedNode), ResolveError> {
        let (constant, name_node, name_text) = {
            let resolved_expr = self.resolve_expression(&constant.expression)?;
            let resolved_type = resolved_expr.resolution();
            let name_node = self.to_node(&constant.constant_identifier.0);
            let name_text = self.get_text_resolved(&name_node).to_string();
            let constant = ResolvedConstant {
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

    pub(crate) fn resolve_constant_definition(
        &mut self,
        constant: &ConstantInfo,
    ) -> Result<ResolvedDefinition, ResolveError> {
        let (constant_ref, name, node) = self.resolve_constant(constant)?;

        self.global
            .block_scope_stack
            .last_mut()
            .unwrap()
            .constants
            .insert(name, constant_ref.clone())?;

        Ok(ResolvedDefinition::Constant(node, constant_ref))
    }

    pub(crate) fn resolve_constants(
        &mut self,
        ast_constants: &Vec<ConstantInfo>,
    ) -> Result<Vec<ResolvedConstantRef>, ResolveError> {
        let mut constants = Vec::new();
        for constant in ast_constants {
            let (constant_ref, name, _node) = self.resolve_constant(&constant)?;

            constants.push(constant_ref.clone());

            let constants = &mut self
                .scope
                .block_scope_stack
                .last_mut()
                .expect("block scope should have at least one scope")
                .constants;

            constants.insert(name, constant_ref)?;
        }
        Ok(constants)
    }

    pub(crate) fn resolve_constant_access(
        &self,
        constant_identifier: &ConstantIdentifier,
    ) -> Result<ResolvedExpression, ResolveError> {
        self.try_find_constant(constant_identifier).map_or(
            Err(ResolveError::UnknownConstant(
                self.to_node(&constant_identifier.0),
            )),
            |constant_ref| Ok(ResolvedExpression::ConstantAccess(constant_ref)),
        )
    }

    fn try_find_constant(
        &self,
        constant_identifier: &ConstantIdentifier,
    ) -> Option<ResolvedConstantRef> {
        let constant_name = self.get_text(&constant_identifier.0);
        for scope in self.scope.block_scope_stack.iter().rev() {
            if let Some(value) = scope.constants.get(&constant_name.to_string()) {
                return Some(value.clone());
            }
        }

        self.shared.lookup.get_constant(&vec![], constant_name)
    }
}
