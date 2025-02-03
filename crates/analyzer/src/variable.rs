/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::{ResolveError, ResolveErrorKind};
use crate::{BlockScopeMode, Resolver};
use std::rc::Rc;
use swamp_script_ast::{Node, Variable};
use swamp_script_semantic::{
    ResolvedExpression, ResolvedExpressionKind, ResolvedMutOrImmutableExpression, ResolvedNode,
    ResolvedType, ResolvedVariable, ResolvedVariableRef,
};
use tracing::error;

impl<'a> Resolver<'a> {
    fn try_find_local_variable(&self, node: &ResolvedNode) -> Option<&ResolvedVariableRef> {
        let current_scope = self
            .scope
            .block_scope_stack
            .iter()
            .last()
            .expect("no scope stack available");

        let variable_text = self.get_text_resolved(node).to_string();

        current_scope.variables.get(&variable_text)
    }
    #[allow(unused)]
    pub(crate) fn find_variable(
        &self,
        variable: &Variable,
    ) -> Result<ResolvedVariableRef, ResolveError> {
        self.try_find_variable(&variable.name).map_or_else(
            || Err(self.create_err(ResolveErrorKind::UnknownVariable, &variable.name)),
            Ok,
        )
    }

    pub(crate) fn try_find_variable(&self, node: &Node) -> Option<ResolvedVariableRef> {
        let variable_text = self.get_text(node);

        for scope in self.scope.block_scope_stack.iter().rev() {
            if let Some(value) = scope.variables.get(&variable_text.to_string()) {
                return Some(value.clone());
            }
            if scope.mode == BlockScopeMode::Closed {
                break;
            }
        }

        None
    }

    pub(crate) fn set_or_overwrite_variable_with_type(
        &mut self,
        variable: &Variable,
        variable_type_ref: &ResolvedType,
    ) -> Result<(ResolvedVariableRef, bool), ResolveError> {
        if let Some(existing_variable) = self.try_find_variable(&variable.name) {
            // Check type compatibility
            if !&existing_variable
                .resolved_type
                .assignable_type(variable_type_ref)
            {
                return Err(self.create_err(
                    ResolveErrorKind::OverwriteVariableWithAnotherType,
                    &variable.name,
                ));
            }

            // For reassignment, check if the EXISTING variable is mutable
            if !existing_variable.is_mutable() {
                return Err(self.create_err(
                    ResolveErrorKind::CanOnlyOverwriteVariableWithMut,
                    &variable.name,
                ));
            }

            return Ok((existing_variable, true));
        }

        // For first assignment, create new variable with the mutability from the assignment
        let scope_index = self.scope.block_scope_stack.len() - 1;
        let name = self.to_node(&variable.name);
        let mutable_node = self.to_node_option(&variable.is_mutable);
        let variable_name_str = self.get_text_resolved(&name).to_string();

        let variables = &mut self
            .scope
            .block_scope_stack
            .last_mut()
            .expect("block scope should have at least one scope")
            .variables;
        let variable_index = variables.len();

        let resolved_variable = ResolvedVariable {
            name,
            resolved_type: variable_type_ref.clone(),
            mutable_node,
            scope_index,
            variable_index,
        };

        let variable_ref = Rc::new(resolved_variable);

        {
            variables
                .insert(variable_name_str, variable_ref.clone())
                .expect("should have checked earlier for variable");
        }

        Ok((variable_ref, false))
    }
    pub(crate) fn create_local_variable(
        &mut self,
        variable: &Node,
        is_mutable: &Option<Node>,
        variable_type_ref: &ResolvedType,
    ) -> Result<ResolvedVariableRef, ResolveError> {
        if variable_type_ref == &ResolvedType::Unit {
            error!("serious");
        }
        assert_ne!(*variable_type_ref, ResolvedType::Unit);
        self.create_local_variable_resolved(
            &self.to_node(variable),
            &self.to_node_option(is_mutable),
            variable_type_ref,
        )
    }

    pub(crate) fn create_variable(
        &mut self,
        variable: &Variable,
        variable_type_ref: &ResolvedType,
    ) -> Result<ResolvedVariableRef, ResolveError> {
        self.create_local_variable(&variable.name, &variable.is_mutable, variable_type_ref)
    }

    pub(crate) fn create_local_variable_resolved(
        &mut self,
        variable: &ResolvedNode,
        is_mutable: &Option<ResolvedNode>,
        variable_type_ref: &ResolvedType,
    ) -> Result<ResolvedVariableRef, ResolveError> {
        if let Some(_existing_variable) = self.try_find_local_variable(variable) {
            return Err(self
                .create_err_resolved(ResolveErrorKind::OverwriteVariableNotAllowedHere, variable));
        }
        let variable_str = self.get_text_resolved(variable).to_string();

        let scope_index = self.scope.block_scope_stack.len() - 1;

        let variables = &mut self
            .scope
            .block_scope_stack
            .last_mut()
            .expect("block scope should have at least one scope")
            .variables;

        let resolved_variable = ResolvedVariable {
            name: variable.clone(),
            resolved_type: variable_type_ref.clone(),
            mutable_node: is_mutable.clone(),
            scope_index,
            variable_index: variables.len(),
        };

        let variable_ref = Rc::new(resolved_variable);

        variables
            .insert(variable_str, variable_ref.clone())
            .expect("should have checked earlier for variable");

        Ok(variable_ref)
    }

    pub(crate) fn create_local_variable_generated(
        &mut self,
        variable_str: &str,
        is_mutable: bool,
        variable_type_ref: &ResolvedType,
    ) -> Result<ResolvedVariableRef, ResolveError> {
        let scope_index = self.scope.block_scope_stack.len() - 1;

        let variables = &mut self
            .scope
            .block_scope_stack
            .last_mut()
            .expect("block scope should have at least one scope")
            .variables;

        let resolved_variable = ResolvedVariable {
            name: ResolvedNode::default(),
            resolved_type: variable_type_ref.clone(),
            mutable_node: if is_mutable {
                Some(ResolvedNode::default())
            } else {
                None
            },
            scope_index,
            variable_index: variables.len(),
        };

        let variable_ref = Rc::new(resolved_variable);

        variables
            .insert(variable_str.to_string(), variable_ref.clone())
            .expect("should have checked earlier for variable");

        Ok(variable_ref)
    }

    pub(crate) fn create_variable_binding_for_with(
        &mut self,
        ast_variable: &Variable,
        converted_expression: ResolvedMutOrImmutableExpression,
    ) -> Result<ResolvedExpression, ResolveError> {
        let expression_type = converted_expression.ty().clone();
        let (variable_ref, _is_reassignment) =
            self.set_or_overwrite_variable_with_type(ast_variable, &expression_type)?;
        let expr_kind = ResolvedExpressionKind::VariableDefinition(
            variable_ref,
            Box::from(converted_expression),
        );

        let expr = self.create_expr(expr_kind, expression_type, &ast_variable.name);
        //        };
        Ok(expr)
    }
}
