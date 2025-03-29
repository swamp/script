/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::SemanticError;
use crate::instantiator::TypeVariableScope;
use seq_map::SeqMap;
use swamp_types::Type;
use tracing::info;

#[derive(Debug, Default)]
pub struct SemanticContext {
    type_variable_scopes: Vec<TypeVariableScope>,
}

impl SemanticContext {
    pub fn push_type_scope(&mut self) {
        self.type_variable_scopes
            .push(TypeVariableScope::new(SeqMap::new()));
    }

    pub fn push_type_scope_with_variables(
        &mut self,
        type_variables: &[String],
    ) -> Result<(), SemanticError> {
        self.push_type_scope();
        for type_variable in type_variables {
            self.declare_type_variable(type_variable)?;
        }
        Ok(())
    }

    pub fn pop_type_scope(&mut self) {
        assert!(!self.type_variable_scopes.is_empty());
        self.type_variable_scopes.pop();
    }

    /// # Errors
    ///
    /// # Panics
    /// if no stack is present
    pub fn declare_type_variable(&mut self, name: &str) -> Result<(), SemanticError> {
        self.type_variable_scopes.last_mut().map_or_else(
            || {
                panic!("Error: No active type variable scope!");
            },
            |current_scope| current_scope.add_type_variable(name),
        )
    }

    #[must_use]
    pub fn resolve_type_variable(&self, name: &str) -> Option<Type> {
        for scope in self.type_variable_scopes.iter().rev() {
            if let Some(found_type) = scope.internal_get_type(&name) {
                return Some(found_type.clone());
            }
        }
        None
    }
}
