/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use seq_map::SeqMap;
use swamp_script_semantic::Type;
use tracing::info;

#[derive(Debug)]
pub struct TypeParameter {
    pub ty: Type,
    pub debug_name: String,
}

#[derive(Debug)]
pub struct TypeVariableScope {
    pub type_variables: SeqMap<String, Type>,
}

impl TypeVariableScope {}

impl TypeVariableScope {
    pub fn new(scope: SeqMap<String, Type>) -> Self {
        Self {
            type_variables: scope,
        }
    }

    pub(crate) fn variables(&self) -> Vec<Type> {
        self.type_variables.values().map(|ty| ty.clone()).collect()
    }
}

pub struct TypeVariableStack {
    pub type_variable_scopes: Vec<TypeVariableScope>,
}

impl TypeVariableStack {}

impl Default for TypeVariableStack {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeVariableStack {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            type_variable_scopes: Vec::new(),
        }
    }
    pub fn push(&mut self, parameter_name_to_analyzed_type: SeqMap<String, Type>) {
        for ty in &parameter_name_to_analyzed_type {
            info!(?ty, "pushing scope!");
        }
        self.type_variable_scopes.push(TypeVariableScope {
            type_variables: parameter_name_to_analyzed_type,
        });
    }

    /*
    pub(crate) fn get(&self, type_name: &str) -> Option<Type> {
        for scope in self.type_variable_scopes.iter().rev() {
            if let Some(found_type) = scope.type_variables.get(&type_name.to_string()) {
                return Some(found_type.clone());
            }
        }
        None
    }

     */

    /// Pops the most recent type parameter scope off the stack.
    pub fn pop(&mut self) -> TypeVariableScope {
        let found_scope = self.type_variable_scopes.pop().unwrap();
        for ty in &found_scope.type_variables {
            info!(?ty, "popping scope!");
        }

        found_scope
    }
}
