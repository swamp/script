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
pub struct TypeParameterScope {
    pub type_parameters: SeqMap<String, TypeParameter>,
}

pub struct TypeParameterStack {
    pub type_parameter_scope_stack: Vec<TypeParameterScope>,
}

impl TypeParameterStack {}

impl Default for TypeParameterStack {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeParameterStack {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            type_parameter_scope_stack: Vec::new(),
        }
    }
    pub fn push_type_parameters(
        &mut self,
        parameter_name_to_analyzed_type: SeqMap<String, TypeParameter>,
    ) {
        for ty in &parameter_name_to_analyzed_type {
            info!(?ty, "pushing scope!");
        }
        self.type_parameter_scope_stack.push(TypeParameterScope {
            type_parameters: parameter_name_to_analyzed_type,
        });
    }

    pub(crate) fn get(&self, type_name: &str) -> Option<Type> {
        for scope in self.type_parameter_scope_stack.iter().rev() {
            if let Some(found_type) = scope.type_parameters.get(&type_name.to_string()) {
                return Some(found_type.ty.clone());
            }
        }
        None
    }

    /// Pops the most recent type parameter scope off the stack.
    pub fn pop_type_parameters(&mut self) -> Option<TypeParameterScope> {
        self.type_parameter_scope_stack.pop()
    }
}
