/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::ExecuteError;
use crate::prelude::{ValueReference, VariableValue};
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_core::prelude::Value;
use swamp_script_core::value::ValueRef;
use swamp_script_semantic::ResolvedVariableRef;
use tracing::error;

#[derive(Debug, Clone)]
pub struct BlockScope {
    variables: Vec<VariableValue>,
}

fn create_fixed_vec(capacity: usize) -> Vec<VariableValue> {
    let mut items = Vec::with_capacity(capacity);
    items.extend((0..capacity).map(|_| VariableValue::Value(Value::Unit)));
    items
}
impl Default for BlockScope {
    fn default() -> Self {
        Self {
            variables: create_fixed_vec(64),
        }
    }
}

impl BlockScope {
    #[inline(always)]
    fn set(&mut self, variable_index: usize, value: VariableValue) {
        self.variables[variable_index] = value;
    }

    #[inline(always)]
    fn get(&self, variable_index: usize) -> &VariableValue {
        &self.variables[variable_index]
    }
}

#[derive(Clone)]
pub struct BlockScopes {
    current_block_scopes: Vec<BlockScope>,
}

impl BlockScopes {}

impl Default for BlockScopes {
    fn default() -> Self {
        Self::new()
    }
}

impl BlockScopes {
    pub fn new() -> Self {
        Self {
            current_block_scopes: vec![BlockScope::default()],
        }
    }

    pub fn push(&mut self) {
        self.current_block_scopes.push(BlockScope::default());
    }

    pub fn pop(&mut self) {
        self.current_block_scopes.pop();
    }

    pub fn clear(&mut self) {
        self.current_block_scopes.clear();
    }
    /// Initializes a variable for the first time
    #[inline]
    pub fn initialize_var(
        &mut self,
        relative_scope_index: usize,
        variable_index: usize,
        value: Value,
        is_mutable: bool,
    ) {
        if is_mutable {
            // TODO: Check that we are not overwriting an existing used variables (debug)

            self.current_block_scopes[relative_scope_index].set(
                variable_index,
                VariableValue::Reference(ValueReference(Rc::new(RefCell::new(value)))),
            );
        } else {
            // If it is immutable, just store normal values
            self.current_block_scopes[relative_scope_index]
                .set(variable_index, VariableValue::Value(value));
        }
    }

    /// Initializes a variable for the first time
    #[inline]
    pub fn initialize_var_mut(&mut self, variable: &ResolvedVariableRef, value_ref: ValueRef) {
        // TODO: Check that we are not overwriting an existing used variables (debug)
        self.current_block_scopes[variable.scope_index].set(
            variable.variable_index,
            VariableValue::Reference(ValueReference(value_ref)),
        );
    }

    #[inline]
    fn set_local_var(&mut self, variable_index: usize, value: VariableValue) {
        let last_scope_index = self.current_block_scopes.len() - 1;

        self.current_block_scopes[last_scope_index].set(variable_index, value);
    }

    #[inline]
    pub fn lookup_var(&self, relative_scope_index: usize, variable_index: usize) -> &VariableValue {
        if relative_scope_index >= self.current_block_scopes.len() {
            panic!(
                "illegal scope index {relative_scope_index} of {}",
                self.current_block_scopes.len()
            );
        }

        let variables = &self.current_block_scopes[relative_scope_index].variables;
        if variable_index >= variables.len() {
            panic!("illegal index");
        }
        &variables[variable_index]
    }

    // ------------------

    #[inline]
    pub fn lookup_var_value(&self, variable: &ResolvedVariableRef) -> Value {
        let x = self.lookup_var(variable.scope_index, variable.variable_index);
        match x {
            VariableValue::Value(v) => v.clone(),
            VariableValue::Reference(var_ref) => var_ref.0.borrow().clone(),
        }
    }

    #[inline]
    pub fn lookup_variable(&self, variable: &ResolvedVariableRef) -> &VariableValue {
        self.lookup_var(variable.scope_index, variable.variable_index)
    }

    // MutRef and Overwrite ============================

    #[inline]
    pub fn lookup_variable_mut_ref(
        &self,
        variable: &ResolvedVariableRef,
    ) -> Result<&ValueRef, ExecuteError> {
        let complete_var = self.lookup_var(variable.scope_index, variable.variable_index);
        match complete_var {
            VariableValue::Value(_) => Err(ExecuteError::VariableWasNotMutable),
            VariableValue::Reference(reference) => Ok(&reference.0),
        }
    }

    #[inline]
    fn lookup_mut_var(
        &self,
        relative_scope_index: usize,
        variable_index: usize,
    ) -> Result<Rc<RefCell<Value>>, ExecuteError> {
        if relative_scope_index >= self.current_block_scopes.len() {
            panic!(
                "illegal scope index {relative_scope_index} of {}",
                self.current_block_scopes.len()
            );
        }

        let variables = &self.current_block_scopes[relative_scope_index].variables;
        if variable_index >= variables.len() {
            panic!("illegal index");
        }
        let existing_var = &variables[variable_index];

        Ok(match existing_var {
            VariableValue::Reference(reference) => reference.0.clone(),
            _ => {
                error!(?existing_var, "was not mutable");

                return Err(ExecuteError::VariableWasNotMutable);
            }
        })
    }

    #[inline]
    pub fn lookup_mut_variable(
        &self,
        variable: &ResolvedVariableRef,
    ) -> Result<Rc<RefCell<Value>>, ExecuteError> {
        self.lookup_mut_var(variable.scope_index, variable.variable_index)
    }

    // Overwrite ============================
    #[inline]
    pub fn set_local_var_value(&mut self, var: &ResolvedVariableRef, value: Value) {
        self.set_local_var(var.variable_index, VariableValue::Value(value));
    }

    #[inline]
    pub fn overwrite_existing_var(
        &mut self,
        relative_scope_index: usize,
        variable_index: usize,
        new_value: Value,
    ) -> Result<(), ExecuteError> {
        let existing_var = &mut self.current_block_scopes[relative_scope_index].get(variable_index);

        match existing_var {
            VariableValue::Reference(r) => {
                r.set(new_value);
                Ok(())
            }
            _ => Err(format!("Cannot assign to immutable variable: {variable_index:?}",).into()),
        }
    }

    #[inline]
    pub fn overwrite_existing_var_ref(
        &mut self,
        relative_scope_index: usize,
        variable_index: usize,
        new_value_ref: ValueRef,
    ) -> Result<(), ExecuteError> {
        let existing_var = &mut self.current_block_scopes[relative_scope_index].get(variable_index);

        match existing_var {
            VariableValue::Reference(_) => {
                *existing_var = &VariableValue::Reference(ValueReference(new_value_ref));
                Ok(())
            }
            _ => Err(format!("Cannot assign to immutable variable: {variable_index:?}",).into()),
        }
    }

    #[inline]
    pub fn overwrite_existing_var_mem(
        &mut self,
        variable: &ResolvedVariableRef,
        variable_value: VariableValue,
    ) -> Result<(), ExecuteError> {
        match variable_value {
            VariableValue::Reference(reference) => self.overwrite_existing_var_ref(
                variable.scope_index,
                variable.variable_index,
                reference.0,
            ),
            VariableValue::Value(value) => {
                self.overwrite_existing_var(variable.scope_index, variable.variable_index, value)
            }
        }
    }

    pub(crate) fn initialize_var_mem(
        &mut self,
        init_var: &ResolvedVariableRef,
        source_memory: VariableValue,
    ) -> Result<(), ExecuteError> {
        let is_mutable = init_var.is_mutable();
        match &source_memory {
            VariableValue::Value(normal_value) => {
                if is_mutable {
                    self.current_block_scopes[init_var.scope_index].set(
                        init_var.variable_index,
                        VariableValue::Reference(ValueReference(Rc::new(RefCell::new(
                            normal_value.clone(),
                        )))),
                    );
                } else {
                    self.current_block_scopes[init_var.scope_index]
                        .set(init_var.variable_index, source_memory);
                }
            }
            VariableValue::Reference(reference) => {
                if is_mutable {
                    self.current_block_scopes[init_var.scope_index].set(
                        init_var.variable_index,
                        VariableValue::Reference(reference.clone()),
                    );
                } else {
                    self.current_block_scopes[init_var.scope_index].set(
                        init_var.variable_index,
                        VariableValue::Value(reference.0.borrow().clone()),
                    );
                }
            }
        }

        Ok(())
    }

    #[inline]
    pub fn set_local_var_ex(
        &mut self,
        variable_index: usize,
        memory_value: VariableValue,
        check_is_mut: bool,
    ) -> Result<(), ExecuteError> {
        match memory_value {
            VariableValue::Value(_) => {
                if check_is_mut {
                    return Err(ExecuteError::VariableWasNotMutable);
                }
            }
            VariableValue::Reference(_) => {}
        }
        self.set_local_var(variable_index, memory_value);
        Ok(())
    }
}
