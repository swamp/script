/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::RuntimeErrorKind;
use crate::prelude::RuntimeError;
use crate::prelude::VariableValue;
use source_map_node::Node;
use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;
use swamp_core_extra::prelude::Value;
use swamp_core_extra::value::ValueRef;
use swamp_semantic::VariableRef;

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

impl Display for BlockScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        for (index, variable_value) in self.variables.iter().enumerate().take(4) {
            writeln!(f, "  var({}): {}", index, variable_value.to_value())?;
        }
        Ok(())
    }
}

impl BlockScope {
    #[inline(always)]
    fn set(&mut self, variable_index: usize, value: VariableValue) {
        self.variables[variable_index] = value;
    }

    #[inline(always)]
    #[allow(unused)]
    fn get(&self, variable_index: usize) -> &VariableValue {
        &self.variables[variable_index]
    }
}

#[derive(Clone)]
pub struct BlockScopes {
    current_block_scopes: Vec<BlockScope>,
}

impl Default for BlockScopes {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for BlockScopes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        for (index, scope) in self.current_block_scopes.iter().enumerate() {
            writeln!(f, "block({index}):\n{scope}")?;
        }
        Ok(())
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
                VariableValue::Reference(Rc::new(RefCell::new(value))),
            );
        } else {
            // If it is immutable, just store normal values
            self.current_block_scopes[relative_scope_index]
                .set(variable_index, VariableValue::Value(value));
        }
    }

    #[inline]
    pub fn init_var_ref(&mut self, variable: &VariableRef, value_ref: &ValueRef) {
        if variable.is_mutable() {
            // TODO: Check that we are not overwriting an existing used variables (debug)
            self.current_block_scopes[variable.scope_index].set(
                variable.variable_index,
                VariableValue::Reference(value_ref.clone()),
            );
        } else {
            // If it is immutable, just store normal values
            self.current_block_scopes[variable.scope_index].set(
                variable.variable_index,
                VariableValue::Value(value_ref.borrow().clone()),
            );
        }
    }

    /// Initializes a variable for the first time
    #[inline]
    pub fn initialize_var_mut(&mut self, variable: &VariableRef, value_ref: ValueRef) {
        // TODO: Check that we are not overwriting an existing used variables (debug)
        self.current_block_scopes[variable.scope_index]
            .set(variable.variable_index, VariableValue::Reference(value_ref));
    }

    #[inline]
    fn set_local_var(&mut self, variable_index: usize, value: VariableValue) {
        let last_scope_index = self.current_block_scopes.len() - 1;

        self.current_block_scopes[last_scope_index].set(variable_index, value);
    }

    #[inline]
    pub fn lookup_var(&self, relative_scope_index: usize, variable_index: usize) -> &VariableValue {
        debug_assert!(
            relative_scope_index < self.current_block_scopes.len(),
            "illegal scope index {relative_scope_index} of {}",
            self.current_block_scopes.len()
        );

        let variables = &self.current_block_scopes[relative_scope_index].variables;
        debug_assert!(variable_index < variables.len(), "illegal index");
        &variables[variable_index]
    }

    pub fn get_var(&self, var: &VariableRef) -> &VariableValue {
        self.lookup_var(var.scope_index, var.variable_index)
    }

    // ------------------

    #[inline]
    pub fn lookup_var_value(&self, variable: &VariableRef) -> Value {
        let x = self.lookup_var(variable.scope_index, variable.variable_index);
        match x {
            VariableValue::Value(v) => v.clone(),
            VariableValue::Reference(var_ref) => var_ref.borrow().clone(),
        }
    }

    /*
    #[inline]
    pub fn lookup_variable(&self, variable: &VariableRef) -> &VariableValue {
        self.lookup_var(variable.scope_index, variable.variable_index)
    }

     */

    // MutRef and Overwrite ============================

    #[inline]
    pub fn lookup_variable_mut_ref(
        &self,
        variable: &VariableRef,
    ) -> Result<&ValueRef, RuntimeError> {
        let complete_var = self.lookup_var(variable.scope_index, variable.variable_index);
        match complete_var {
            VariableValue::Value(_) => Err(RuntimeError {
                kind: RuntimeErrorKind::VariableWasNotMutable,
                node: variable.name.clone(),
            }),
            VariableValue::Reference(reference) => Ok(reference),
        }
    }

    pub(crate) fn initialize_var_mem(
        &mut self,
        init_var: &VariableRef,
        source_memory: VariableValue,
    ) -> Result<(), RuntimeError> {
        let is_mutable = init_var.is_mutable();
        match &source_memory {
            VariableValue::Value(normal_value) => {
                if is_mutable {
                    self.current_block_scopes[init_var.scope_index].set(
                        init_var.variable_index,
                        VariableValue::Reference(Rc::new(RefCell::new(normal_value.clone()))),
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
                        VariableValue::Value(reference.borrow().clone()),
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
    ) -> Result<(), RuntimeError> {
        match memory_value {
            VariableValue::Value(_) => {
                if check_is_mut {
                    return Err(RuntimeError {
                        kind: RuntimeErrorKind::VariableWasNotMutable,
                        node: Node::new_unknown(),
                    });
                }
            }
            VariableValue::Reference(_) => {}
        }
        self.set_local_var(variable_index, memory_value);
        Ok(())
    }

    #[inline]
    pub fn overwrite_existing_var_ref(
        &mut self,
        variable: &VariableRef,
        new_value_ref: ValueRef,
    ) -> Result<(), RuntimeError> {
        let existing_var =
            &mut self.current_block_scopes[variable.scope_index].get(variable.variable_index);

        match existing_var {
            VariableValue::Reference(_) => {
                *existing_var = &VariableValue::Reference(new_value_ref);
                Ok(())
            }
            _ => Err(RuntimeError {
                kind: RuntimeErrorKind::VariableWasNotMutable,
                node: Node::new_unknown(),
            }),
        }
    }

    #[inline]
    pub fn overwrite_existing_var(
        &mut self,
        variable: &VariableRef,
        new_value: Value,
    ) -> Result<(), RuntimeError> {
        let existing_var =
            &mut self.current_block_scopes[variable.scope_index].get(variable.variable_index);

        match existing_var {
            VariableValue::Reference(r) => {
                *r.borrow_mut() = new_value;
                Ok(())
            }
            _ => Err(RuntimeError {
                kind: RuntimeErrorKind::VariableWasNotMutable,
                node: Node::new_unknown(),
            }),
        }
    }

    #[inline]
    pub fn overwrite_existing_var_mem(
        &mut self,
        variable: &VariableRef,
        variable_value: VariableValue,
    ) -> Result<(), RuntimeError> {
        match variable_value {
            VariableValue::Reference(reference) => {
                self.overwrite_existing_var_ref(variable, reference)
            }
            VariableValue::Value(value) => self.overwrite_existing_var(variable, value),
        }
    }
}
