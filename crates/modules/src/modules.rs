/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::symtbl::{SymbolTable, SymbolTableRef};
use seq_map::SeqMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use swamp_script_semantic::{Constant, ConstantId, ConstantRef, Expression};

#[derive(Debug)]
pub struct Modules {
    pub modules: SeqMap<Vec<String>, ModuleRef>,
    pub constants: Vec<ConstantRef>,
    pub auto_use: Vec<SymbolTableRef>,
}

impl Default for Modules {
    fn default() -> Self {
        Self::new()
    }
}

type NamespacePath = Vec<String>;

#[derive(Debug)]
pub struct Namespace {
    pub path: NamespacePath,
    pub symbol_table: SymbolTableRef,
}

impl Namespace {
    pub fn new(path: NamespacePath, symbol_table: SymbolTable) -> Self {
        Self {
            path,
            symbol_table: Rc::new(symbol_table),
        }
    }
}

#[derive(Debug)]
pub struct NamespaceRegistry {
    pub namespaces: SeqMap<NamespacePath, Namespace>,
}

pub struct Module {
    pub expression: Option<Expression>,
    pub namespace: Namespace,
}

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, " {:?}", self.namespace)?;

        Ok(())
    }
}

pub type ModuleRef = Rc<Module>;

impl Module {
    #[must_use]
    pub fn new(
        module_path: &[String],
        symbol_table: SymbolTable,
        expression: Option<Expression>,
    ) -> Self {
        Self {
            namespace: Namespace::new(NamespacePath::from(module_path), symbol_table),
            expression,
        }
    }
}

impl Modules {
    pub fn new() -> Self {
        Self {
            modules: SeqMap::new(),
            constants: Vec::new(),
            auto_use: Vec::new(),
        }
    }
    pub fn add(&mut self, module: ModuleRef) {
        self.modules
            .insert(module.clone().namespace.path.clone(), module)
            .expect("todo");
    }

    pub fn link_module(&mut self, module_path: &[String], referred_module: ModuleRef) {
        self.modules
            .insert(module_path.to_vec(), referred_module)
            .expect("todo");
    }

    pub fn add_constant(&mut self, resolved_constant: Constant) -> ConstantRef {
        let id = self.constants.len();
        let mut copy = resolved_constant;
        copy.id = id as ConstantId;
        let constant_ref = Rc::new(copy);
        self.constants.push(constant_ref.clone());

        constant_ref
    }

    #[must_use]
    pub fn contains_key(&self, module_path: &[String]) -> bool {
        self.modules.contains_key(&module_path.to_vec())
    }

    #[must_use]
    pub fn get(&self, module_path: &[String]) -> Option<&ModuleRef> {
        self.modules.get(&module_path.to_vec())
    }
}
