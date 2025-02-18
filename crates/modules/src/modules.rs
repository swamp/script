/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::symtbl::Symbol;
use crate::symtbl::{SymbolTable, SymbolTableRef};
use seq_map::SeqMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use swamp_script_semantic::{Constant, ConstantId, ConstantRef, Expression, ExpressionKind};

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

        if let Some(resolved_expression) = &self.expression {
            pretty_print(f, resolved_expression, 0)?;
        }

        Ok(())
    }
}

pub type ModuleRef = Rc<Module>;

/// # Errors
///
pub fn pretty_print(
    f: &mut Formatter<'_>,
    resolved_expression: &Expression,
    tabs: usize,
) -> std::fmt::Result {
    if let ExpressionKind::Block(expressions) = &resolved_expression.kind {
        for internal_expr in expressions {
            pretty_print(f, internal_expr, tabs + 1)?;
        }
        Ok(())
    } else {
        let tab_str = "..".repeat(tabs);
        writeln!(
            f,
            "{}{},{:?}",
            tab_str, resolved_expression.ty, resolved_expression.kind
        )
    }
}

pub fn pretty_print_symbol(
    f: &mut Formatter<'_>,
    name: &str,
    symbol: &Symbol,
    tabs: usize,
) -> std::fmt::Result {
    let tab_str = "..".repeat(tabs);

    write!(f, "{}{}: ", tab_str, name)?;

    match symbol {
        Symbol::Type(ty) => {
            write!(f, "{ty:?}")
        }
        Symbol::Module(module_ref) => {
            write!(f, "{module_ref:?}")
        }
        Symbol::Constant(constant_ref) => {
            write!(f, "{constant_ref:?}")
        }
        Symbol::FunctionDefinition(func_def) => {
            write!(f, "{func_def:?}")
        }
        Symbol::Alias(alias_type_ref) => {
            write!(f, "{alias_type_ref:?}")
        }
        Symbol::Generic(generic_type_ref) => {
            write!(f, "{generic_type_ref:?}")
        }
    }
}

/// # Errors
///
pub fn pretty_print_symbol_table(
    f: &mut Formatter<'_>,
    symbol_table: &SymbolTable,
    tabs: usize,
) -> std::fmt::Result {
    for (name, symbol) in symbol_table.symbols() {
        pretty_print_symbol(f, name, symbol, tabs)?;
        writeln!(f)?;
    }
    Ok(())
}

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
