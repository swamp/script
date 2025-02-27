/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::ns::NamespacePath;
use crate::ns::ResolvedModuleNamespace;
use crate::symtbl::SymbolTable;
use crate::ResolvedExpressionKind;
use crate::{ConstantId, ResolvedConstant, ResolvedConstantRef, ResolvedExpression};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

#[derive(Debug)]
pub struct ResolvedModules {
    pub modules: HashMap<Vec<String>, ResolvedModuleRef>,
    pub constants: Vec<ResolvedConstantRef>,
}

impl Default for ResolvedModules {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ResolvedModule {
    pub namespace: ResolvedModuleNamespace,
    pub expression: Option<ResolvedExpression>,
}

impl Debug for ResolvedModule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(resolved_expression) = &self.expression {
            pretty_print(f, resolved_expression, 0)?;
        }

        Ok(())
    }
}

/// # Errors
///
pub fn pretty_print(
    f: &mut Formatter<'_>,
    resolved_expression: &ResolvedExpression,
    tabs: usize,
) -> std::fmt::Result {
    if let ResolvedExpressionKind::Block(expressions) = &resolved_expression.kind {
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

pub type ResolvedModuleRef = Rc<ResolvedModule>;

impl ResolvedModule {
    pub fn new(
        module_path: &[String],
        symbol_table: SymbolTable,
        expression: Option<ResolvedExpression>,
    ) -> Self {
        Self {
            namespace: ResolvedModuleNamespace::new(NamespacePath::from(module_path), symbol_table),
            expression,
        }
    }
}

impl ResolvedModules {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            constants: Vec::new(),
        }
    }
    pub fn add(&mut self, module: ResolvedModuleRef) {
        self.modules
            .insert(module.clone().namespace.path.clone(), module);
    }

    pub fn add_constant(&mut self, resolved_constant: ResolvedConstant) -> ResolvedConstantRef {
        let id = self.constants.len();
        let mut copy = resolved_constant;
        copy.id = id as ConstantId;
        let constant_ref = Rc::new(copy);
        self.constants.push(constant_ref.clone());

        constant_ref
    }

    #[must_use]
    pub fn contains_key(&self, module_path: &[String]) -> bool {
        self.modules.contains_key(module_path)
    }

    #[must_use]
    pub fn get(&self, module_path: &[String]) -> Option<ResolvedModuleRef> {
        self.modules.get(module_path).cloned()
    }
}
