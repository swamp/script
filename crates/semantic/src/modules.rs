/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::ns::Namespace;
use crate::ns::NamespacePath;
use crate::symtbl::SymbolTable;
use crate::{Constant, ConstantId, ConstantRef, Expression, ExpressionKind};
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

#[derive(Debug)]
pub struct Modules {
    pub modules: HashMap<Vec<String>, ModuleRef>,
    pub constants: Vec<ConstantRef>,
}

impl Default for Modules {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Module {
    pub namespace: Namespace,
    pub expression: Option<Expression>,
}

impl Debug for Module {
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

pub type ModuleRef = Rc<Module>;

impl Module {
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
            modules: HashMap::new(),
            constants: Vec::new(),
        }
    }

    #[must_use]
    pub fn contains_key(&self, module_path: &[String]) -> bool {
        self.modules.contains_key(module_path)
    }

    #[must_use]
    pub fn get(&self, module_path: &[String]) -> Option<&ModuleRef> {
        self.modules.get(&module_path.to_vec())
    }
}
