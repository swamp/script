/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::ns::{ModuleNamespace, ModuleNamespaceRef};
use seq_map::SeqMap;
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use swamp_script_semantic::{
    AssociatedImpls, Constant, ConstantId, ConstantRef, Expression, ExpressionKind,
};

#[derive(Debug)]
pub struct Modules {
    pub modules: SeqMap<Vec<String>, ModuleRef>,
    pub constants: Vec<ConstantRef>,
    pub auto_use: Vec<ModuleNamespaceRef>,
    pub associated_functions: AssociatedImpls,
}

impl Default for Modules {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Module {
    //    pub definitions: Vec<Definition>,
    pub expression: Option<Expression>,
    pub namespace: ModuleNamespaceRef,
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

pub type ModuleRef = Rc<RefCell<Module>>;

impl Module {
    pub fn new(module_path: &[String]) -> Self {
        let ns_ref = Rc::new(RefCell::new(ModuleNamespace::new(module_path)));
        Self {
            namespace: ns_ref,
            expression: None,
        }
    }
}

impl Modules {
    pub fn new() -> Self {
        Self {
            modules: SeqMap::new(),
            constants: Vec::new(),
            auto_use: Vec::new(),
            associated_functions: AssociatedImpls::new(),
        }
    }
    pub fn add(&mut self, module: ModuleRef) {
        self.modules
            .insert(
                module.clone().borrow().namespace.borrow().path.clone(),
                module,
            )
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

    pub fn add_empty_module(&mut self, module_path: &[String]) -> ModuleRef {
        let ns_ref = Rc::new(RefCell::new(ModuleNamespace::new(module_path)));
        let module = Module {
            expression: None,
            namespace: ns_ref,
        };
        let module_ref = Rc::new(RefCell::new(module));

        self.modules
            .insert(Vec::from(module_path), module_ref.clone())
            .expect("todo");

        module_ref
    }

    #[must_use]
    pub fn contains_key(&self, module_path: &[String]) -> bool {
        self.modules.contains_key(&module_path.to_vec())
    }

    #[must_use]
    pub fn get(&self, module_path: &[String]) -> Option<ModuleRef> {
        self.modules.get(&module_path.to_vec()).cloned()
    }
}
