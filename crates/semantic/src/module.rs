/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::ns::ResolvedModuleNamespace;
use std::rc::Rc;
use swamp_script_ast::LocalIdentifier;

#[derive(Debug)]
pub struct Module {
    pub namespace: ResolvedModuleNamespace, // pub is probably more performant
    parent_module: Option<Rc<Module>>,
    identifier: LocalIdentifier,
}

impl Module {
    pub fn new(
        parent_module: Option<Rc<Module>>,
        identifier: LocalIdentifier,
        namespace: ResolvedModuleNamespace,
    ) -> Self {
        Self {
            namespace,
            identifier,
            parent_module,
        }
    }

    pub fn namespace(&self) -> &ResolvedModuleNamespace {
        &self.namespace
    }

    pub fn name(&self) -> String {
        if let Some(parent) = &self.parent_module {
            parent.name().to_string() + "." + &*self.identifier.text
        } else {
            self.identifier.text.to_string()
        }
    }
}
