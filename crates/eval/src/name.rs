/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DefinitionKind {
    Struct,
    Enum,
    Function,
    Variable,
    ImplMethod(String), // String is the type name
}

#[derive(Default, Debug)]
pub struct NameRegistry {
    names: HashMap<String, DefinitionKind>,
}

impl NameRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register(&mut self, name: &str, kind: DefinitionKind) -> Result<(), String> {
        if let Some(existing) = self.names.get(name) {
            Err(format!(
                "Name conflict: '{}' is already defined as a {}",
                name,
                match existing {
                    DefinitionKind::Struct => "struct",
                    DefinitionKind::Enum => "enum",
                    DefinitionKind::Function => "function",
                    DefinitionKind::Variable => "variable",
                    DefinitionKind::ImplMethod(type_name) =>
                        return Err(format!("method for type '{}'", type_name)),
                }
            ))
        } else {
            self.names.insert(name.to_string(), kind);
            Ok(())
        }
    }

    pub fn contains(&self, name: &str) -> bool {
        self.names.contains_key(name)
    }

    pub fn get_definition(&self, name: &str) -> Option<&DefinitionKind> {
        self.names.get(name)
    }
}
