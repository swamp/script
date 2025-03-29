/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use seq_fmt::comma;
use seq_map::{SeqMap, SeqMapError};
use swamp_types::Type;

/// It is not a proper Monomorphization cache, but similar
#[derive(Debug, Clone)]
pub struct InstantiationCache {
    pub cache: SeqMap<String, Type>,
}

impl InstantiationCache {
    pub fn new() -> Self {
        Self {
            cache: SeqMap::default(),
        }
    }

    pub fn complete_name(path: &[String], base_name: &str, argument_types: &[Type]) -> String {
        format!(
            "{}::{}<{}>",
            path.join("::"),
            base_name,
            comma(argument_types)
        )
    }

    pub fn add(
        &mut self,
        path: &[String],
        name: &str,
        ty: Type,
        argument_type: &[Type],
    ) -> Result<(), SeqMapError> {
        if let Type::Blueprint(_) = ty {
            panic!("can not add blueprint to cache");
        }
        let converted_name = Self::complete_name(path, name, argument_type);
        self.cache.insert(converted_name, ty)
    }

    pub fn is_empty(&self) -> bool {
        self.cache.is_empty()
    }

    pub fn get(&self, path: &[String], base_name: &str, argument_type: &[Type]) -> Option<&Type> {
        let name = Self::complete_name(path, base_name, argument_type);

        self.cache.get(&name)
    }
}
