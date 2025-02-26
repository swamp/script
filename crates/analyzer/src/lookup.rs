/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use seq_map::SeqMap;
use swamp_script_semantic::Type;
use tracing::info;

#[derive(Debug)]
pub struct TypeParameter {
    pub ty: Type,
    pub debug_name: String,
}
