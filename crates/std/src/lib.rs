/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

pub mod collections;
pub mod prelude;

use swamp_script_semantic::{
    create_rust_type, modules::ResolvedModule, ResolvedRustType, TypeNumber,
};
pub const SPARSE_TYPE_ID: TypeNumber = 999;
pub const SPARSE_ID_TYPE_ID: TypeNumber = 998;

#[must_use]
pub fn create_std_module() -> ResolvedModule {
    let std_module = ResolvedModule::new(&["std".to_string()]);

    {
        let mut borrowed_namespace = std_module.namespace.borrow_mut();

        let sparse_rust_type = ResolvedRustType {
            type_name: "Sparse".to_string(),
            number: SPARSE_TYPE_ID,
        };
        borrowed_namespace
            .add_built_in_rust_type(sparse_rust_type)
            .expect("could not register Sparse type");

        let rust_type_ref_for_id = ResolvedRustType {
            type_name: "SparseId".to_string(),
            number: SPARSE_ID_TYPE_ID,
        };
        borrowed_namespace.add_built_in_rust_type(rust_type_ref_for_id);
    }

    std_module
}
