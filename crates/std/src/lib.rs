/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/swamp
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod collections;
pub mod prelude;

/*
pub const SPARSE_TYPE_ID: TypeNumber = 999;
pub const SPARSE_ID_TYPE_ID: TypeNumber = 998;

#[must_use]
pub fn create_std_module() -> Module {
    let mut symbol_table = SymbolTable::new();

    {
        let sparse_rust_type = ExternalType {
            type_name: "Sparse".to_string(),
            number: SPARSE_TYPE_ID,
        };
        symbol_table
            .add_external_type(sparse_rust_type)
            .expect("could not register Sparse type");

        let rust_type_ref_for_id = ExternalType {
            type_name: "SparseId".to_string(),
            number: SPARSE_ID_TYPE_ID,
        };
        symbol_table
            .add_external_type(rust_type_ref_for_id)
            .expect("could not register SparseId type");
    }

    {
        let sparse_rust_type = ExternalType {
            type_name: "Grid".to_string(),
            number: SPARSE_TYPE_ID,
        };
        symbol_table
            .add_external_type(sparse_rust_type)
            .expect("could not register Grid type");
    }

    let std_module = Module::new(&["std".to_string()], symbol_table, None);

    std_module
}
*/
