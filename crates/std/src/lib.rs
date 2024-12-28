pub mod collections;
pub mod prelude;

use swamp_script_semantic::{modules::ResolvedModule, ResolvedRustType, TypeNumber};
pub const SPARSE_TYPE_ID: TypeNumber = 999;

#[must_use]
pub fn create_std_module() -> ResolvedModule {
    let mut std_module = ResolvedModule::new(&["std".to_string()]);
    let sparse_rust_type = ResolvedRustType {
        type_name: "Sparse".to_string(),
        number: SPARSE_TYPE_ID,
    };

    std_module
        .namespace
        .borrow_mut()
        .add_built_in_rust_type(sparse_rust_type)
        .expect("could not register Sparse type");

    std_module
}
