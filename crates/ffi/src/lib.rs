use swamp_script_modules::modules::Module;
use swamp_script_modules::symtbl::SymbolTable;
use tiny_ver::TinyVersion;

pub const PACKAGE_NAME: &str = "ffi";

/// # Panics
/// if `versioned_name` is wrong
#[must_use]
pub fn create_module(tiny_version: &TinyVersion) -> Module {
    let intrinsic_types_symbol_table = SymbolTable::new();
    let canonical_core_path = [tiny_version.versioned_name(PACKAGE_NAME).unwrap()];

    Module::new(&canonical_core_path, intrinsic_types_symbol_table, None)
}
