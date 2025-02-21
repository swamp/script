use std::rc::Rc;
use swamp_script_modules::modules::Module;
use swamp_script_modules::symtbl::{Symbol, SymbolTable};
use swamp_script_semantic::{ExternalType, TYPE_NUMBER_FFI_VALUE, Type};
use tiny_ver::TinyVersion;

pub const PACKAGE_NAME: &str = "ffi";

fn add_intrinsic_types(ffi_ns: &mut SymbolTable) {}

/// # Panics
/// if `versioned_name` is wrong
#[must_use]
pub fn create_module(tiny_version: &TinyVersion) -> Module {
    let mut intrinsic_types_symbol_table = SymbolTable::new();
    let canonical_core_path = [tiny_version.versioned_name(PACKAGE_NAME).unwrap()];
    add_intrinsic_types(&mut intrinsic_types_symbol_table);

    Module::new(&canonical_core_path, intrinsic_types_symbol_table, None)
}
