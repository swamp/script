use swamp_script_modules::modules::Module;
use swamp_script_modules::symtbl::SymbolTable;
use swamp_script_semantic::Node;
use swamp_script_semantic::Type;
use swamp_script_semantic::{
    AliasType, IntrinsicFunction, IntrinsicFunctionDefinition, Signature, TypeForParameter,
};
use tiny_ver::TinyVersion;
pub const PACKAGE_NAME: &str = "core";
fn add_intrinsic_types(core_ns: &mut SymbolTable) {
    let int_alias = AliasType {
        name: Node::default(),
        assigned_name: "Int".to_string(),
        referenced_type: Type::Int,
    };
    core_ns.add_alias(int_alias).unwrap();

    let float_alias = AliasType {
        name: Node::default(),
        assigned_name: "Float".to_string(),
        referenced_type: Type::Float,
    };
    core_ns.add_alias(float_alias).unwrap();

    let string_alias = AliasType {
        name: Node::default(),
        assigned_name: "String".to_string(),
        referenced_type: Type::String,
    };
    core_ns.add_alias(string_alias).unwrap();

    let bool_alias = AliasType {
        name: Node::default(),
        assigned_name: "Bool".to_string(),
        referenced_type: Type::Bool,
    };
    core_ns.add_alias(bool_alias).unwrap();
}

fn add_intrinsic_functions(core_ns: &mut SymbolTable) {
    let float_to_float = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: Type::Float,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Float),
    };

    core_ns
        .add_intrinsic_function(IntrinsicFunctionDefinition {
            name: "float_round".into(),
            intrinsic: IntrinsicFunction::FloatRound,
            signature: float_to_float.clone(),
        })
        .unwrap();

    core_ns
        .add_intrinsic_function(IntrinsicFunctionDefinition {
            name: "float_floor".into(),
            intrinsic: IntrinsicFunction::FloatFloor,
            signature: float_to_float,
        })
        .unwrap();
}

/// # Panics
/// if `versioned_name` is wrong
#[must_use]
pub fn create_module(tiny_version: &TinyVersion) -> Module {
    let mut intrinsic_types_symbol_table = SymbolTable::new();
    let canonical_core_path = [tiny_version.versioned_name(PACKAGE_NAME).unwrap()];
    add_intrinsic_types(&mut intrinsic_types_symbol_table);
    add_intrinsic_functions(&mut intrinsic_types_symbol_table);

    Module::new(&canonical_core_path, intrinsic_types_symbol_table, None)
}
