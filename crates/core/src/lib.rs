use swamp_script_modules::symtbl::SymbolTable;
use swamp_script_semantic::AliasType;
use swamp_script_semantic::Node;
use swamp_script_semantic::Type;

pub fn add_intrinsic_types(core_ns: &mut SymbolTable) {
    let int_alias = AliasType {
        name: Node::default(),
        assigned_name: "Int".to_string(),
        referenced_type: Type::Int,
    };
    core_ns.add_alias(int_alias).expect("TODO: panic message");

    let float_alias = AliasType {
        name: Node::default(),
        assigned_name: "Float".to_string(),
        referenced_type: Type::Float,
    };
    core_ns.add_alias(float_alias).expect("TODO: panic message");

    let string_alias = AliasType {
        name: Node::default(),
        assigned_name: "String".to_string(),
        referenced_type: Type::String,
    };
    core_ns
        .add_alias(string_alias)
        .expect("TODO: panic message");

    let bool_alias = AliasType {
        name: Node::default(),
        assigned_name: "Bool".to_string(),
        referenced_type: Type::Bool,
    };
    core_ns.add_alias(bool_alias).expect("TODO: panic message");
}
