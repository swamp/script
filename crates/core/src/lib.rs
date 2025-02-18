use swamp_script_modules::symtbl::SymbolTable;
use swamp_script_semantic::Node;
use swamp_script_semantic::Type;
use swamp_script_semantic::{
    AliasType, IntrinsicFunction, IntrinsicFunctionDefinition, Signature, TypeForParameter,
};

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

pub fn add_intrinsic_functions(core_ns: &mut SymbolTable) {
    /*
        pub struct TypeForParameter {
        pub name: String,
        pub resolved_type: Type,
        pub is_mutable: bool,
        pub node: Option<ParameterNode>,
    }
         */
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
