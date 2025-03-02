use std::rc::Rc;
use swamp_script_modules::modules::Module;
use swamp_script_modules::symtbl::{GeneratorKind, Symbol, SymbolTable, TypeGenerator};
use swamp_script_semantic::Type;
use swamp_script_semantic::{
    AliasType, IntrinsicFunction, IntrinsicFunctionDefinition, Signature, TypeForParameter,
};
use swamp_script_semantic::{ExternalType, Node, TYPE_NUMBER_FFI_VALUE};
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

    let value_type = ExternalType {
        type_name: "Value".to_string(),
        number: TYPE_NUMBER_FFI_VALUE,
    };
    let value_type = Symbol::Type(Type::External(Rc::new(value_type)));
    core_ns.add_symbol("Value", value_type).unwrap();

    let slice_type_generator = TypeGenerator {
        kind: GeneratorKind::Slice,
        arity: 1,
    };

    core_ns
        .add_type_generator("Slice", slice_type_generator)
        .unwrap();

    let slice_type_generator = TypeGenerator {
        kind: GeneratorKind::SlicePair,
        arity: 2,
    };

    core_ns
        .add_type_generator("SlicePair", slice_type_generator)
        .unwrap();
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_functions(core_ns: &mut SymbolTable) {
    add_intrinsic_float_functions(core_ns);
    add_intrinsic_int_functions(core_ns);
    add_intrinsic_string_functions(core_ns);
    let value_type = core_ns.get_type("Value").unwrap().clone();
    add_intrinsic_vec_functions(core_ns, &value_type);
    add_intrinsic_map_functions(core_ns, &value_type);
    add_intrinsic_sparse_functions(core_ns, &value_type);
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_sparse_functions(core_ns: &mut SymbolTable, value_type: &Type) {
    let slice_to_self = Signature {
        parameters: [TypeForParameter {
            name: "slice".to_string(),
            resolved_type: Type::Slice(Box::from(value_type.clone())),
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(value_type.clone()),
    };
    let slice_to_self_functions = [IntrinsicFunction::SparseFromSlice];
    for intrinsic_fn in slice_to_self_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: slice_to_self.clone(),
            })
            .unwrap();
    }

    let unit_to_value = Signature {
        parameters: [].into(),
        return_type: Box::new(value_type.clone()),
    };

    let unit_to_value_functions = [IntrinsicFunction::SparseCreate];

    for intrinsic_fn in unit_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: unit_to_value.clone(),
            })
            .unwrap();
    }

    let self_to_value = Signature {
        parameters: [TypeForParameter {
            name: "self".to_string(),
            resolved_type: value_type.clone(),
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(value_type.clone()),
    };

    let self_to_value_functions = [
        IntrinsicFunction::SparseIter,
        IntrinsicFunction::SparseIterMut,
    ];

    for intrinsic_fn in self_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_to_value.clone(),
            })
            .unwrap();
    }

    let self_value_to_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(value_type.clone()),
    };

    let self_value_to_value_functions = [
        IntrinsicFunction::SparseSubscript,
        IntrinsicFunction::SparseAdd,
    ];

    for intrinsic_fn in self_value_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_to_value.clone(),
            })
            .unwrap();
    }

    let self_value_value_mut_to_unit = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "key".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "value".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: true,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(value_type.clone()),
    };

    let self_value_value_mut_to_unit_functions = [IntrinsicFunction::SparseSubscriptMut];

    for intrinsic_fn in self_value_value_mut_to_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_value_mut_to_unit.clone(),
            })
            .unwrap();
    }

    let self_value_to_bool = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Bool),
    };

    let self_value_to_bool_functions = [IntrinsicFunction::SparseHas];

    for intrinsic_fn in self_value_to_bool_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_to_bool.clone(),
            })
            .unwrap();
    }

    let self_value_to_option_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "key".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Optional(Box::new(value_type.clone()))),
    };

    let self_value_to_option_value_functions = [IntrinsicFunction::SparseRemove];

    for intrinsic_fn in self_value_to_option_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_to_option_value.clone(),
            })
            .unwrap();
    }
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_map_functions(core_ns: &mut SymbolTable, value_type: &Type) {
    let slice_to_self = Signature {
        parameters: [TypeForParameter {
            name: "slice".to_string(),
            resolved_type: Type::Slice(Box::from(value_type.clone())),
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(value_type.clone()),
    };
    let slice_to_self_functions = [IntrinsicFunction::MapFromSlicePair];
    for intrinsic_fn in slice_to_self_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: slice_to_self.clone(),
            })
            .unwrap();
    }

    let unit_to_value = Signature {
        parameters: [].into(),
        return_type: Box::new(value_type.clone()),
    };

    let unit_to_value_functions = [IntrinsicFunction::MapCreate];

    for intrinsic_fn in unit_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: unit_to_value.clone(),
            })
            .unwrap();
    }

    let self_to_value = Signature {
        parameters: [TypeForParameter {
            name: "self".to_string(),
            resolved_type: value_type.clone(),
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(value_type.clone()),
    };

    let self_to_value_functions = [IntrinsicFunction::MapIter, IntrinsicFunction::MapIterMut];

    for intrinsic_fn in self_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_to_value.clone(),
            })
            .unwrap();
    }

    let self_value_to_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(value_type.clone()),
    };

    let self_value_to_value_functions = [IntrinsicFunction::MapSubscript];

    for intrinsic_fn in self_value_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_to_value.clone(),
            })
            .unwrap();
    }

    let self_value_value_mut_to_unit = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "key".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "value".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: true,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(value_type.clone()),
    };

    let self_value_value_mut_to_unit_functions = [IntrinsicFunction::MapSubscriptMut];

    for intrinsic_fn in self_value_value_mut_to_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_value_mut_to_unit.clone(),
            })
            .unwrap();
    }

    let self_value_to_bool = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Bool),
    };

    let self_value_to_bool_functions = [IntrinsicFunction::MapHas];

    for intrinsic_fn in self_value_to_bool_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_to_bool.clone(),
            })
            .unwrap();
    }

    let self_value_to_option_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "key".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Optional(Box::new(value_type.clone()))),
    };

    let self_value_to_option_value_functions = [IntrinsicFunction::MapRemove];

    for intrinsic_fn in self_value_to_option_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_to_option_value.clone(),
            })
            .unwrap();
    }
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_vec_functions(core_ns: &mut SymbolTable, value_type: &Type) {
    let unit_to_value = Signature {
        parameters: [].into(),
        return_type: Box::new(value_type.clone()),
    };

    let unit_to_value_functions = [IntrinsicFunction::VecCreate];

    for intrinsic_fn in unit_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: unit_to_value.clone(),
            })
            .unwrap();
    }

    let self_value_to_unit = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "v".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Unit),
    };

    let self_value_to_unit_functions = [IntrinsicFunction::VecPush];

    for intrinsic_fn in self_value_to_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_value_to_unit.clone(),
            })
            .unwrap();
    }

    let self_int_to_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(value_type.clone()),
    };

    let self_int_to_value_functions = [IntrinsicFunction::VecSubscript];

    for intrinsic_fn in self_int_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_int_to_value.clone(),
            })
            .unwrap();
    }

    let self_int_value_mut_to_unit = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "v".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: true,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(value_type.clone()),
    };

    let self_int_value_mut_to_unit_functions = [IntrinsicFunction::VecSubscriptMut];

    for intrinsic_fn in self_int_value_mut_to_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_int_value_mut_to_unit.clone(),
            })
            .unwrap();
    }

    let self_int_to_optional_value = Signature {
        parameters: [
            TypeForParameter {
                name: "self".to_string(),
                resolved_type: value_type.clone(),
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "i".to_string(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Optional(Box::new(value_type.clone()))),
    };

    let self_int_to_optional_value_functions = [IntrinsicFunction::VecRemove];

    for intrinsic_fn in self_int_to_optional_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_int_to_optional_value.clone(),
            })
            .unwrap();
    }

    let self_to_unit = Signature {
        parameters: [TypeForParameter {
            name: "self".to_string(),
            resolved_type: value_type.clone(),
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Unit),
    };

    let self_to_unit_functions = [IntrinsicFunction::VecClear];

    for intrinsic_fn in self_to_unit_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_to_unit.clone(),
            })
            .unwrap();
    }

    let self_to_value = Signature {
        parameters: [TypeForParameter {
            name: "self".to_string(),
            resolved_type: value_type.clone(),
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(value_type.clone()),
    };

    let self_to_value_functions = [IntrinsicFunction::VecIter, IntrinsicFunction::VecIterMut];

    for intrinsic_fn in self_to_value_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: self_to_value.clone(),
            })
            .unwrap();
    }

    let slice_to_self = Signature {
        parameters: [TypeForParameter {
            name: "slice".to_string(),
            resolved_type: Type::Slice(Box::from(value_type.clone())),
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(value_type.clone()),
    };
    let slice_to_self_functions = [IntrinsicFunction::VecFromSlice];
    for intrinsic_fn in slice_to_self_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: slice_to_self.clone(),
            })
            .unwrap();
    }
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_string_functions(core_ns: &mut SymbolTable) {
    let string_to_int = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: Type::String,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Int),
    };

    let string_to_int_functions = [IntrinsicFunction::StringLen];

    for intrinsic_fn in string_to_int_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: string_to_int.clone(),
            })
            .unwrap();
    }
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_int_functions(core_ns: &mut SymbolTable) {
    let int_to_int = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: Type::Int,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Int),
    };

    let int_to_int_functions = [IntrinsicFunction::IntAbs, IntrinsicFunction::IntRnd];

    for intrinsic_fn in int_to_int_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: int_to_int.clone(),
            })
            .unwrap();
    }

    let int_int_to_int = Signature {
        parameters: [
            TypeForParameter {
                name: "self".into(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "b".into(),
                resolved_type: Type::Int,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Int),
    };
    let int_int_to_int_functions = [IntrinsicFunction::IntMax, IntrinsicFunction::IntMin];

    for intrinsic_fn in int_int_to_int_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: int_int_to_int.clone(),
            })
            .unwrap();
    }

    let int_to_float = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: Type::Int,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Float),
    };

    core_ns
        .add_intrinsic_function(IntrinsicFunctionDefinition {
            name: IntrinsicFunction::IntToFloat.to_string(),
            intrinsic: IntrinsicFunction::IntToFloat,
            signature: int_to_float.clone(),
        })
        .unwrap();
}

#[allow(clippy::too_many_lines)]
fn add_intrinsic_float_functions(core_ns: &mut SymbolTable) {
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

    let float_to_int = Signature {
        parameters: [TypeForParameter {
            name: "self".into(),
            resolved_type: Type::Float,
            is_mutable: false,
            node: None,
        }]
        .into(),
        return_type: Box::new(Type::Int),
    };

    let float_to_float_functions = [
        IntrinsicFunction::FloatFloor,
        IntrinsicFunction::FloatSqrt,
        IntrinsicFunction::FloatSign,
        IntrinsicFunction::FloatAbs,
        IntrinsicFunction::FloatRnd,
        IntrinsicFunction::FloatCos,
        IntrinsicFunction::FloatSin,
        IntrinsicFunction::FloatAcos,
        IntrinsicFunction::FloatAsin,
    ];
    for intrinsic_fn in float_to_float_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: float_to_float.clone(),
            })
            .unwrap();
    }

    let float_to_int_functions = [IntrinsicFunction::FloatRound];
    for intrinsic_fn in float_to_int_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: float_to_int.clone(),
            })
            .unwrap();
    }

    let float_float_to_float = Signature {
        parameters: [
            TypeForParameter {
                name: "self".into(),
                resolved_type: Type::Float,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "other".into(),
                resolved_type: Type::Float,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Float),
    };

    let float_float_to_float_functions = [
        IntrinsicFunction::FloatAtan2,
        IntrinsicFunction::FloatMin,
        IntrinsicFunction::FloatMax,
        IntrinsicFunction::Float2Magnitude,
    ];
    for intrinsic_fn in float_float_to_float_functions {
        let name = intrinsic_fn.to_string();
        core_ns
            .add_intrinsic_function(IntrinsicFunctionDefinition {
                name,
                intrinsic: intrinsic_fn,
                signature: float_float_to_float.clone(),
            })
            .unwrap();
    }

    let float_float_float_to_float = Signature {
        parameters: [
            TypeForParameter {
                name: "self".into(),
                resolved_type: Type::Float,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "a".into(),
                resolved_type: Type::Float,
                is_mutable: false,
                node: None,
            },
            TypeForParameter {
                name: "b".into(),
                resolved_type: Type::Float,
                is_mutable: false,
                node: None,
            },
        ]
        .into(),
        return_type: Box::new(Type::Float),
    };

    core_ns
        .add_intrinsic_function(IntrinsicFunctionDefinition {
            name: IntrinsicFunction::FloatClamp.to_string(),
            intrinsic: IntrinsicFunction::FloatClamp,
            signature: float_float_float_to_float.clone(),
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
    add_external_types(&mut intrinsic_types_symbol_table);
    add_intrinsic_functions(&mut intrinsic_types_symbol_table);

    Module::new(&canonical_core_path, intrinsic_types_symbol_table, None)
}

/*

fn add_external_types(symbol_table: &mut SymbolTable) {
    symbol_table
        .add_type_generator(
            "Sparse",
            TypeGenerator {
                arity: 1,
                kind: GeneratorKind::Sparse,
            },
        )
        .expect("TODO: panic message");

    symbol_table
        .add_type_generator(
            "Vec",
            TypeGenerator {
                arity: 1,
                kind: GeneratorKind::Vec,
            },
        )
        .expect("TODO: panic message");
    symbol_table
        .add_type_generator(
            "Map",
            TypeGenerator {
                arity: 2,
                kind: GeneratorKind::Map,
            },
        )
        .expect("TODO: panic message");
}
*/
