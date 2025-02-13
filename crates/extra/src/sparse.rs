use seq_map::SeqMap;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_semantic::modules::Modules;
use swamp_script_semantic::ns::{ClosureTypeGenerator, ModuleNamespace, TypeGenerator};
use swamp_script_semantic::{
    AnonymousStructType, Expression, ExpressionKind, ExternalFunctionDefinition,
    ExternalFunctionDefinitionRef, ExternalType, ExternalTypeRef, Function,
    InternalFunctionDefinition, InternalFunctionDefinitionRef, IteratorTypeDetails,
    IteratorYieldType, LocalIdentifier, Node, Signature, StructType, StructTypeField, Type,
    TypeForParameter,
};

/// # Panics
/// if sparse types are not present.
#[allow(clippy::too_many_lines, clippy::module_name_repetitions)]
#[must_use]
pub fn sparse_type_generator() -> Rc<dyn TypeGenerator> {
    let closure_gen = ClosureTypeGenerator::new(
        |ns: &mut ModuleNamespace, modules: &Modules, params: &[Type]| {
            let concrete_struct_name_in_namespace = format!("Sparse<{}>", params[0]);
            if let Some(found_concrete_struct_type) =
                ns.get_struct(&concrete_struct_name_in_namespace)
            {
                return Ok(Type::Struct(found_concrete_struct_type));
            }

            let value_type = &params[0];
            let optional_value_type = Type::Optional(Box::from(value_type.clone()));
            let sparse_map_rust_type = ExternalType {
                type_name: "SparseMap".to_string(),
                number: 999,
            };

            let sparse_map_rust_type_ref = ExternalTypeRef::from(sparse_map_rust_type);
            let sparse_map_type = Type::External(sparse_map_rust_type_ref.clone());
            let mut fields = SeqMap::new();

            let hidden_field = StructTypeField {
                identifier: None,
                field_type: sparse_map_type,
            };
            fields
                .insert("__hidden".into(), hidden_field)
                .expect("TODO: panic message");

            let ext_sparse_new_fn = ns
                .get_external_function_declaration("sparse_map_create")
                .expect("must have sparse_map");

            let create_struct = StructType {
                name: Node::default(),
                assigned_name: concrete_struct_name_in_namespace,
                anon_struct_type: AnonymousStructType {
                    defined_fields: fields,
                },
                functions: SeqMap::default(),
            };

            let create_struct_ref = Rc::new(RefCell::new(create_struct));
            let create_struct_type = Type::Struct(create_struct_ref.clone());

            let mut functions = SeqMap::new();

            let _ext_sparse_new_reference = Expression {
                ty: create_struct_type.clone(),
                node: Node::default(),
                kind: ExpressionKind::ExternalFunctionAccess(ext_sparse_new_fn.clone()),
            };

            let create_new_expr = Expression {
                ty: create_struct_type.clone(),
                node: Node::default(),
                kind: ExpressionKind::RustValueInstantiation(
                    sparse_map_rust_type_ref,
                    value_type.clone(),
                ),
            };

            // ::new()
            let def_new_fn = InternalFunctionDefinition {
                body: create_new_expr,
                name: LocalIdentifier(Node::default()),
                signature: Signature {
                    parameters: vec![],
                    return_type: Box::new(Type::Struct(create_struct_ref.clone())),
                },
            };
            let external_func = Rc::new(Function::Internal(InternalFunctionDefinitionRef::from(
                def_new_fn,
            )));
            functions.insert("new".to_string(), external_func).unwrap();

            let collection_module = modules
                .get(&["mangrove".into(), "collection".into()])
                .unwrap();
            let sparse_id_struct_type = collection_module
                .borrow()
                .namespace
                .borrow()
                .get_struct("SparseId")
                .unwrap();
            let sparse_id_type = Type::Struct(sparse_id_struct_type);

            // ::iter()
            let external_iter_fn = ExternalFunctionDefinition {
                name: None,
                assigned_name: "iter".to_string(),
                signature: Signature {
                    parameters: vec![],
                    return_type: Box::new(Type::Iterator(Box::from(IteratorTypeDetails {
                        yield_type: IteratorYieldType::KeyValue(
                            sparse_id_type.clone(),
                            value_type.clone(),
                        ),
                    }))),
                },
                id: 0,
            };
            let external_iter_func = Rc::new(Function::External(
                ExternalFunctionDefinitionRef::from(external_iter_fn),
            ));
            functions
                .insert("iter".to_string(), external_iter_func)
                .unwrap();

            // ::subscript()
            let external_subscript_mut_fn = ExternalFunctionDefinition {
                name: None,
                assigned_name: "subscript_mut".to_string(),
                signature: Signature {
                    parameters: vec![
                        TypeForParameter {
                            name: "self".to_string(),
                            resolved_type: Type::Struct(create_struct_ref.clone()),
                            is_mutable: true,
                            node: None,
                        },
                        TypeForParameter {
                            name: "index".to_string(),
                            resolved_type: Type::Int,
                            is_mutable: false,
                            node: None,
                        },
                        TypeForParameter {
                            name: "out".to_string(),
                            resolved_type: optional_value_type.clone(),
                            is_mutable: true,
                            node: None,
                        },
                    ],
                    return_type: Box::from(Type::Unit),
                },
                id: 0,
            };
            let external_subscript_mut_func = Rc::new(Function::External(
                ExternalFunctionDefinitionRef::from(external_subscript_mut_fn),
            ));
            functions
                .insert("subscript_mut".to_string(), external_subscript_mut_func)
                .unwrap();

            // ::subscript()
            let external_subscript_fn = ExternalFunctionDefinition {
                name: None,
                assigned_name: "subscript".to_string(),
                signature: Signature {
                    parameters: vec![
                        TypeForParameter {
                            name: "self".to_string(),
                            resolved_type: Type::Struct(create_struct_ref.clone()),
                            is_mutable: true,
                            node: None,
                        },
                        TypeForParameter {
                            name: "index".to_string(),
                            resolved_type: Type::Int,
                            is_mutable: false,
                            node: None,
                        },
                    ],
                    return_type: Box::from(optional_value_type.clone()),
                },
                id: 0,
            };
            let external_subscript_func = Rc::new(Function::External(
                ExternalFunctionDefinitionRef::from(external_subscript_fn),
            ));
            functions
                .insert("subscript".to_string(), external_subscript_func)
                .unwrap();

            // ::remove()
            let remove_fn_def = ExternalFunctionDefinition {
                name: None,
                assigned_name: "remove".to_string(),
                signature: Signature {
                    parameters: vec![
                        TypeForParameter {
                            name: "self".to_string(),
                            resolved_type: Type::Struct(create_struct_ref.clone()),
                            is_mutable: true,
                            node: None,
                        },
                        TypeForParameter {
                            name: "id".to_string(),
                            resolved_type: sparse_id_type.clone(),
                            is_mutable: false,
                            node: None,
                        },
                    ],
                    return_type: Box::from(optional_value_type),
                },
                id: 0,
            };
            let remove_fn_def_ref = Rc::new(Function::External(
                ExternalFunctionDefinitionRef::from(remove_fn_def),
            ));
            functions
                .insert("remove".to_string(), remove_fn_def_ref)
                .unwrap();

            // ::add()
            let add_fn_def = ExternalFunctionDefinition {
                name: None,
                assigned_name: "add".to_string(),
                signature: Signature {
                    parameters: vec![
                        TypeForParameter {
                            name: "self".to_string(),
                            resolved_type: Type::Struct(create_struct_ref.clone()),
                            is_mutable: true,
                            node: None,
                        },
                        TypeForParameter {
                            name: "value".to_string(),
                            resolved_type: value_type.clone(),
                            is_mutable: false,
                            node: None,
                        },
                    ],
                    return_type: Box::from(sparse_id_type),
                },
                id: 0,
            };
            let add_fn_def_ref = Rc::new(Function::External(ExternalFunctionDefinitionRef::from(
                add_fn_def,
            )));
            functions.insert("add".to_string(), add_fn_def_ref).unwrap();

            // ------------------

            create_struct_ref.borrow_mut().functions = functions;

            Ok(Type::Struct(create_struct_ref))
        },
    );

    Rc::new(closure_gen)
}
