/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::ns::{EnumVariantContainerType, ImplType, ModuleNamespace, StructType, SwampTypeId};
use crate::value::{FunctionRef, Value};
use seq_map::SeqMap;
use swamp_script_ast::SelfParameter;
use swamp_script_ast::{Definition, EnumVariant, ImplItem, ImplMember, LocalTypeIdentifier, Type};
use tracing::debug;

pub struct DefinitionRunner;

impl DefinitionRunner {
    pub fn evaluate_types(
        namespace: &mut ModuleNamespace,
        ast_type_types: &[Type],
    ) -> Result<Vec<SwampTypeId>, String> {
        let mut items = vec![];

        for item_type in ast_type_types {
            items.push(Self::evaluate_type(namespace, item_type)?);
        }

        Ok(items)
    }

    pub fn evaluate_type(
        namespace: &mut ModuleNamespace,
        ast_type: &Type,
    ) -> Result<SwampTypeId, String> {
        let t = match ast_type {
            Type::Int => SwampTypeId::Int,
            Type::Float => SwampTypeId::Float,
            Type::String => SwampTypeId::String,
            Type::Bool => SwampTypeId::Bool,
            Type::Void => SwampTypeId::Void,
            Type::Any => SwampTypeId::Any,

            Type::Struct(local_struct_type_name) => {
                let struct_type_ref = namespace
                    .get_struct(&local_struct_type_name)
                    .expect("should work"); // TODO: Error handling
                SwampTypeId::Struct(struct_type_ref.clone())
            }

            Type::Enum(ast_enum_name) => {
                let enum_type_ref = namespace.get_enum(ast_enum_name).expect("should work"); // TODO: Error handling

                SwampTypeId::Enum(enum_type_ref.clone())
            }

            Type::Array(ast_type) => {
                let swamp_type = Self::evaluate_type(namespace, &**ast_type)?;
                SwampTypeId::Array(Box::new(swamp_type))
            }

            Type::Tuple(ast_types) => {
                let mut swamp_types = vec![];

                for ast_type in ast_types {
                    swamp_types.push(Self::evaluate_type(namespace, ast_type)?);
                }

                let tuple_type_ref = namespace.get_or_create_tuple(swamp_types);

                SwampTypeId::Tuple(tuple_type_ref)
            }

            Type::Map(_, _) => todo!(),
        };

        Ok(t)
    }

    pub(crate) fn execute_definition(
        namespace: &mut ModuleNamespace,
        definition: &Definition,
    ) -> Result<Value, String> {
        debug!("defining {:?}", definition);
        let value = match definition {
            Definition::StructDef(name, fields) => {
                let mut fields_in_order = SeqMap::new();
                for (name, field) in fields {
                    fields_in_order
                        .insert(name.clone(), Self::evaluate_type(namespace, field)?)
                        .unwrap(); // TODO: Error handling
                }

                let struct_type = StructType::new(name.clone(), fields_in_order);

                namespace.add_struct_type(&name, struct_type).map_err(|e| {
                    format!(
                        "Failed to define struct in module '{}': {}",
                        namespace.name(),
                        e
                    )
                })?;
                Value::Unit
            }
            Definition::EnumDef(name, variants) => {
                let mut created_variant_containers = SeqMap::new();

                for (ident, variant) in variants {
                    let container_type = match variant {
                        EnumVariant::Simple => EnumVariantContainerType::Nothing,

                        EnumVariant::Tuple(ast_types) => {
                            let converted_types = Self::evaluate_types(namespace, ast_types)?;
                            let tuple_type = namespace.get_or_create_tuple(converted_types);
                            EnumVariantContainerType::Tuple(tuple_type)
                        }

                        EnumVariant::Struct(ast_fields) => {
                            let mut fields = SeqMap::new();
                            for (field_name, field_type) in ast_fields {
                                fields
                                    .insert(
                                        field_name.clone(),
                                        Self::evaluate_type(namespace, field_type)?,
                                    )
                                    .unwrap();
                            }
                            let internal_struct_type_name =
                                LocalTypeIdentifier::new(&*("_".to_string() + &ident.0));
                            let internal_struct_type =
                                StructType::new(LocalTypeIdentifier::new(""), fields);
                            let created_struct_type_ref = namespace
                                .add_struct_type(&internal_struct_type_name, internal_struct_type)
                                .expect("should work with internal struct type");
                            EnumVariantContainerType::Struct(created_struct_type_ref)
                        }
                    };

                    created_variant_containers
                        .insert(ident, container_type)
                        .expect("should work"); // TODO:
                }

                namespace
                    .create_enum_type(&name, created_variant_containers)
                    .map_err(|e| {
                        format!(
                            "Failed to define enum in module '{}': {}",
                            namespace.name(),
                            e
                        )
                    })?;
                Value::Unit
            }
            Definition::FunctionDef(name, data) => {
                let func_ref = FunctionRef::Internal(
                    LocalTypeIdentifier::new(&*name.0),
                    (data.params.clone(), data.return_type.clone()),
                    data.body.clone(),
                );

                // Single call to add both signature and value
                namespace
                    .add_function(
                        name.0.clone(),
                        (data.params.clone(), data.return_type.clone()),
                        func_ref,
                    )
                    .map_err(|e| {
                        format!(
                            "Failed to define function in module '{}': {}",
                            namespace.name(),
                            e
                        )
                    })?;
                Value::Unit
            }

            Definition::ImplDef(ref ast_struct_type_name, methods) => {
                let mut type_methods = SeqMap::new();

                debug!(
                    "Processing impl for {:?} with {} methods",
                    ast_struct_type_name,
                    methods.len()
                );
                debug!(
                    "Methods to process: {:?}",
                    methods.keys().map(|k| &k.0).collect::<Vec<_>>()
                );

                for (local_ident, method) in methods {
                    debug!("Processing method '{}' of type {:?}", local_ident.0, method);

                    // Check if it's a method with 'self' parameter
                    let is_instance_method = match method {
                        ImplItem::Member(_m) => true, // Member always has self
                        ImplItem::Function(f) => f
                            .params
                            .first()
                            .map_or(false, |p| p.variable.name() == "self"),
                    };

                    if is_instance_method {
                        debug!("Adding instance method: {}", local_ident.0);
                        // Convert to ImplMember regardless of original variant
                        let impl_member = match method {
                            ImplItem::Member(m) => m.clone(),
                            ImplItem::Function(f) => {
                                // Get the first parameter (self) and its mutability
                                let (self_param, remaining_params) = f
                                    .params
                                    .split_first()
                                    .expect("Already checked for self parameter");

                                ImplMember {
                                    self_param: SelfParameter {
                                        is_mutable: self_param.is_mutable,
                                    },
                                    params: remaining_params.to_vec(),
                                    return_type: f.return_type.clone(),
                                    body: f.body.clone(),
                                }
                            }
                        };
                        type_methods
                            .insert(local_ident.0.clone(), impl_member)
                            .expect("should work"); // TODO:
                    } else {
                        debug!("Adding static function: {}", local_ident.0);
                        // Handle static function (no self parameter)
                        let (params, return_type, body) = match method {
                            ImplItem::Member(m) => {
                                (m.params.clone(), m.return_type.clone(), m.body.clone())
                            }
                            ImplItem::Function(f) => {
                                (f.params.clone(), f.return_type.clone(), f.body.clone())
                            }
                        };
                        let func_ref = FunctionRef::Internal(
                            local_ident.clone(),
                            (params.clone(), return_type.clone()),
                            body,
                        );
                        let scoped_name = format!("{}::{}", ast_struct_type_name.0, local_ident.0);

                        namespace
                            .add_function(scoped_name, (params, return_type), func_ref)
                            .map_err(|e| format!("Failed to add static function: {}", e))?;
                    }
                }

                debug!(
                    "Final type_methods contains: {:?}",
                    type_methods.keys().collect::<Vec<_>>()
                );

                let struct_type_ref = namespace.get_local_struct(ast_struct_type_name).unwrap();

                let impl_type = ImplType::new(type_methods, struct_type_ref.clone());

                namespace
                    .add_impl(ast_struct_type_name, struct_type_ref.clone(), impl_type)
                    .map_err(|e| {
                        format!("Failed to add impl in module '{}': {}", namespace.name(), e)
                    })?;
                Value::Unit
            }

            Definition::ExternalFunctionDef(_scoped_name, _data) => {
                // External functions are handled at registration time
                Value::Unit
            }
            Definition::Import(_) => {
                todo!()
            }
            Definition::Comment(_) => {
                todo!()
            }
        };

        Ok(value)
    }
}
