/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::ResolveError;
use crate::Resolver;
use seq_map::SeqMap;
use std::rc::Rc;
use swamp_script_ast::{
    Definition, EnumVariantType, Function, LocalTypeIdentifier, Node, QualifiedTypeIdentifier,
    StructType, Use, UseItem,
};
use swamp_script_semantic::{
    CommonEnumVariantType, FunctionTypeSignature, ResolvedAnonymousStructFieldType,
    ResolvedAnonymousStructType, ResolvedDefinition, ResolvedEnumType, ResolvedEnumTypeRef,
    ResolvedEnumVariantContainerType, ResolvedEnumVariantStructType, ResolvedEnumVariantTupleType,
    ResolvedEnumVariantType, ResolvedEnumVariantTypeRef, ResolvedExternalFunctionDefinition,
    ResolvedFunction, ResolvedInternalFunctionDefinition, ResolvedLocalIdentifier,
    ResolvedLocalTypeIdentifier, ResolvedModulePath, ResolvedParameterNode, ResolvedStructType,
    ResolvedStructTypeRef, ResolvedType, ResolvedTypeForParameter, ResolvedUse, ResolvedUseItem,
};
use tracing::info;

impl<'a> Resolver<'a> {
    fn resolve_use_definition(
        &self,
        use_definition: &Use,
    ) -> Result<ResolvedDefinition, ResolveError> {
        let mut nodes = Vec::new();
        for ast_node in &use_definition.module_path.0 {
            nodes.push(self.to_node(ast_node));
        }

        let path: Vec<String> = nodes
            .iter()
            .map(|node| {
                let text = self.get_text_resolved(node);
                text.to_string()
            })
            .collect();

        let mut items = Vec::new();
        let lookup = &self.shared.lookup;

        for ast_items in &use_definition.items {
            let resolved_item = match ast_items {
                UseItem::Identifier(node) => {
                    let ident_resolved_node = self.to_node(&node.0);
                    let ident = ResolvedUseItem::Identifier(ident_resolved_node.clone());
                    let ident_text = self.get_text_resolved(&ident_resolved_node);

                    lookup.get_internal_function(&path, ident_text).ok_or(
                        ResolveError::UnknownTypeReference(ident_resolved_node.clone()),
                    )?;
                    if let Some(found_internal_function) =
                        lookup.get_internal_function(&path, ident_text)
                    {
                        lookup.add_internal_function_link(ident_text, found_internal_function)?;
                    } else if let Some(found_external_function_def) =
                        lookup.get_external_function_declaration(&path, ident_text)
                    {
                        lookup.add_external_function_declaration_link(
                            ident_text,
                            found_external_function_def,
                        )?;
                    } else {
                        return Err(ResolveError::UnknownFunction(ident_resolved_node.clone()));
                    }
                    ident
                }
                UseItem::Type(node) => {
                    let ident_resolved_node = self.to_node(&node.0);
                    let ident_text = self.get_text_resolved(&ident_resolved_node);

                    if let Some(found_struct) = lookup.get_struct(&path, ident_text) {
                        lookup.add_struct_link(found_struct)?;
                    } else if let Some(found_enum) = lookup.get_enum(&path, ident_text) {
                        lookup.add_enum_link(found_enum)?;
                    } else {
                        return Err(ResolveError::UnknownTypeReference(
                            ident_resolved_node.clone(),
                        ));
                    }
                    ResolvedUseItem::TypeIdentifier(self.to_node(&node.0))
                }
            };
            items.push(resolved_item);
        }

        Ok(ResolvedDefinition::Use(ResolvedUse { path: nodes, items }))
    }

    fn resolve_enum_type_definition(
        &mut self,
        enum_type_name: &Node,
        ast_variants: &Vec<EnumVariantType>,
    ) -> Result<(ResolvedEnumTypeRef, Vec<ResolvedEnumVariantTypeRef>), ResolveError> {
        let mut resolved_variants = Vec::new();

        let parent_number = self.shared.state.allocate_number();

        let enum_parent = ResolvedEnumType {
            name: ResolvedLocalTypeIdentifier(self.to_node(enum_type_name)),
            assigned_name: self.get_text(enum_type_name).to_string(),
            module_path: self.shared.lookup.get_path(),
            number: parent_number,
        };

        let enum_type_str = self.get_text(enum_type_name).to_string();
        let parent_ref = self.shared.lookup.add_enum_type(enum_parent)?;

        for (container_index_usize, variant_type) in ast_variants.iter().enumerate() {
            let variant_name_node = match variant_type {
                EnumVariantType::Simple(name) => name,
                EnumVariantType::Tuple(name, _) => name,
                EnumVariantType::Struct(name, _) => name,
            };

            let container = match variant_type {
                EnumVariantType::Simple(_variant_name_node) => {
                    ResolvedEnumVariantContainerType::Nothing
                }
                EnumVariantType::Tuple(_variant_name_node, types) => {
                    let mut vec = Vec::new();
                    for tuple_type in types {
                        let resolved_type = self.resolve_type(tuple_type)?;
                        vec.push(resolved_type);
                    }

                    let number = self.shared.state.allocate_number();

                    let common = CommonEnumVariantType {
                        number,
                        module_path: ResolvedModulePath(vec![]), // TODO:
                        variant_name: ResolvedLocalTypeIdentifier(self.to_node(variant_name_node)),
                        assigned_name: self.get_text(variant_name_node).to_string(),
                        enum_ref: parent_ref.clone(),
                        container_index: container_index_usize as u8,
                    };

                    let resolved_tuple_type = ResolvedEnumVariantTupleType {
                        common,
                        fields_in_order: vec,
                    };
                    let resolved_tuple_type_ref = Rc::new(resolved_tuple_type);

                    ResolvedEnumVariantContainerType::Tuple(resolved_tuple_type_ref)
                }
                EnumVariantType::Struct(_variant_name_node, ast_struct_fields) => {
                    let mut fields = SeqMap::new();

                    for (_index, field_with_type) in ast_struct_fields.fields.iter().enumerate() {
                        // TODO: Check the index
                        let resolved_type = self.resolve_type(&field_with_type.field_type)?;
                        let field_name_str =
                            self.get_text(&field_with_type.field_name.0).to_string();

                        let resolved_field = ResolvedAnonymousStructFieldType {
                            identifier: Some(self.to_node(&field_with_type.field_name.0)),
                            field_type: resolved_type,
                        };

                        fields.insert(field_name_str, resolved_field).map_err(|_| {
                            ResolveError::DuplicateFieldName(
                                self.to_node(&field_with_type.field_name.0),
                            )
                        })?;
                    }

                    let number = self.shared.state.allocate_number();

                    let common = CommonEnumVariantType {
                        number,
                        container_index: container_index_usize as u8,
                        module_path: ResolvedModulePath(vec![]), // TODO:
                        variant_name: ResolvedLocalTypeIdentifier(self.to_node(variant_name_node)),
                        assigned_name: self.get_text(variant_name_node).to_string(),
                        enum_ref: parent_ref.clone(),
                    };

                    let enum_variant_struct_type = ResolvedEnumVariantStructType {
                        common,
                        anon_struct: ResolvedAnonymousStructType {
                            defined_fields: fields,
                        },
                    };

                    let enum_variant_struct_type_ref = Rc::new(enum_variant_struct_type);

                    ResolvedEnumVariantContainerType::Struct(enum_variant_struct_type_ref)
                }
            };

            let variant_name_str = self.get_text(variant_name_node).to_string();

            let unique_variant_type_number = self.shared.state.allocate_number();
            let variant_type = ResolvedEnumVariantType {
                owner: parent_ref.clone(),
                data: container,
                name: ResolvedLocalTypeIdentifier(self.to_node(variant_name_node)),
                assigned_name: self.get_text(variant_name_node).to_string(),
                number: unique_variant_type_number,
                container_index: container_index_usize as u8,
            };

            let variant_type_ref = self.shared.lookup.add_enum_variant(
                &enum_type_str,
                &variant_name_str,
                variant_type,
            )?;

            resolved_variants.push(variant_type_ref);
        }

        Ok((parent_ref, resolved_variants))
    }

    /// # Errors
    ///
    pub fn resolve_struct_type_definition(
        &mut self,
        ast_struct: &StructType,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        let mut resolved_fields = SeqMap::new();

        for field_name_and_type in &ast_struct.fields {
            let resolved_type = self.resolve_type(&field_name_and_type.field_type)?;
            let name_string = self.get_text(&field_name_and_type.field_name.0).to_string();

            let field_type = ResolvedAnonymousStructFieldType {
                identifier: Some(self.to_node(&field_name_and_type.field_name.0)),
                field_type: resolved_type,
            };

            resolved_fields
                .insert(name_string, field_type)
                .map_err(|_| {
                    ResolveError::DuplicateFieldName(
                        self.to_node(&field_name_and_type.field_name.0),
                    )
                })?;
        }

        let resolved_anon_struct = ResolvedAnonymousStructType {
            defined_fields: resolved_fields,
        };

        let struct_name_str = self.get_text(&ast_struct.identifier.0).to_string();

        let resolved_struct = ResolvedStructType::new(
            self.to_node(&ast_struct.identifier.0),
            &struct_name_str,
            resolved_anon_struct,
        );

        let resolved_struct_ref = self.shared.lookup.add_struct(resolved_struct)?;

        Ok(resolved_struct_ref)
    }

    fn resolve_function_definition(
        &mut self,
        function: &Function,
    ) -> Result<ResolvedDefinition, ResolveError> {
        let resolved_function = match function {
            Function::Internal(function_data) => {
                let parameters = self.resolve_parameters(&function_data.declaration.params)?;
                let return_type = if let Some(found) = &function_data.declaration.return_type {
                    self.resolve_type(found)?
                } else {
                    ResolvedType::Unit
                };

                self.scope.return_type = return_type.clone();

                // Set up scope for function body
                for param in &parameters {
                    self.create_local_variable_resolved(
                        &param.node.as_ref().unwrap().name,
                        &param.node.as_ref().unwrap().is_mutable,
                        &param.resolved_type.clone(),
                    )?;
                }

                // Constants must be resolved first, since they can be referenced in the function expressions
                let constants = self.resolve_constants(&function_data.constants)?;

                let statements =
                    self.resolve_statements_in_function(&function_data.body, &return_type)?;
                self.scope.return_type = ResolvedType::Unit;

                let internal = ResolvedInternalFunctionDefinition {
                    signature: FunctionTypeSignature {
                        first_parameter_is_self: false,
                        parameters,
                        return_type: Box::new(return_type),
                    },
                    body: statements,
                    name: ResolvedLocalIdentifier(self.to_node(&function_data.declaration.name)),
                    constants,
                };

                let function_name = self.get_text(&function_data.declaration.name).to_string();
                info!(?function_name, "adding function");
                let function_ref = self
                    .shared
                    .lookup
                    .add_internal_function_ref(&function_name, internal)?;
                ResolvedFunction::Internal(function_ref)
            }
            Function::External(ast_signature) => {
                let parameters = self.resolve_parameters(&ast_signature.params)?;
                let external_return_type = if let Some(found) = &ast_signature.return_type {
                    self.resolve_type(found)?
                } else {
                    ResolvedType::Unit
                };

                let return_type = external_return_type;
                let external_function_id = self.shared.state.allocate_external_function_id();

                let external = ResolvedExternalFunctionDefinition {
                    assigned_name: self.get_text(&ast_signature.name).to_string(),
                    signature: FunctionTypeSignature {
                        first_parameter_is_self: false,
                        parameters,
                        return_type: Box::new(return_type),
                    },
                    name: Some(self.to_node(&ast_signature.name)),
                    id: external_function_id,
                };

                ResolvedFunction::External(Rc::new(external))
            }
        };

        Ok(ResolvedDefinition::FunctionDef(resolved_function))
    }

    /// # Errors
    ///
    pub fn resolve_definition(
        &mut self,
        ast_def: &Definition,
    ) -> Result<ResolvedDefinition, ResolveError> {
        let resolved_def = match ast_def {
            Definition::StructDef(ref ast_struct) => {
                ResolvedDefinition::StructType(self.resolve_struct_type_definition(ast_struct)?)
            }
            Definition::EnumDef(identifier, variants) => {
                let (parent, variants) = self.resolve_enum_type_definition(identifier, variants)?;
                ResolvedDefinition::EnumType(parent, variants)
            }
            Definition::FunctionDef(function) => {
                let resolved_return_type = self.resolve_return_type(function)?;
                self.start_function(resolved_return_type);
                let resolved_def = self.resolve_function_definition(function)?;
                self.stop_function();
                resolved_def
            }
            Definition::ImplDef(type_identifier, functions) => {
                let attached_type_type =
                    self.resolve_impl_definition(type_identifier, functions)?;
                ResolvedDefinition::ImplType(attached_type_type)
            }
            Definition::Comment(comment_ref) => {
                ResolvedDefinition::Comment(self.to_node(comment_ref))
            }
            Definition::Use(use_info) => self.resolve_use_definition(use_info)?,
            Definition::Constant(const_info) => self.resolve_constant_definition(const_info)?,
        };

        Ok(resolved_def)
    }

    fn resolve_impl_definition(
        &mut self,
        attached_to_type: &Node,
        functions: &Vec<Function>,
    ) -> Result<ResolvedType, ResolveError> {
        let fake_qualified_type_name = QualifiedTypeIdentifier {
            name: LocalTypeIdentifier(attached_to_type.clone()),
            module_path: None,
            generic_params: vec![],
        };

        let found_struct = self.find_struct_type(&fake_qualified_type_name)?;

        for function in functions {
            let new_return_type = self.resolve_return_type(function)?;
            self.start_function(new_return_type);

            let function_name = match function {
                Function::Internal(function_with_body) => &function_with_body.declaration,
                Function::External(external_declaration) => external_declaration,
            };

            let function_name_str = self.get_text(&function_name.name).to_string();

            let resolved_function = self.resolve_impl_func(function, &found_struct)?;
            let resolved_function_ref = Rc::new(resolved_function);

            found_struct
                .borrow_mut()
                .functions
                .insert(function_name_str, resolved_function_ref)?;
            self.stop_function();
        }

        Ok(ResolvedType::Struct(found_struct))
    }

    fn resolve_impl_func(
        &mut self,
        function: &Function,
        found_struct: &ResolvedStructTypeRef,
    ) -> Result<ResolvedFunction, ResolveError> {
        let resolved_fn = match function {
            Function::Internal(function_data) => {
                let mut parameters = Vec::new();

                if let Some(found_self) = &function_data.declaration.self_parameter {
                    let resolved_type = ResolvedType::Struct(found_struct.clone());
                    parameters.push(ResolvedTypeForParameter {
                        name: self.get_text(&found_self.self_node).to_string(),
                        resolved_type,
                        is_mutable: found_self.is_mutable.is_some(),
                        node: Option::from(ResolvedParameterNode {
                            name: self.to_node(&found_self.self_node),
                            is_mutable: self.to_node_option(&found_self.is_mutable),
                        }),
                    });
                }

                for param in &function_data.declaration.params {
                    let resolved_type = self.resolve_type(&param.param_type)?;

                    parameters.push(ResolvedTypeForParameter {
                        name: self.get_text(&param.variable.name).to_string(),
                        resolved_type,
                        is_mutable: param.variable.is_mutable.is_some(),
                        node: Option::from(ResolvedParameterNode {
                            name: self.to_node(&param.variable.name),
                            is_mutable: self.to_node_option(&param.variable.is_mutable),
                        }),
                    });
                }

                let return_type =
                    self.resolve_maybe_type(&function_data.declaration.return_type)?;

                for param in &parameters {
                    self.create_local_variable_resolved(
                        &param.node.as_ref().unwrap().name,
                        &param.node.as_ref().unwrap().is_mutable,
                        &param.resolved_type,
                    )?;
                }

                // Constants needs to be resolved before the function expressions, since they can be accessed.
                let constants = self.resolve_constants(&function_data.constants)?;

                let statements =
                    self.resolve_statements_in_function(&function_data.body, &return_type)?;

                let internal = ResolvedInternalFunctionDefinition {
                    signature: FunctionTypeSignature {
                        first_parameter_is_self: function_data.declaration.self_parameter.is_some(),
                        parameters,
                        return_type: Box::new(return_type),
                    },
                    body: statements,
                    name: ResolvedLocalIdentifier(self.to_node(&function_data.declaration.name)),
                    constants,
                };

                let internal_ref = Rc::new(internal);

                ResolvedFunction::Internal(internal_ref)
            }

            Function::External(signature) => {
                let mut parameters = Vec::new();

                if let Some(found_self) = &signature.self_parameter {
                    let resolved_type = ResolvedType::Struct(found_struct.clone());
                    parameters.push(ResolvedTypeForParameter {
                        name: self.get_text(&found_self.self_node).to_string(),
                        resolved_type,
                        is_mutable: found_self.is_mutable.is_some(),
                        node: Option::from(ResolvedParameterNode {
                            name: self.to_node(&found_self.self_node),
                            is_mutable: self.to_node_option(&found_self.is_mutable),
                        }),
                    });
                }

                // Handle parameters, including self if present
                for param in &signature.params {
                    let resolved_type = self.resolve_type(&param.param_type)?;

                    parameters.push(ResolvedTypeForParameter {
                        name: self.get_text(&param.variable.name).to_string(),
                        resolved_type,
                        is_mutable: param.variable.is_mutable.is_some(),
                        node: Option::from(ResolvedParameterNode {
                            name: self.to_node(&param.variable.name),
                            is_mutable: self.to_node_option(&param.variable.is_mutable),
                        }),
                    });
                }

                let return_type = self.resolve_maybe_type(&signature.return_type)?;

                let external = ResolvedExternalFunctionDefinition {
                    assigned_name: self.get_text(&signature.name).to_string(),
                    name: Some(self.to_node(&signature.name)),
                    signature: FunctionTypeSignature {
                        first_parameter_is_self: signature.self_parameter.is_some(),
                        parameters,
                        return_type: Box::new(return_type),
                    },
                    id: 0,
                };

                let external_ref = Rc::new(external);

                ResolvedFunction::External(external_ref)
            }
        };
        Ok(resolved_fn)
    }
}
