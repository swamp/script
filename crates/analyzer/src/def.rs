/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::{ResolveError, ResolveErrorKind};
use crate::Analyzer;
use seq_map::SeqMap;
use std::rc::Rc;
use swamp_script_semantic::{
    AnonymousStructFieldType, AnonymousStructType, EnumType, EnumTypeRef, EnumVariantCommon,
    EnumVariantSimpleType, EnumVariantSimpleTypeRef, EnumVariantStructType, EnumVariantTupleType,
    EnumVariantType, ExternalFunctionDefinition, Function, FunctionTypeSignature,
    InternalFunctionDefinition, LocalIdentifier, LocalTypeIdentifier, ParameterNode, StructType,
    StructTypeRef, Type, TypeForParameter, UseItem,
};

impl<'a> Analyzer<'a> {
    fn resolve_use_definition(
        &mut self,
        use_definition: &swamp_script_ast::Use,
    ) -> Result<(), ResolveError> {
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

        let found_module = self.shared.modules.get(&path).unwrap();
        if use_definition.items.is_empty() {
            let last_name = path.last().unwrap();
            self.shared
                .lookup_table
                .add_module_link(last_name, found_module.clone())
                .map_err(|err| {
                    self.create_err(
                        ResolveErrorKind::SemanticError(err),
                        &use_definition.module_path.0[0],
                    )
                })?;
        }

        for ast_items in &use_definition.items {
            match ast_items {
                swamp_script_ast::UseItem::Identifier(node) => {
                    let ident_resolved_node = self.to_node(&node.0);
                    let ident = UseItem::Identifier(ident_resolved_node.clone());
                    let ident_text = self.get_text_resolved(&ident_resolved_node).to_string();
                    if let Some(found_symbol) =
                        found_module.namespace.symbol_table.get_symbol(&ident_text)
                    {
                        self.shared
                            .lookup_table
                            .add_symbol(&ident_text, found_symbol.clone())
                            .map_err(|err| {
                                self.create_err(ResolveErrorKind::SemanticError(err), &node.0)
                            })?;
                    } else {
                        return Err(self.create_err_resolved(
                            ResolveErrorKind::UnknownTypeReference,
                            &ident_resolved_node,
                        ));
                    }
                    ident
                }
                swamp_script_ast::UseItem::Type(node) => {
                    let ident_resolved_node = self.to_node(&node.0);
                    let ident_text = self.get_text_resolved(&ident_resolved_node).to_string();
                    if let Some(found_symbol) =
                        found_module.namespace.symbol_table.get_symbol(&ident_text)
                    {
                        self.shared
                            .lookup_table
                            .add_symbol(&ident_text, found_symbol.clone())
                            .map_err(|err| {
                                self.create_err(ResolveErrorKind::SemanticError(err), &node.0)
                            })?;
                    } else {
                        return Err(self.create_err_resolved(
                            ResolveErrorKind::UnknownTypeReference,
                            &ident_resolved_node,
                        ));
                    }
                    UseItem::TypeIdentifier(self.to_node(&node.0))
                }
            };
        }

        Ok(())
    }

    fn resolve_enum_type_definition(
        &mut self,
        enum_type_name: &swamp_script_ast::Node,
        ast_variants: &Vec<swamp_script_ast::EnumVariantType>,
    ) -> Result<EnumTypeRef, ResolveError> {
        let mut resolved_variants = SeqMap::new();

        let parent_number = self.shared.state.allocate_number();

        let enum_parent = EnumType {
            name: LocalTypeIdentifier(self.to_node(enum_type_name)),
            assigned_name: self.get_text(enum_type_name).to_string(),
            module_path: self.shared.lookup.get_path(),
            number: parent_number,
            variants: SeqMap::default(),
        };

        let parent_ref = self.shared.lookup.add_enum_type(enum_parent)?;

        for (container_index_usize, ast_variant_type) in ast_variants.iter().enumerate() {
            let variant_name_node = match ast_variant_type {
                swamp_script_ast::EnumVariantType::Simple(name) => name,
                swamp_script_ast::EnumVariantType::Tuple(name, _) => name,
                swamp_script_ast::EnumVariantType::Struct(name, _) => name,
            };

            let number = self.shared.state.allocate_number();

            let common = EnumVariantCommon {
                name: LocalTypeIdentifier(self.to_node(variant_name_node)),
                number,
                //module_path: ModulePath(vec![]), // TODO:
                //variant_name: LocalTypeIdentifier(self.to_node(variant_name_node)),
                assigned_name: self.get_text(variant_name_node).to_string(),
                //enum_ref: parent_ref.clone(),
                container_index: container_index_usize as u8,
                owner: parent_ref.clone(),
            };

            let variant_type = match ast_variant_type {
                swamp_script_ast::EnumVariantType::Simple(_variant_name_node) => {
                    let simple_ref = EnumVariantSimpleType { common };
                    EnumVariantType::Nothing(EnumVariantSimpleTypeRef::from(simple_ref))
                }
                swamp_script_ast::EnumVariantType::Tuple(_variant_name_node, types) => {
                    let mut vec = Vec::new();
                    for tuple_type in types {
                        let resolved_type = self.resolve_type(tuple_type)?;
                        vec.push(resolved_type);
                    }

                    let resolved_tuple_type = EnumVariantTupleType {
                        common,
                        fields_in_order: vec,
                    };
                    let resolved_tuple_type_ref = Rc::new(resolved_tuple_type);

                    EnumVariantType::Tuple(resolved_tuple_type_ref)
                }
                swamp_script_ast::EnumVariantType::Struct(
                    _variant_name_node,
                    ast_struct_fields,
                ) => {
                    let mut fields = SeqMap::new();

                    for (_index, field_with_type) in ast_struct_fields.fields.iter().enumerate() {
                        // TODO: Check the index
                        let resolved_type = self.resolve_type(&field_with_type.field_type)?;
                        let field_name_str =
                            self.get_text(&field_with_type.field_name.0).to_string();

                        let resolved_field = AnonymousStructFieldType {
                            identifier: Some(self.to_node(&field_with_type.field_name.0)),
                            field_type: resolved_type,
                        };

                        fields.insert(field_name_str, resolved_field).map_err(|_| {
                            self.create_err(
                                ResolveErrorKind::DuplicateFieldName,
                                &field_with_type.field_name.0,
                            )
                        })?;
                    }

                    let enum_variant_struct_type = EnumVariantStructType {
                        common,
                        anon_struct: AnonymousStructType {
                            defined_fields: fields,
                        },
                    };

                    let enum_variant_struct_type_ref = Rc::new(enum_variant_struct_type);

                    EnumVariantType::Struct(enum_variant_struct_type_ref)
                }
            };

            let variant_name_str = self.get_text(variant_name_node).to_string();

            resolved_variants
                .insert(variant_name_str, variant_type.into())
                .map_err(|_| {
                    self.create_err(ResolveErrorKind::DuplicateFieldName, variant_name_node)
                })?;
        }

        parent_ref.borrow_mut().variants = resolved_variants;
        Ok(parent_ref)
    }

    /// # Errors
    ///
    pub fn resolve_struct_type_definition(
        &mut self,
        ast_struct: &swamp_script_ast::StructType,
    ) -> Result<(), ResolveError> {
        let mut resolved_fields = SeqMap::new();

        for field_name_and_type in &ast_struct.fields {
            let resolved_type = self.resolve_type(&field_name_and_type.field_type)?;
            let name_string = self.get_text(&field_name_and_type.field_name.0).to_string();

            let field_type = AnonymousStructFieldType {
                identifier: Some(self.to_node(&field_name_and_type.field_name.0)),
                field_type: resolved_type,
            };

            resolved_fields
                .insert(name_string, field_type)
                .map_err(|_| {
                    self.create_err(
                        ResolveErrorKind::DuplicateFieldName,
                        &field_name_and_type.field_name.0,
                    )
                })?;
        }

        let resolved_anon_struct = AnonymousStructType {
            defined_fields: resolved_fields,
        };

        let struct_name_str = self.get_text(&ast_struct.identifier.0).to_string();

        let resolved_struct = StructType::new(
            self.to_node(&ast_struct.identifier.0),
            &struct_name_str,
            resolved_anon_struct,
        );

        let resolved_struct_ref = self.shared.lookup.add_struct(resolved_struct)?;

        Ok(())
    }

    fn resolve_function_definition(
        &mut self,
        function: &swamp_script_ast::Function,
    ) -> Result<(), ResolveError> {
        match function {
            swamp_script_ast::Function::Internal(function_data) => {
                let parameters = self.resolve_parameters(&function_data.declaration.params)?;
                let return_type = if let Some(found) = &function_data.declaration.return_type {
                    self.resolve_type(found)?
                } else {
                    Type::Unit
                };

                self.scope.return_type = return_type.clone();

                // Set up scope for function body
                for param in &parameters {
                    self.create_local_variable_resolved(
                        &param.node.as_ref().unwrap().name,
                        &param.node.as_ref().unwrap().is_mutable,
                        &<std::option::Option<swamp_script_semantic::Type> as Clone>::clone(
                            &param.resolved_type,
                        )
                        .unwrap()
                        .clone(),
                    )?;
                }
                let function_name = self.get_text(&function_data.declaration.name).to_string();
                let statements =
                    self.resolve_statements_in_function(&function_data.body, &return_type)?;
                self.scope.return_type = Type::Unit;

                let internal = InternalFunctionDefinition {
                    signature: FunctionTypeSignature {
                        parameters,
                        return_type: Box::new(return_type),
                    },
                    body: statements,
                    name: LocalIdentifier(self.to_node(&function_data.declaration.name)),
                };

                let function_ref = self
                    .shared
                    .lookup
                    .add_internal_function_ref(&function_name, internal)?;
            }
            swamp_script_ast::Function::External(ast_signature) => {
                let parameters = self.resolve_parameters(&ast_signature.params)?;
                let external_return_type = if let Some(found) = &ast_signature.return_type {
                    self.resolve_type(found)?
                } else {
                    Type::Unit
                };

                let return_type = external_return_type;
                let external_function_id = self.shared.state.allocate_external_function_id();

                let external = ExternalFunctionDefinition {
                    assigned_name: self.get_text(&ast_signature.name).to_string(),
                    signature: FunctionTypeSignature {
                        parameters,
                        return_type: Box::new(return_type),
                    },
                    name: Some(self.to_node(&ast_signature.name)),
                    id: external_function_id,
                };
            }
        }

        Ok(())
    }

    /// # Errors
    ///
    pub fn resolve_definition(
        &mut self,
        ast_def: &swamp_script_ast::Definition,
    ) -> Result<(), ResolveError> {
        let resolved_def = match ast_def {
            swamp_script_ast::Definition::StructDef(ref ast_struct) => {
                self.resolve_struct_type_definition(ast_struct)?
            }
            swamp_script_ast::Definition::EnumDef(identifier, variants) => {
                self.resolve_enum_type_definition(identifier, variants)?;
            }
            swamp_script_ast::Definition::FunctionDef(function) => {
                let resolved_return_type = self.resolve_return_type(function)?;
                self.start_function(resolved_return_type);
                self.resolve_function_definition(function)?;
                self.stop_function();
            }
            swamp_script_ast::Definition::ImplDef(type_identifier, functions) => {
                self.resolve_impl_definition(type_identifier, functions)?;
            }
            swamp_script_ast::Definition::Use(use_info) => self.resolve_use_definition(use_info)?,
            swamp_script_ast::Definition::Constant(const_info) => {
                self.resolve_constant_definition(const_info)?
            }
        };

        Ok(resolved_def)
    }

    fn resolve_impl_definition(
        &mut self,
        attached_to_type: &swamp_script_ast::Node,
        functions: &Vec<swamp_script_ast::Function>,
    ) -> Result<Type, ResolveError> {
        let fake_qualified_type_name = swamp_script_ast::QualifiedTypeIdentifier {
            name: swamp_script_ast::LocalTypeIdentifier(attached_to_type.clone()),
            module_path: None,
            generic_params: vec![],
        };

        let found_struct = self.find_struct_type(&fake_qualified_type_name)?;

        for function in functions {
            let new_return_type = self.resolve_return_type(function)?;
            self.start_function(new_return_type);

            let function_name = match function {
                swamp_script_ast::Function::Internal(function_with_body) => {
                    &function_with_body.declaration
                }
                swamp_script_ast::Function::External(external_declaration) => external_declaration,
            };

            let function_name_str = self.get_text(&function_name.name).to_string();

            let resolved_function = self.resolve_impl_func(function, &found_struct)?;
            let resolved_function_ref = Rc::new(resolved_function);

            found_struct
                .borrow_mut()
                .functions
                .insert(function_name_str, resolved_function_ref)
                .map_err(|_| {
                    self.create_err(ResolveErrorKind::DuplicateFieldName, attached_to_type)
                })?;
            self.stop_function();
        }

        Ok(Type::Struct(found_struct))
    }

    fn resolve_impl_func(
        &mut self,
        function: &swamp_script_ast::Function,
        found_struct: &StructTypeRef,
    ) -> Result<Function, ResolveError> {
        let resolved_fn = match function {
            swamp_script_ast::Function::Internal(function_data) => {
                let mut parameters = Vec::new();

                if let Some(found_self) = &function_data.declaration.self_parameter {
                    let resolved_type = Type::Struct(found_struct.clone());
                    parameters.push(TypeForParameter {
                        name: self.get_text(&found_self.self_node).to_string(),
                        resolved_type: Some(resolved_type),
                        is_mutable: found_self.is_mutable.is_some(),
                        node: Option::from(ParameterNode {
                            name: self.to_node(&found_self.self_node),
                            is_mutable: self.to_node_option(&found_self.is_mutable),
                        }),
                    });
                }

                for param in &function_data.declaration.params {
                    let resolved_type = self.resolve_type(&param.param_type)?;

                    parameters.push(TypeForParameter {
                        name: self.get_text(&param.variable.name).to_string(),
                        resolved_type: Some(resolved_type),
                        is_mutable: param.variable.is_mutable.is_some(),
                        node: Option::from(ParameterNode {
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
                        &param.resolved_type.clone().unwrap(),
                    )?;
                }

                let statements =
                    self.resolve_statements_in_function(&function_data.body, &return_type)?;

                let internal = InternalFunctionDefinition {
                    signature: FunctionTypeSignature {
                        parameters,
                        return_type: Box::new(return_type),
                    },
                    body: statements,
                    name: LocalIdentifier(self.to_node(&function_data.declaration.name)),
                };

                let internal_ref = Rc::new(internal);

                Function::Internal(internal_ref)
            }

            swamp_script_ast::Function::External(signature) => {
                let mut parameters = Vec::new();

                if let Some(found_self) = &signature.self_parameter {
                    let resolved_type = Type::Struct(found_struct.clone());
                    parameters.push(TypeForParameter {
                        name: self.get_text(&found_self.self_node).to_string(),
                        resolved_type: Some(resolved_type),
                        is_mutable: found_self.is_mutable.is_some(),
                        node: Option::from(ParameterNode {
                            name: self.to_node(&found_self.self_node),
                            is_mutable: self.to_node_option(&found_self.is_mutable),
                        }),
                    });
                }

                // Handle parameters, including self if present
                for param in &signature.params {
                    let resolved_type = self.resolve_type(&param.param_type)?;

                    parameters.push(TypeForParameter {
                        name: self.get_text(&param.variable.name).to_string(),
                        resolved_type: Some(resolved_type),
                        is_mutable: param.variable.is_mutable.is_some(),
                        node: Option::from(ParameterNode {
                            name: self.to_node(&param.variable.name),
                            is_mutable: self.to_node_option(&param.variable.is_mutable),
                        }),
                    });
                }

                let return_type = self.resolve_maybe_type(&signature.return_type)?;

                let external = ExternalFunctionDefinition {
                    assigned_name: self.get_text(&signature.name).to_string(),
                    name: Some(self.to_node(&signature.name)),
                    signature: FunctionTypeSignature {
                        parameters,
                        return_type: Box::new(return_type),
                    },
                    id: 0,
                };

                let external_ref = Rc::new(external);

                Function::External(external_ref)
            }
        };
        Ok(resolved_fn)
    }
}
