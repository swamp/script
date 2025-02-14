/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::{Error, ErrorKind};
use crate::Resolver;
use seq_map::SeqMap;
use std::rc::Rc;
use swamp_script_modules::ns::GenericAwareType;
use swamp_script_semantic::{
    AliasType, AliasTypeRef, AnonymousStructType, Definition, EnumType, EnumTypeRef,
    EnumVariantCommon, EnumVariantSimpleType, EnumVariantSimpleTypeRef, EnumVariantStructType,
    EnumVariantTupleType, EnumVariantType, ExternalFunctionDefinition, Function,
    InternalFunctionDefinition, LocalIdentifier, LocalTypeIdentifier, Mod, ParameterNode,
    Signature, StructType, StructTypeField, StructTypeRef, Type, TypeForParameter,
    TypeParameterName, Use, UseItem,
};

impl<'a> Resolver<'a> {
    fn analyze_use_definition(
        &mut self,
        use_definition: &swamp_script_ast::Use,
    ) -> Result<Definition, Error> {
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

        if use_definition.items.is_empty() {
            let last_name = path.last().unwrap();
            let ns = self.shared.lookup.get_namespace(&path).unwrap();
            self.shared.lookup.add_namespace_link(last_name, ns)?;
        }

        for ast_items in &use_definition.items {
            let resolved_item = match ast_items {
                swamp_script_ast::UseItem::Identifier(node) => {
                    let ident_resolved_node = self.to_node(&node.0);
                    let ident = UseItem::Identifier(ident_resolved_node.clone());
                    let ident_text = self.get_text_resolved(&ident_resolved_node).to_string();

                    if let Some(found_internal_function) = self
                        .shared
                        .lookup
                        .get_internal_function(&path, &*ident_text)
                    {
                        self.shared
                            .lookup
                            .add_internal_function_link(&*ident_text, found_internal_function)?;
                    } else if let Some(found_external_function_def) = self
                        .shared
                        .lookup
                        .get_external_function_declaration(&path, &*ident_text)
                    {
                        self.shared.lookup.add_external_function_declaration_link(
                            &*ident_text,
                            found_external_function_def,
                        )?;
                    } else {
                        return Err(self.create_err_resolved(
                            ErrorKind::UnknownFunction,
                            &ident_resolved_node,
                        ));
                    }
                    ident
                }
                swamp_script_ast::UseItem::Type(node) => {
                    let ident_resolved_node = self.to_node(&node.0);
                    let ident_text = self.get_text_resolved(&ident_resolved_node).to_string();
                    let maybe_generic = { self.shared.lookup.get_generic(&path, &*ident_text) };
                    if let Some(found_generic_type) = maybe_generic {
                        self.shared
                            .lookup
                            .add_generic_link(&*ident_text, found_generic_type.clone())?
                    } else if let Some(found_struct) =
                        self.shared.lookup.get_struct(&path, &*ident_text)
                    {
                        self.shared.lookup.add_struct_link(found_struct)?;
                    } else if let Some(found_enum) =
                        self.shared.lookup.get_enum(&path, &*ident_text)
                    {
                        self.shared.lookup.add_enum_link(&found_enum)?;
                    } else {
                        return Err(self.create_err_resolved(
                            ErrorKind::UnknownTypeReference,
                            &ident_resolved_node,
                        ));
                    }
                    UseItem::TypeIdentifier(self.to_node(&node.0))
                }
            };
            items.push(resolved_item);
        }

        Ok(Definition::Use(Use { path: nodes, items }))
    }

    fn analyze_mod_definition(
        &self,
        mod_definition: &swamp_script_ast::Mod,
    ) -> Result<Definition, Error> {
        let mut nodes = Vec::new();
        let mut path = Vec::new();
        for ast_node in &mod_definition.module_path.0 {
            nodes.push(self.to_node(ast_node));
            path.push(self.get_text(ast_node).to_string());
        }

        let lookup = &self.shared.lookup;

        let mut nodes_copy = path.clone();
        nodes_copy.insert(0, "crate".to_string());

        if let Some(found_namespace) = lookup.get_namespace(&nodes_copy) {
            lookup.add_namespace_link(nodes_copy.last().unwrap(), found_namespace)?;

            Ok(Definition::Mod(Mod { path: nodes }))
        } else {
            let first = &mod_definition.module_path.0[0];
            Err(self.create_err(ErrorKind::UnknownModule, &first))
        }
    }

    fn analyze_enum_type_definition(
        &mut self,
        enum_type_name: &swamp_script_ast::LocalTypeIdentifierWithOptionalTypeParams,
        ast_variants: &[swamp_script_ast::EnumVariantType],
    ) -> Result<EnumTypeRef, Error> {
        let mut resolved_variants = SeqMap::new();

        let parent_number = self.shared.state.allocate_number();

        let enum_parent = EnumType {
            name: LocalTypeIdentifier(self.to_node(&enum_type_name.name)),
            assigned_name: self.get_text(&enum_type_name.name).to_string(),
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
                        let resolved_type = self.analyze_type(tuple_type)?;
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

                    for field_with_type in &ast_struct_fields.fields {
                        // TODO: Check the index
                        let resolved_type = self.analyze_type(&field_with_type.field_type)?;
                        let field_name_str =
                            self.get_text(&field_with_type.field_name.0).to_string();

                        let resolved_field = StructTypeField {
                            identifier: Some(self.to_node(&field_with_type.field_name.0)),
                            field_type: resolved_type,
                        };

                        fields.insert(field_name_str, resolved_field).map_err(|_| {
                            self.create_err(
                                ErrorKind::DuplicateFieldName,
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
                .map_err(|_| self.create_err(ErrorKind::DuplicateFieldName, variant_name_node))?;
        }

        parent_ref.borrow_mut().variants = resolved_variants;
        Ok(parent_ref)
    }

    pub fn analyze_alias_type_definition(
        &mut self,
        ast_alias: &swamp_script_ast::AliasType,
    ) -> Result<AliasTypeRef, Error> {
        let resolved_type = self.analyze_type(&ast_alias.referenced_type)?;

        let alias_name_str = self.get_text(&ast_alias.identifier.0).to_string();
        let resolved_alias = AliasType {
            name: self.to_node(&ast_alias.identifier.0),
            assigned_name: alias_name_str,
            referenced_type: resolved_type,
        };

        let resolved_alias_ref = self.shared.lookup.add_alias(resolved_alias)?;

        Ok(resolved_alias_ref)
    }

    pub fn analyze_struct_type(
        &mut self,
        assigned_name: &str,
        ast_struct: &swamp_script_ast::StructType,
    ) -> Result<StructTypeRef, Error> {
        let mut resolved_fields = SeqMap::new();

        for field_name_and_type in &ast_struct.fields {
            let resolved_type = self.analyze_type(&field_name_and_type.field_type)?;
            let name_string = self.get_text(&field_name_and_type.field_name.0).to_string();

            let field_type = StructTypeField {
                identifier: Some(self.to_node(&field_name_and_type.field_name.0)),
                field_type: resolved_type,
            };

            resolved_fields
                .insert(name_string, field_type)
                .map_err(|_| {
                    self.create_err(
                        ErrorKind::DuplicateFieldName,
                        &field_name_and_type.field_name.0,
                    )
                })?;
        }

        let resolved_anon_struct = AnonymousStructType {
            defined_fields: resolved_fields,
        };

        let resolved_struct = StructType::new(
            self.to_node(&ast_struct.identifier.name),
            assigned_name,
            resolved_anon_struct,
        );

        let resolved_struct_ref = self.shared.lookup.add_struct(resolved_struct)?;
        Ok(resolved_struct_ref)
    }

    /// # Errors
    ///
    pub fn analyze_struct_type_definition(
        &mut self,
        ast_struct: &swamp_script_ast::StructType,
    ) -> Result<Definition, Error> {
        let struct_name_str = self.get_text(&ast_struct.identifier.name).to_string();

        if !ast_struct.identifier.parameter_names.is_empty() {
            let mut parameter_names = SeqMap::new();
            for name in &ast_struct.identifier.parameter_names {
                let assigned_name = self.get_text(name).to_string();
                parameter_names
                    .insert(
                        assigned_name.clone(),
                        TypeParameterName {
                            resolved_node: self.to_node(name),
                            assigned_name,
                        },
                    )
                    .expect("TODO: panic message");
            }

            self.shared.lookup.add_generic(
                &struct_name_str,
                parameter_names,
                GenericAwareType::Struct(ast_struct.clone()),
                self.shared.file_id,
            )?;

            return Ok(Definition::GenericType);
        }

        let struct_name_str = self.get_text(&ast_struct.identifier.name).to_string();
        let resolved_struct_ref = self.analyze_struct_type(&*struct_name_str, ast_struct)?;

        Ok(Definition::StructType(resolved_struct_ref))
    }

    fn analyze_function_definition(
        &mut self,
        function: &swamp_script_ast::Function,
    ) -> Result<Definition, Error> {
        let resolved_function = match function {
            swamp_script_ast::Function::Internal(function_data) => {
                let parameters = self.analyze_parameters(&function_data.declaration.params)?;
                let return_type = if let Some(found) = &function_data.declaration.return_type {
                    self.analyze_type(found)?
                } else {
                    Type::Unit
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
                let function_name = self.get_text(&function_data.declaration.name).to_string();
                let statements =
                    self.analyze_statements_in_function(&function_data.body, &return_type)?;
                self.scope.return_type = Type::Unit;

                let internal = InternalFunctionDefinition {
                    signature: Signature {
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
                Function::Internal(function_ref)
            }
            swamp_script_ast::Function::External(ast_signature) => {
                let parameters = self.analyze_parameters(&ast_signature.params)?;
                let external_return_type = if let Some(found) = &ast_signature.return_type {
                    self.analyze_type(found)?
                } else {
                    Type::Unit
                };

                let return_type = external_return_type;
                let external_function_id = self.shared.state.allocate_external_function_id();

                let external = ExternalFunctionDefinition {
                    assigned_name: self.get_text(&ast_signature.name).to_string(),
                    signature: Signature {
                        parameters,
                        return_type: Box::new(return_type),
                    },
                    name: Some(self.to_node(&ast_signature.name)),
                    id: external_function_id,
                };

                let function_ref = self
                    .shared
                    .lookup
                    .add_external_function_declaration_ref(external)?;

                Function::External(function_ref)
            }
        };

        Ok(Definition::FunctionDef(resolved_function))
    }

    /// # Errors
    ///
    pub fn analyze_definition(
        &mut self,
        ast_def: &swamp_script_ast::Definition,
    ) -> Result<Definition, Error> {
        let resolved_def = match ast_def {
            swamp_script_ast::Definition::StructDef(ref ast_struct) => {
                self.analyze_struct_type_definition(ast_struct)?
            }

            swamp_script_ast::Definition::AliasDef(ref alias_def) => {
                Definition::AliasType(self.analyze_alias_type_definition(alias_def)?)
            }
            swamp_script_ast::Definition::EnumDef(identifier, variants) => {
                let parent = self.analyze_enum_type_definition(identifier, variants)?;
                Definition::EnumType(parent)
            }
            swamp_script_ast::Definition::FunctionDef(function) => {
                let resolved_return_type = self.analyze_return_type(function)?;
                self.start_function(resolved_return_type);
                let resolved_def = self.analyze_function_definition(function)?;
                self.stop_function();
                resolved_def
            }
            swamp_script_ast::Definition::ImplDef(type_identifier, functions) => {
                let attached_type_type =
                    self.analyze_impl_definition(type_identifier, functions)?;
                Definition::ImplType(attached_type_type)
            }
            swamp_script_ast::Definition::Comment(comment_ref) => {
                Definition::Comment(self.to_node(comment_ref))
            }
            swamp_script_ast::Definition::Use(use_info) => self.analyze_use_definition(use_info)?,
            swamp_script_ast::Definition::Mod(mod_info) => self.analyze_mod_definition(mod_info)?,
            swamp_script_ast::Definition::Constant(const_info) => {
                self.analyze_constant_definition(const_info)?
            }
        };

        Ok(resolved_def)
    }

    fn analyze_impl_definition(
        &mut self,
        attached_to_type: &swamp_script_ast::LocalTypeIdentifierWithOptionalTypeParams,
        functions: &Vec<swamp_script_ast::Function>,
    ) -> Result<Type, Error> {
        let (found_struct, _is_generic) = self.find_local_impl_target(&attached_to_type)?;

        for function in functions {
            let new_return_type = self.analyze_return_type(function)?;
            self.start_function(new_return_type);

            let function_name = match function {
                swamp_script_ast::Function::Internal(function_with_body) => {
                    &function_with_body.declaration
                }
                swamp_script_ast::Function::External(external_declaration) => external_declaration,
            };

            let function_name_str = self.get_text(&function_name.name).to_string();

            let resolved_function = self.analyze_impl_func(function, &found_struct)?;
            let resolved_function_ref = Rc::new(resolved_function);

            found_struct
                .borrow_mut()
                .functions
                .insert(function_name_str, resolved_function_ref)
                .map_err(|_| {
                    self.create_err(ErrorKind::DuplicateFieldName, &attached_to_type.name)
                })?;
            self.stop_function();
        }

        Ok(Type::Struct(found_struct))
    }

    fn analyze_impl_func(
        &mut self,
        function: &swamp_script_ast::Function,
        found_struct: &StructTypeRef,
    ) -> Result<Function, Error> {
        let resolved_fn = match function {
            swamp_script_ast::Function::Internal(function_data) => {
                let mut parameters = Vec::new();

                if let Some(found_self) = &function_data.declaration.self_parameter {
                    let resolved_type = Type::Struct(found_struct.clone());
                    parameters.push(TypeForParameter {
                        name: self.get_text(&found_self.self_node).to_string(),
                        resolved_type,
                        is_mutable: found_self.is_mutable.is_some(),
                        node: Option::from(ParameterNode {
                            name: self.to_node(&found_self.self_node),
                            is_mutable: self.to_node_option(&found_self.is_mutable),
                        }),
                    });
                }

                for param in &function_data.declaration.params {
                    let resolved_type = self.analyze_type(&param.param_type)?;

                    parameters.push(TypeForParameter {
                        name: self.get_text(&param.variable.name).to_string(),
                        resolved_type,
                        is_mutable: param.variable.is_mutable.is_some(),
                        node: Option::from(ParameterNode {
                            name: self.to_node(&param.variable.name),
                            is_mutable: self.to_node_option(&param.variable.is_mutable),
                        }),
                    });
                }

                let return_type =
                    self.analyze_maybe_type(&function_data.declaration.return_type)?;

                for param in &parameters {
                    self.create_local_variable_resolved(
                        &param.node.as_ref().unwrap().name,
                        &param.node.as_ref().unwrap().is_mutable,
                        &param.resolved_type.clone(),
                    )?;
                }

                let statements =
                    self.analyze_statements_in_function(&function_data.body, &return_type)?;

                let internal = InternalFunctionDefinition {
                    signature: Signature {
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
                        resolved_type,
                        is_mutable: found_self.is_mutable.is_some(),
                        node: Option::from(ParameterNode {
                            name: self.to_node(&found_self.self_node),
                            is_mutable: self.to_node_option(&found_self.is_mutable),
                        }),
                    });
                }

                // Handle parameters, including self if present
                for param in &signature.params {
                    let resolved_type = self.analyze_type(&param.param_type)?;

                    parameters.push(TypeForParameter {
                        name: self.get_text(&param.variable.name).to_string(),
                        resolved_type,
                        is_mutable: param.variable.is_mutable.is_some(),
                        node: Option::from(ParameterNode {
                            name: self.to_node(&param.variable.name),
                            is_mutable: self.to_node_option(&param.variable.is_mutable),
                        }),
                    });
                }

                let return_type = self.analyze_maybe_type(&signature.return_type)?;

                let external = ExternalFunctionDefinition {
                    assigned_name: self.get_text(&signature.name).to_string(),
                    name: Some(self.to_node(&signature.name)),
                    signature: Signature {
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
