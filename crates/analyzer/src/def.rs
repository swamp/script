/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use crate::err::{Error, ErrorKind};
use seq_map::SeqMap;
use std::rc::Rc;
use swamp_script_semantic::{
    AliasType, AliasTypeRef, AnonymousStructType, EnumType, EnumTypeRef, EnumVariantCommon,
    EnumVariantSimpleType, EnumVariantSimpleTypeRef, EnumVariantStructType, EnumVariantTupleType,
    EnumVariantType, ExternalFunctionDefinition, Function, InternalFunctionDefinition,
    LocalIdentifier, LocalTypeIdentifier, ParameterNode, Signature, StructType, StructTypeField,
    StructTypeRef, Type, TypeForParameter, TypeVariable, UseItem,
};

impl Analyzer<'_> {
    fn analyze_use_definition(
        &mut self,
        use_definition: &swamp_script_ast::Use,
    ) -> Result<(), Error> {
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
                .add_module_link(last_name, found_module.clone())?;
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
                            .add_symbol(&ident_text, found_symbol.clone())?;
                    } else {
                        return Err(self.create_err_resolved(
                            ErrorKind::UnknownTypeReference,
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
                            .add_symbol(&ident_text, found_symbol.clone())?;
                    } else {
                        return Err(self.create_err_resolved(
                            ErrorKind::UnknownTypeReference,
                            &ident_resolved_node,
                        ));
                    }
                    UseItem::TypeIdentifier(self.to_node(&node.0))
                }
            };
        }

        Ok(())
    }

    fn analyze_mod_definition(
        &mut self,
        mod_definition: &swamp_script_ast::Mod,
    ) -> Result<(), Error> {
        let mut path = Vec::new();
        for ast_node in &mod_definition.module_path.0 {
            path.push(self.get_text(ast_node).to_string());
        }

        let mut nodes_copy = path.clone();
        nodes_copy.insert(0, "crate".to_string());

        if let Some(found_namespace) = self.shared.modules.get(&nodes_copy) {
            self.shared
                .lookup_table
                .add_module_link(nodes_copy.last().unwrap(), found_namespace.clone())?;
            Ok(())
        } else {
            let first = &mod_definition.module_path.0[0];
            Err(self.create_err(ErrorKind::UnknownModule, first))
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
            number: parent_number,
            variants: SeqMap::default(),
        };

        let parent_ref = self.shared.definition_table.add_enum_type(enum_parent)?;
        self.shared
            .lookup_table
            .add_enum_type_link(parent_ref.clone())?;

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

        let resolved_alias_ref = self.shared.definition_table.add_alias(resolved_alias)?;
        self.shared
            .lookup_table
            .add_alias_link(resolved_alias_ref.clone())?;

        Ok(resolved_alias_ref)
    }

    pub fn analyze_struct_type(
        &mut self,
        assigned_name: &str,
        ast_struct: &swamp_script_ast::StructType,
    ) -> Result<StructType, Error> {
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

        let unique_id = self.shared.state.allocate_number();
        let resolved_struct = StructType::new(
            self.to_node(&ast_struct.identifier.name),
            assigned_name,
            unique_id,
            resolved_anon_struct,
        );

        Ok(resolved_struct)
    }

    pub fn monomorphize_struct(
        &mut self,
        assigned_name: &str,
        generic_ast_struct: &swamp_script_ast::StructType,
        functions: &[swamp_script_ast::Function],
    ) -> Result<StructTypeRef, Error> {
        let struct_type = self.analyze_struct_type(assigned_name, generic_ast_struct)?;
        let struct_type_ref = Rc::new(struct_type);

        Ok(struct_type_ref)
    }

    pub(crate) fn monomorphize_enum(
        &self,
        assigned_name: &str,
        variants: &[swamp_script_ast::EnumVariantType],
    ) -> Result<EnumType, Error> {
        todo!()
    }

    /*
    pub fn store_generic_struct(
        &mut self,
        ast_struct: &swamp_script_ast::StructType,
    ) -> Result<(), Error> {
        let struct_name_str = self.get_text(&ast_struct.identifier.name).to_string();

        let mut parameter_names = SeqMap::new();
        for name in &ast_struct.identifier.parameter_names {
            let assigned_name = self.get_text(name).to_string();
            parameter_names
                .insert(
                    assigned_name.clone(),
                    TypeParameterName {
                        resolved_node: self.to_node(name),
                        assigned_name: assigned_name.clone(),
                    },
                )
                .expect("TODO: panic message");
        }

        let type_id = self.shared.state.allocate_number();
        let generic_type = GenericType {
            type_parameters: parameter_names,
            base_type: ParameterizedType::Struct(ast_struct.clone()),
            stored_lookup_table: SymbolTable::new(),
            ast_functions: Vec::new(),
            canonical_path: self.shared.current_path.clone(),
            type_id,
            file_id: self.shared.file_id,
        };

        info!(struct_name_str, "add generic type");
        let generic_type_ref = self
            .shared
            .definition_table
            .add_generic(&struct_name_str, generic_type)?;

        self.shared
            .lookup_table
            .add_generic_link(&struct_name_str, generic_type_ref.clone())?;

        // Must save lookup table after `add_generic` so it includes itself
        generic_type_ref.borrow_mut().stored_lookup_table = self.shared.lookup_table.clone();

        Ok(())
    }

     */

    /// # Panics
    ///
    pub fn push_type_variables(&mut self, ast_nodes: &[swamp_script_ast::Node]) {
        let mut types = SeqMap::new();
        for ast_node in ast_nodes {
            let name = self.get_text(ast_node).to_string();
            let number = self.shared.state.allocate_number();
            types
                .insert(
                    name.to_string(),
                    Type::Variable(TypeVariable { name, number }),
                )
                .unwrap();
        }

        self.shared.type_variables_stack.push(types);
    }

    /// # Errors
    ///
    pub fn analyze_struct_type_definition(
        &mut self,
        ast_struct: &swamp_script_ast::StructType,
    ) -> Result<(), Error> {
        let is_parameterized = !ast_struct.identifier.parameter_names.is_empty();
        if is_parameterized {
            self.push_type_variables(&ast_struct.identifier.parameter_names);
        }

        let struct_name_str = self.get_text(&ast_struct.identifier.name).to_string();
        let analyzed_struct_ref = self.analyze_struct_type(&struct_name_str, ast_struct)?;

        if is_parameterized {
            self.shared.type_variables_stack.pop();
        }

        let struct_ref = self
            .shared
            .definition_table
            .add_struct(analyzed_struct_ref)?;

        self.shared.lookup_table.add_struct_link(struct_ref)?;

        Ok(())
    }

    pub(crate) fn analyze_function_definition(
        &mut self,
        function: &swamp_script_ast::Function,
    ) -> Result<Function, Error> {
        let func = match function {
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
                        param.node.as_ref().unwrap().is_mutable.as_ref(),
                        &param.resolved_type.clone(),
                    )?;
                }
                let function_name = self.get_text(&function_data.declaration.name).to_string();
                let statements =
                    self.analyze_function_body_expression(&function_data.body, &return_type)?;
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
                    .definition_table
                    .add_internal_function(&function_name, internal)?;

                self.shared
                    .lookup_table
                    .add_internal_function_link(&function_name, function_ref.clone())?;

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
                    .definition_table
                    .add_external_function_declaration(external)?;

                self.shared
                    .lookup_table
                    .add_external_function_declaration_link(function_ref.clone())?;

                Function::External(function_ref)
            }
        };

        Ok(func)
    }

    /// # Errors
    ///
    pub fn analyze_definition(
        &mut self,
        ast_def: &swamp_script_ast::Definition,
    ) -> Result<(), Error> {
        match ast_def {
            swamp_script_ast::Definition::StructDef(ast_struct) => {
                self.analyze_struct_type_definition(ast_struct)?;
            }

            swamp_script_ast::Definition::AliasDef(alias_def) => {
                self.analyze_alias_type_definition(alias_def)?;
            }
            swamp_script_ast::Definition::EnumDef(identifier, variants) => {
                self.analyze_enum_type_definition(identifier, variants)?;
            }
            swamp_script_ast::Definition::FunctionDef(function) => {
                let resolved_return_type = self.analyze_return_type(function)?;
                self.start_function(resolved_return_type);
                self.analyze_function_definition(function)?;
                self.stop_function();
            }
            swamp_script_ast::Definition::ImplDef(type_identifier, functions) => {
                self.analyze_impl_definition(type_identifier, functions)?;
            }
            swamp_script_ast::Definition::Use(use_info) => self.analyze_use_definition(use_info)?,
            swamp_script_ast::Definition::Mod(mod_info) => self.analyze_mod_definition(mod_info)?,
            swamp_script_ast::Definition::Constant(const_info) => {
                self.analyze_constant_definition(const_info)?;
            }
        };

        Ok(())
    }

    fn analyze_impl_definition(
        &mut self,
        attached_to_type: &swamp_script_ast::LocalTypeIdentifierWithOptionalTypeParams,
        functions: &Vec<swamp_script_ast::Function>,
    ) -> Result<(), Error> {
        let qualified = swamp_script_ast::QualifiedTypeIdentifier {
            name: swamp_script_ast::LocalTypeIdentifier(attached_to_type.name.clone()),
            module_path: None,
            generic_params: vec![],
        };

        let is_parameterized = !attached_to_type.parameter_names.is_empty();

        if is_parameterized {
            self.push_type_variables(&attached_to_type.parameter_names);
        }
        let type_to_attach_to = self.analyze_named_type(&qualified)?;
        let function_refs: Vec<&swamp_script_ast::Function> = functions.iter().collect();
        self.analyze_impl_functions(&type_to_attach_to, &function_refs)?;

        if is_parameterized {
            self.shared.type_variables_stack.pop();
        }

        Ok(())
    }

    pub fn analyze_impl_functions(
        &mut self,
        type_to_attach_to: &Type,
        functions: &[&swamp_script_ast::Function],
    ) -> Result<(), Error> {
        self.shared
            .state
            .associated_impls
            .prepare(type_to_attach_to);

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

            let resolved_function = self.analyze_impl_func(function, type_to_attach_to)?;

            let resolved_function_ref = Rc::new(resolved_function);

            self.stop_function();

            self.shared.state.associated_impls.add_member_function(
                type_to_attach_to,
                &function_name_str,
                resolved_function_ref,
            )?;
        }

        Ok(())
    }

    pub(crate) fn analyze_impl_func(
        &mut self,
        function: &swamp_script_ast::Function,
        self_type: &Type,
    ) -> Result<Function, Error> {
        let resolved_fn = match function {
            swamp_script_ast::Function::Internal(function_data) => {
                let mut parameters = Vec::new();

                if let Some(found_self) = &function_data.declaration.self_parameter {
                    parameters.push(TypeForParameter {
                        name: self.get_text(&found_self.self_node).to_string(),
                        resolved_type: self_type.clone(),
                        is_mutable: found_self.is_mutable.is_some(),
                        node: Option::from(ParameterNode {
                            name: self.to_node(&found_self.self_node),
                            is_mutable: self.to_node_option(Option::from(&found_self.is_mutable)),
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
                            is_mutable: self
                                .to_node_option(Option::from(&param.variable.is_mutable)),
                        }),
                    });
                }

                let return_type =
                    self.analyze_maybe_type(Option::from(&function_data.declaration.return_type))?;

                for param in &parameters {
                    self.create_local_variable_resolved(
                        &param.node.as_ref().unwrap().name,
                        param.node.as_ref().unwrap().is_mutable.as_ref(),
                        &param.resolved_type.clone(),
                    )?;
                }

                let statements =
                    self.analyze_function_body_expression(&function_data.body, &return_type)?;

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
                    parameters.push(TypeForParameter {
                        name: self.get_text(&found_self.self_node).to_string(),
                        resolved_type: self_type.clone(),
                        is_mutable: found_self.is_mutable.is_some(),
                        node: Option::from(ParameterNode {
                            name: self.to_node(&found_self.self_node),
                            is_mutable: self.to_node_option(Option::from(&found_self.is_mutable)),
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
                            is_mutable: self
                                .to_node_option(Option::from(&param.variable.is_mutable)),
                        }),
                    });
                }

                let return_type = self.analyze_maybe_type(Option::from(&signature.return_type))?;

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
