/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::{Error, ErrorKind};
use crate::lookup::TypeParameter;
use crate::Analyzer;
use seq_map::SeqMap;
use std::rc::Rc;
use swamp_script_modules::symtbl::{GenericAwareType, GenericTypeRef, SymbolTable};
use swamp_script_semantic::{
    ArrayType, ArrayTypeRef, ExternalType, ExternalTypeRef, MapType, MapTypeRef, Signature,
    TupleType, Type, TypeForParameter,
};
use tracing::info;

impl<'a> Analyzer<'a> {
    /// # Errors
    ///
    pub fn analyze_map_type(
        &mut self,
        ast_key_type: &swamp_script_ast::Type,
        ast_value_type: &swamp_script_ast::Type,
    ) -> Result<MapTypeRef, Error> {
        // TODO: Check for an existing map type with exact same type

        let key_type = self.analyze_type(ast_key_type)?;
        let value_type = self.analyze_type(ast_value_type)?;

        let original_map_type = MapType {
            key_type,
            value_type,
        };

        let map_type_ref = Rc::new(original_map_type);

        //self.state.map_types.push(rc_array.clone());

        Ok(map_type_ref)
    }

    pub(crate) fn get_type(
        &mut self,
        type_name_to_find: &swamp_script_ast::QualifiedTypeIdentifier,
    ) -> Result<Type, Error> {
        //let (module, text) = self.get_module(type_name_to_find)?;
        //info!(?text, ?module, "looking for named type");
        if type_name_to_find.generic_params.is_empty() {
            let (path, name) = self.get_path(type_name_to_find);
            if path.is_empty() {
                if let Some(found_type) = self.shared.type_parameter_scope_stack.get(&name) {
                    return Ok(found_type);
                }
            }
            let symbol_table = self.shared.get_symbol_table(&path);
            symbol_table.map_or_else(
                || Err(self.create_err(ErrorKind::UnknownSymbol, &type_name_to_find.name.0)),
                |found_table| {
                    found_table.get_type(&name).map_or_else(|| Err(self.create_err(ErrorKind::UnknownSymbol, &type_name_to_find.name.0)), |found| Ok(found.clone()))
                },
            )
        } else {
            /*
            let (base_type, params) = self.create_type_parameters(type_name_to_find)?;
            let first_name = params.iter().next().unwrap().0.clone();
            let resolved_type = if let Some((scope_index, index)) =
                self.shared.lookup.get_type_parameter_reference(&first_name) {
                if let Some(found_generic) = self.shared.lookup.get_generic(&path, &*text) {
                    Ok(Refound_generic)
                } else {
                    Err(self.create_err(ErrorKind::NotAGeneric, &type_name_to_find.name.0))
                }
            } else {

             */
            self.monomorphize(type_name_to_find)
            //}
        }
    }

    /*
    /// # Errors
    ///
    pub fn find_struct_type(
        &mut self,
        type_name: &swamp_script_ast::QualifiedTypeIdentifier,
    ) -> Result<StructTypeRef, Error> {
        if !type_name.generic_params.is_empty() {
            let ty = self.monomorphize(type_name)?;
            return if let Type::Struct(struct_ref) = ty {
                Ok(struct_ref)
            } else {
                Err(self.create_err(ErrorKind::ExpectedStruct, &type_name.name.0))
            };
        }
        let (path, name_string) = self.get_path(type_name);

        self.shared
            .lookup
            .get_struct(&path, &name_string)
            .map_or_else(
                || {
                    let resolved_node = self.to_node(&type_name.name.0);
                    Err(self
                        .create_err_resolved(ErrorKind::UnknownStructTypeReference, &resolved_node))
                },
                Ok,
            )
    }

     */

    /// # Errors
    ///
    pub fn analyze_array_type(
        &mut self,
        ast_type: &swamp_script_ast::Type,
    ) -> Result<ArrayTypeRef, Error> {
        // TODO: Check for an existing array type with exact same type

        let resolved_type = self.analyze_type(ast_type)?;

        let original_array_type = ArrayType {
            item_type: resolved_type,
        };

        let rc_array = Rc::new(original_array_type);

        self.shared.state.array_types.push(rc_array.clone());

        Ok(rc_array)
    }

    /// # Errors
    ///
    pub fn analyze_type(&mut self, ast_type: &swamp_script_ast::Type) -> Result<Type, Error> {
        let resolved = match ast_type {
            swamp_script_ast::Type::Unit(_) => Type::Unit,
            /*
            swamp_script_ast::Type::Struct(ast_struct) => {
                let text = self.get_text(&ast_struct.name.0);
                let struct_ref = self.analyze_struct_type(text, ast_struct)?;
                Type::Struct(StructTypeRef::from(struct_ref))
            }

             */
            swamp_script_ast::Type::Array(ast_type) => {
                Type::Array(self.analyze_array_type(ast_type)?)
            }
            swamp_script_ast::Type::Map(key_type, value_type) => {
                Type::Map(self.analyze_map_type(key_type, value_type)?)
            }
            swamp_script_ast::Type::External(node) => {
                // TODO: Fix this. Generate unique number
                let external = ExternalType {
                    type_name: self.get_text(node).to_string(),
                    number: 0,
                };
                Type::External(ExternalTypeRef::from(external))
            }
            swamp_script_ast::Type::Tuple(types) => {
                Type::Tuple(TupleType(self.analyze_types(types)?).into())
            }
            /*
            swamp_script_ast::Type::Generic(type_identifier_with_params) => {
                self.monomorphize(type_identifier_with_params)?
            }

             */
            swamp_script_ast::Type::Enum(_) => todo!(),
            swamp_script_ast::Type::Named(ast_type_reference) => {
                self.get_type(ast_type_reference)?
            }
            swamp_script_ast::Type::Optional(inner_type_ast, _node) => {
                let inner_resolved_type = self.analyze_type(inner_type_ast)?;
                Type::Optional(Box::from(inner_resolved_type))
            }
            swamp_script_ast::Type::Function(parameters, return_type) => {
                let parameter_types = self.analyze_param_types(parameters)?;

                let resolved_return_type = self.analyze_type(return_type)?;
                Type::Function(Signature {
                    parameters: parameter_types,
                    return_type: Box::new(resolved_return_type),
                })
            }
        };

        Ok(resolved)
    }

    pub(crate) fn analyze_types(
        &mut self,
        types: &[swamp_script_ast::Type],
    ) -> Result<Vec<Type>, Error> {
        let mut resolved_types = Vec::new();
        for some_type in types {
            resolved_types.push(self.analyze_type(some_type)?);
        }
        Ok(resolved_types)
    }

    fn analyze_param_types(
        &mut self,
        type_for_parameters: &Vec<swamp_script_ast::TypeForParameter>,
    ) -> Result<Vec<TypeForParameter>, Error> {
        let mut vec = Vec::new();
        for x in type_for_parameters {
            vec.push(TypeForParameter {
                name: String::new(),
                resolved_type: self.analyze_type(&x.ast_type)?,
                is_mutable: x.is_mutable,
                node: None,
            });
        }

        Ok(vec)
    }

    fn get_generic(
        &self,
        parameterize_definition: &swamp_script_ast::QualifiedTypeIdentifier,
    ) -> Result<(SymbolTable, GenericTypeRef, Vec<String>), Error> {
        let (canonical_path, _name) = self.get_canonical_path_and_name(parameterize_definition)?;
        let (symbol_table, base_name) = self.get_symbol_table_and_name(parameterize_definition)?;
        symbol_table.get_generic(&base_name).map_or_else(
            || {
                Err(self.create_err(
                    ErrorKind::UnknownTypeReference,
                    &parameterize_definition.name.0,
                ))
            },
            |generic| Ok((symbol_table.clone(), generic.clone(), canonical_path)),
        )
    }

    pub fn monomorphize(
        &mut self,
        parameterize_definition: &swamp_script_ast::QualifiedTypeIdentifier,
    ) -> Result<Type, Error> {
        let (symbol_table_generic_is_in, found_generic_ref, canonical_path) =
            self.get_generic(parameterize_definition)?;
        let found_generic = found_generic_ref.borrow();

        if found_generic.type_parameters.len() != parameterize_definition.generic_params.len() {
            return Err(self.create_err(
                ErrorKind::WrongNumberOfTypeArguments(0, 0),
                &parameterize_definition.name.0,
            ));
        }

        let mut type_params = SeqMap::new();
        for ((key, type_param_name), ast_type) in found_generic
            .type_parameters
            .iter()
            .zip(&parameterize_definition.generic_params)
        {
            type_params
                .insert(
                    key.to_string(),
                    TypeParameter {
                        ty: self.analyze_type(ast_type)?,
                        debug_name: type_param_name.assigned_name.clone(),
                    },
                )
                .expect("todo");
        }

        let types_vec: Vec<_> = type_params
            .iter()
            .map(|(_key, type_parameter)| type_parameter.ty.clone())
            .collect();

        let base_name = self.get_text(&parameterize_definition.name.0).to_string();
        let monomorphization_name = SymbolTable::get_monomorphization_name(&base_name, &types_vec);
        info!(?monomorphization_name, "------ MONOMORPHIZE");

        if let Some(monomorphized_struct_ref) =
            self.shared
                .state
                .monomorphization_cache
                .get(&canonical_path, &base_name, &types_vec)
        {
            info!(monomorphization_name, "fetching type from cache");
            Ok(monomorphized_struct_ref.clone())
        } else {
            self.shared
                .type_parameter_scope_stack
                .push_type_parameters(type_params);
            let stored_file_id = self.shared.file_id;
            let saved_lookup_table = self.shared.lookup_table.clone();
            self.shared.file_id = found_generic.file_id;
            self.shared
                .lookup_table
                .clone_from(&symbol_table_generic_is_in);

            // Struct ------------------
            let GenericAwareType::Struct(base_ast_type) = &found_generic.base_type;
            let analyzed_base_type = self
                .analyze_struct_type(&monomorphization_name, base_ast_type)
                .expect("TODO: handle panic message");
            info!(?monomorphization_name, "inserted monomorphized type");

            let created_type = Type::Struct(Rc::new(analyzed_base_type));

            self.shared
                .state
                .monomorphization_cache
                .add(
                    &canonical_path,
                    &base_name,
                    created_type.clone(),
                    &types_vec,
                )
                .expect("TODO: panic message");

            // Functions ------------------

            let functions: Vec<&swamp_script_ast::Function> =
                found_generic.ast_functions.values().collect::<Vec<_>>();

            self.analyze_impl_functions(
                &parameterize_definition.name.0,
                &created_type,
                &functions,
            )?;

            // Pop the stack
            self.shared.type_parameter_scope_stack.pop_type_parameters();
            self.shared.file_id = stored_file_id;
            self.shared.lookup_table = saved_lookup_table;

            Ok(created_type)
        }
    }
}
