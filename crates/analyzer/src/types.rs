/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::{Error, ErrorKind};
use crate::lookup::TypeParameter;
use crate::Resolver;
use seq_map::SeqMap;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_modules::ns::{GenericAwareType, GenericTypeRef, ModuleNamespace};
use swamp_script_semantic::{
    ArrayType, ArrayTypeRef, ExternalType, ExternalTypeRef, Function, MapType, MapTypeRef,
    Signature, StructTypeRef, TupleType, Type, TypeForParameter,
};
use tracing::info;

impl<'a> Resolver<'a> {
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

    pub(crate) fn find_named_type(
        &mut self,
        type_name_to_find: &swamp_script_ast::QualifiedTypeIdentifier,
    ) -> Result<Type, Error> {
        let (path, text) = self.get_full_path(type_name_to_find);
        info!(?text, ?path, "looking for named type");
        if type_name_to_find.generic_params.is_empty() {
            let resolved_type = if let Some(found_type_param) =
                self.shared.lookup.get_type_parameter(&text)
            {
                found_type_param.ty.clone()
            } else if let Some(found) = self.shared.lookup.get_alias_referred_type(&path, &text) {
                found
            } else if let Some(found) = self.shared.lookup.get_struct(&path, &text) {
                Type::Struct(found)
            } else if let Some(found) = self.shared.lookup.get_enum(&path, &text) {
                Type::Enum(found)
            } else if let Some(found) = self.shared.lookup.get_rust_type(&path, &text) {
                Type::External(found)
            } else {
                Err(self.create_err(ErrorKind::UnknownTypeReference, &type_name_to_find.name.0))?
            };

            Ok(resolved_type)
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

    pub fn find_local_impl_target(
        &mut self,
        type_name: &swamp_script_ast::LocalTypeIdentifierWithOptionalTypeParams,
    ) -> Result<(StructTypeRef, bool), Error> {
        let name_string = self.get_text(&type_name.name).to_string();

        self.shared
            .lookup
            .get_struct(&[], &name_string)
            .map_or_else(
                || {
                    let resolved_node = self.to_node(&type_name.name);
                    Err(self
                        .create_err_resolved(ErrorKind::UnknownStructTypeReference, &resolved_node))
                },
                |x| Ok((x, false)),
            )
    }

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
            swamp_script_ast::Type::Int(_) => Type::Int,
            swamp_script_ast::Type::Float(_) => Type::Float,
            swamp_script_ast::Type::String(_) => Type::String,
            swamp_script_ast::Type::Bool(_) => Type::Bool,
            swamp_script_ast::Type::Unit(_) => Type::Unit,
            swamp_script_ast::Type::Struct(ast_struct) => {
                let struct_ref = self.get_struct_type(ast_struct)?;
                Type::Struct(struct_ref)
            }
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
            swamp_script_ast::Type::Generic(type_identifier_with_params) => {
                self.monomorphize(type_identifier_with_params)?
            }
            swamp_script_ast::Type::Enum(_) => todo!(),
            swamp_script_ast::Type::Named(ast_type_reference) => {
                self.find_named_type(ast_type_reference)?
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

    /*
    pub fn specialize_struct_type(
        &mut self,
        name: &str,
        struct_type: &StructTypeRef,
    ) -> Result<StructTypeRef, Error> {
        let mut new_fields = SeqMap::new();
        let borrow = struct_type.as_ref().borrow();
        let fields = &borrow.anon_struct_type.defined_fields;

        for (name, field) in fields {
            let result_type = if let Type::GenericTypeParameter(scope, index) = field.field_type {
                let found_type_param = self
                    .shared
                    .lookup
                    .lookup_type_parameter(scope, index)
                    .clone();
                info!(
                    ?found_type_param,
                    "cool, I found a generic type reference, replacing it"
                );
                found_type_param.ty.clone()
            } else {
                field.field_type.clone()
            };
            new_fields
                .insert(
                    name.clone(),
                    StructTypeField {
                        identifier: field.identifier.clone(),
                        field_type: result_type,
                    },
                )
                .unwrap();
        }

        let new_struct = StructType {
            name: Node::new_unknown(),
            assigned_name: name.to_string(),
            anon_struct_type: AnonymousStructType {
                defined_fields: new_fields,
            },
            functions: SeqMap::default(),
        };

        info!(?name, "specialized done");

        Ok(Rc::new(RefCell::new(new_struct)))
    }

     */

    fn create_type_parameters(
        &mut self,
        parameterize_definition: &swamp_script_ast::QualifiedTypeIdentifier,
    ) -> Result<GenericTypeRef, Error> {
        let found_generic = {
            let path = self.get_module_path(&parameterize_definition.module_path);
            let base_name = self.get_text(&parameterize_definition.name.0).to_string();

            let found_generic = match self.shared.lookup.get_generic(&path, &*base_name) {
                Some(generic) => generic,
                None => {
                    return Err(self.create_err(
                        ErrorKind::UnknownTypeReference,
                        &parameterize_definition.name.0,
                    ))
                }
            };

            found_generic
        };

        Ok(found_generic)
    }

    fn monomorphize(
        &mut self,
        parameterize_definition: &swamp_script_ast::QualifiedTypeIdentifier,
    ) -> Result<Type, Error> {
        let found_generic_ref = self.create_type_parameters(parameterize_definition)?;
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
        let monomorphization_name =
            ModuleNamespace::get_monomorphization_name(&base_name, &types_vec);
        info!(?monomorphization_name, "------ MONOMORPHIZE");

        if let Some(monomorphized_struct_ref) = self
            .shared
            .lookup
            .get_struct(&found_generic.defined_in_path, &monomorphization_name)
        {
            info!(monomorphization_name, "fetching type from cache");
            Ok(Type::Struct(monomorphized_struct_ref))
        } else {
            self.shared.lookup.push_type_parameter_scope(type_params);
            let stored_file_id = self.shared.file_id;
            let stored_default_path = self.shared.lookup.default_path.clone();
            self.shared.file_id = found_generic.file_id;
            self.shared
                .lookup
                .default_path
                .clone_from(&found_generic.defined_in_path.clone());
            info!(path=?self.shared.lookup.default_path, "generic was in path");

            // Struct ------------------
            let GenericAwareType::Struct(base_ast_type) = &found_generic.base_type;
            let analyzed_base_type = self
                .analyze_struct_type(&monomorphization_name, base_ast_type)
                .expect("TODO: handle panic message");
            info!(?monomorphization_name, "inserted monomorphized type");

            let analyzed_base_type_ref = self
                .shared
                .lookup
                .add_generated_struct(&self.shared.lookup.default_path, analyzed_base_type)
                .expect("TODO: panic message");

            let created_type = Type::Struct(analyzed_base_type_ref.clone());

            // Functions ------------------

            let functions: Vec<&swamp_script_ast::Function> =
                found_generic.ast_functions.values().collect::<Vec<_>>();

            self.analyze_impl_functions_to_struct(
                &parameterize_definition.name.0,
                analyzed_base_type_ref,
                &functions,
            )?;

            // Pop the stack
            self.shared.lookup.pop_type_parameter_scope();
            self.shared.file_id = stored_file_id;
            self.shared.lookup.default_path = stored_default_path;

            Ok(created_type)
        }
    }
}
