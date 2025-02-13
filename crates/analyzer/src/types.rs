/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::err::{Error, ErrorKind};
use crate::Resolver;
use std::rc::Rc;

use swamp_script_semantic::{
    ArrayType, ArrayTypeRef, ExternalType, ExternalTypeRef, MapType, MapTypeRef, Signature,
    StructTypeRef, TupleType, Type, TypeForParameter,
};

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
        let (path, text) = self.get_path(type_name_to_find);

        if type_name_to_find.generic_params.is_empty() {
            let resolved_type = if let Some(found) =
                self.shared.lookup.get_alias_referred_type(&path, &text)
            {
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
            self.concretize(&type_name_to_find)
        }
    }

    /// # Errors
    ///
    pub fn find_struct_type(
        &self,
        type_name: &swamp_script_ast::QualifiedTypeIdentifier,
    ) -> Result<StructTypeRef, Error> {
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
                self.concretize(type_identifier_with_params)?
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

    fn concretize(
        &mut self,
        parameterize_definition: &swamp_script_ast::QualifiedTypeIdentifier,
    ) -> Result<Type, Error> {
        let base_name = self.get_text(&parameterize_definition.name.0);
        let path = self.get_module_path(&parameterize_definition.module_path);
        let type_generator = self
            .shared
            .lookup
            .get_type_generator(&path, base_name)
            .ok_or_else(|| {
                self.create_err(
                    ErrorKind::NotATypeGenerator,
                    &parameterize_definition.name.0,
                )
            })?;

        let mut type_params = Vec::new();
        for type_param in &parameterize_definition.generic_params {
            type_params.push(self.analyze_type(type_param)?);
        }

        Ok(type_generator
            .generate_type(
                &mut self.shared.lookup.own_namespace().borrow_mut(),
                self.shared.lookup.modules(),
                type_params,
            )
            .expect("TODO: panic message"))
    }
}
