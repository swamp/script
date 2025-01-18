/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::ResolveError;
use crate::Resolver;
use std::rc::Rc;
use swamp_script_ast::{QualifiedTypeIdentifier, Type, TypeForParameter};
use swamp_script_semantic::{
    FunctionTypeSignature, ResolvedArrayType, ResolvedArrayTypeRef, ResolvedMapType,
    ResolvedMapTypeRef, ResolvedStructTypeRef, ResolvedTupleType, ResolvedType,
    ResolvedTypeForParameter,
};

impl<'a> Resolver<'a> {
    /// # Errors
    ///
    pub fn resolve_map_type(
        &mut self,
        ast_key_type: &Type,
        ast_value_type: &Type,
    ) -> Result<ResolvedMapTypeRef, ResolveError> {
        // TODO: Check for an existing map type with exact same type

        let key_type = self.resolve_type(ast_key_type)?;
        let value_type = self.resolve_type(ast_value_type)?;

        let original_map_type = ResolvedMapType {
            key_type,
            value_type,
        };

        let map_type_ref = Rc::new(original_map_type);

        //self.state.map_types.push(rc_array.clone());

        Ok(map_type_ref)
    }

    fn find_type_reference(
        &self,
        type_name_to_find: &QualifiedTypeIdentifier,
    ) -> Result<ResolvedType, ResolveError> {
        let (path, text) = self.get_path(type_name_to_find);

        let resolved_type = if let Some(found) = self.shared.lookup.get_struct(&path, &text) {
            ResolvedType::Struct(found)
        } else if let Some(found) = self.shared.lookup.get_enum(&path, &text) {
            ResolvedType::Enum(found)
        } else if let Some(found) = self.shared.lookup.get_rust_type(&path, &text) {
            ResolvedType::RustType(found)
        } else {
            Err(ResolveError::UnknownTypeReference(
                self.to_node(&type_name_to_find.name.0),
            ))?
        };

        Ok(resolved_type)
    }

    /// # Errors
    ///
    pub fn find_struct_type(
        &self,
        type_name: &QualifiedTypeIdentifier,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        let (path, name_string) = self.get_path(type_name);

        self.shared
            .lookup
            .get_struct(&path, &name_string)
            .map_or_else(
                || {
                    let resolved_node = self.to_node(&type_name.name.0);
                    Err(ResolveError::UnknownStructTypeReference(resolved_node))
                },
                Ok,
            )
    }

    /// # Errors
    ///
    pub fn resolve_array_type(
        &mut self,
        ast_type: &Type,
    ) -> Result<ResolvedArrayTypeRef, ResolveError> {
        // TODO: Check for an existing array type with exact same type

        let resolved_type = self.resolve_type(ast_type)?;

        let original_array_type = ResolvedArrayType {
            item_type: resolved_type,
        };

        let rc_array = Rc::new(original_array_type);

        self.shared.state.array_types.push(rc_array.clone());

        Ok(rc_array)
    }

    /// # Errors
    ///
    pub fn resolve_type(&mut self, ast_type: &Type) -> Result<ResolvedType, ResolveError> {
        let resolved = match ast_type {
            Type::Any(_) => ResolvedType::Any,
            Type::Int(_) => ResolvedType::Int,
            Type::Float(_) => ResolvedType::Float,
            Type::String(_) => ResolvedType::String,
            Type::Bool(_) => ResolvedType::Bool,
            Type::Unit(_) => ResolvedType::Unit,
            Type::Struct(ast_struct) => {
                let struct_ref = self.get_struct_type(ast_struct)?;
                ResolvedType::Struct(struct_ref)
            }
            Type::Array(ast_type) => ResolvedType::Array(self.resolve_array_type(ast_type)?),
            Type::Map(key_type, value_type) => {
                ResolvedType::Map(self.resolve_map_type(key_type, value_type)?)
            }
            Type::Tuple(types) => {
                ResolvedType::Tuple(ResolvedTupleType(self.resolve_types(types)?).into())
            }
            Type::Generic(base_type, generic_types) => {
                let base_type = self.resolve_type(base_type)?;
                ResolvedType::Generic(Box::new(base_type), self.resolve_types(generic_types)?)
            }
            Type::Enum(_) => todo!(),
            Type::TypeReference(ast_type_reference) => {
                self.find_type_reference(ast_type_reference)?
            }
            Type::Optional(inner_type_ast, _node) => {
                let inner_resolved_type = self.resolve_type(inner_type_ast)?;
                ResolvedType::Optional(Box::from(inner_resolved_type))
            }
            Type::Function(parameters, return_type) => {
                let parameter_types = self.resolve_param_types(parameters)?;

                let resolved_return_type = self.resolve_type(return_type)?;
                ResolvedType::Function(FunctionTypeSignature {
                    first_parameter_is_self: false,
                    parameters: parameter_types,
                    return_type: Box::new(resolved_return_type),
                })
            }
        };

        Ok(resolved)
    }

    pub(crate) fn resolve_types(
        &mut self,
        types: &[Type],
    ) -> Result<Vec<ResolvedType>, ResolveError> {
        let mut resolved_types = Vec::new();
        for some_type in types {
            resolved_types.push(self.resolve_type(some_type)?);
        }
        Ok(resolved_types)
    }

    fn resolve_param_types(
        &mut self,
        type_for_parameters: &Vec<TypeForParameter>,
    ) -> Result<Vec<ResolvedTypeForParameter>, ResolveError> {
        let mut vec = Vec::new();
        for x in type_for_parameters {
            vec.push(ResolvedTypeForParameter {
                name: String::new(),
                resolved_type: self.resolve_type(&x.ast_type)?,
                is_mutable: x.is_mutable,
                node: None,
            });
        }

        Ok(vec)
    }
}
