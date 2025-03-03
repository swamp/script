/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::Analyzer;
use crate::err::Error;
use std::rc::Rc;
use swamp_script_semantic::{
    ArrayType, ArrayTypeRef, MapType, MapTypeRef, Signature, TupleType, Type, TypeForParameter,
};

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
            swamp_script_ast::Type::AnonymousStruct(ast_struct) => {
                let struct_ref = self.analyze_anonymous_struct_type(ast_struct)?;
                Type::AnonymousStruct(struct_ref.into())
            }
            swamp_script_ast::Type::Array(ast_type) => {
                Type::Array(self.analyze_array_type(ast_type)?)
            }
            swamp_script_ast::Type::Map(key_type, value_type) => {
                Type::Map(self.analyze_map_type(key_type, value_type)?)
            }
            swamp_script_ast::Type::Tuple(types) => {
                Type::Tuple(TupleType(self.analyze_types(types)?).into())
            }
            swamp_script_ast::Type::Generic(base_type, generic_types) => {
                let base_type = self.analyze_type(base_type)?;
                Type::Generic(Box::new(base_type), self.analyze_types(generic_types)?)
            }
            swamp_script_ast::Type::Enum(_) => todo!(),
            swamp_script_ast::Type::Named(ast_type_reference) => {
                self.analyze_named_type(ast_type_reference)?
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
}
