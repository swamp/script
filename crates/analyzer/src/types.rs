/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Analyzer;
use crate::err::{Error, ErrorKind};
use seq_map::SeqMap;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_modules::symtbl::{GeneratorKind, Symbol, SymbolTable};
use swamp_script_semantic::{
    ArrayType, ArrayTypeRef, EnumTypeRef, ExternalType, ExternalTypeRef, MapType, MapTypeRef,
    ParameterizedType, Signature, TupleType, Type, TypeForParameter,
};
use tracing::info;

impl Analyzer<'_> {
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

    pub(crate) fn analyze_named_type(
        &mut self,
        type_name_to_find: &swamp_script_ast::QualifiedTypeIdentifier,
    ) -> Result<Type, Error> {
        //let (module, text) = self.get_module(type_name_to_find)?;
        //info!(?text, ?module, "looking for named type");

        let (path, name) = self.get_path(type_name_to_find);
        if path.is_empty() {
            info!(?name, "checking potential type variable");
            if let Some(found_type) = self.shared.type_variables_stack.get(&name) {
                return Ok(found_type);
            }
        }
        let symbol = {
            let maybe_symbol_table = self.shared.get_symbol_table(&path);
            let symbol_table = maybe_symbol_table.ok_or_else(|| {
                self.create_err(ErrorKind::UnknownSymbol, &type_name_to_find.name.0)
            })?;
            symbol_table
                .get_symbol(&name)
                .ok_or_else(|| {
                    self.create_err(ErrorKind::UnknownSymbol, &type_name_to_find.name.0)
                })?
                .clone()
        };

        let is_parameterized = !type_name_to_find.generic_params.is_empty();

        let mut analyzed_types = Vec::new();

        for analyzed_type in &type_name_to_find.generic_params {
            let ty = self.analyze_type(analyzed_type)?;

            analyzed_types.push(ty);
        }

        let result_type = match symbol {
            Symbol::Type(base_type) => {
                let checked_base_type: ParameterizedType = match base_type {
                    Type::Struct(struct_ref) => ParameterizedType::Struct(struct_ref),
                    Type::Enum(enum_ref) => ParameterizedType::Enum(enum_ref),
                    Type::Parameterized(_, _) => {
                        ParameterizedType::Parameterized(Box::new(base_type.clone()))
                    }
                    _ => {
                        return Err(
                            self.create_err(ErrorKind::NotAGeneric, &type_name_to_find.name.0)
                        );
                    }
                };
                Type::Parameterized(checked_base_type, analyzed_types)
            }
            Symbol::Alias(alias_type) => alias_type.referenced_type.clone(),
            Symbol::TypeGenerator(generator) => {
                if analyzed_types.len() != generator.arity {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfTypeArguments(1, analyzed_types.len()),
                        &type_name_to_find.name.0,
                    ));
                }
                match generator.kind {
                    GeneratorKind::Slice => Type::Slice(Box::from(analyzed_types[0].clone())),
                    GeneratorKind::SlicePair => Type::SlicePair(
                        Box::from(analyzed_types[0].clone()),
                        Box::from(analyzed_types[1].clone()),
                    ),
                }
            }
            _ => return Err(self.create_err(ErrorKind::UnknownSymbol, &type_name_to_find.name.0)),
        };

        Ok(result_type)
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
                todo!() //Type::Array(self.analyze_array_type(ast_type)?)
            }
            swamp_script_ast::Type::Map(key_type, value_type) => {
                todo!() //Type::Map(self.analyze_map_type(key_type, value_type)?)
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
