/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::err::{Error, ErrorKind};
use crate::Analyzer;
use std::rc::Rc;
use swamp_script_modules::symtbl::{GeneratorKind, Symbol};
use swamp_script_semantic::{
    ExternalType, ExternalTypeRef, MapType, MapTypeRef, Signature, SparseType, TupleType, Type,
    TypeForParameter, VecType, VecTypeRef,
};

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
        let (path, name) = self.get_path(type_name_to_find);

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

        let mut analyzed_types = Vec::new();

        for analyzed_type in &type_name_to_find.generic_params {
            let ty = self.analyze_type(analyzed_type)?;

            analyzed_types.push(ty);
        }

        let result_type = match symbol {
            Symbol::Type(base_type) => base_type,

            Symbol::Alias(alias_type) => alias_type.referenced_type.clone(),
            Symbol::TypeGenerator(generator) => {
                if analyzed_types.len() != generator.arity {
                    return Err(self.create_err(
                        ErrorKind::WrongNumberOfTypeArguments(
                            generator.arity,
                            analyzed_types.len(),
                        ),
                        &type_name_to_find.name.0,
                    ));
                }
                match generator.kind {
                    GeneratorKind::Slice => Type::Slice(Box::from(analyzed_types[0].clone())),
                    GeneratorKind::SlicePair => Type::SlicePair(
                        Box::from(analyzed_types[0].clone()),
                        Box::from(analyzed_types[1].clone()),
                    ),
                    GeneratorKind::Sparse => Type::Sparse(Rc::new(SparseType {
                        key_type: analyzed_types[0].clone(),
                        value_type: analyzed_types[1].clone(),
                    })),
                }
            }
            _ => return Err(self.create_err(ErrorKind::UnknownSymbol, &type_name_to_find.name.0)),
        };

        Ok(result_type)
    }

    /*
    if is_parameterized {
        info!(?base_type, ?name, "parameterized");

        let kind: ParameterizedTypeKind = match base_type {
            Type::Struct(struct_ref) => ParameterizedTypeKind::Struct(struct_ref),
            Type::Enum(enum_ref) => ParameterizedTypeKind::Enum(enum_ref),
            Type::Parameterized(inner) => {
                ParameterizedTypeKind::Parameterized(Box::new(inner))
            }
            _ => {
                return Err(
                    self.create_err(ErrorKind::NotAGeneric, &type_name_to_find.name.0)
                );
            }
        };

        let parameterized_type = ParameterizedType {
            base: kind,
            parameters: vec![],
        };
        let result_type = Type::Parameterized(parameterized_type);
        info!(?result_type, "parameterized resolution");
        result_type
    } else {
        info!(?name, "base_type");
        base_type
    }
                     */

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
    ) -> Result<VecTypeRef, Error> {
        // TODO: Check for an existing array type with exact same type

        let resolved_type = self.analyze_type(ast_type)?;

        let original_array_type = VecType {
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
            swamp_script_ast::Type::Array(_ast_type) => {
                todo!() //Type::Array(self.analyze_array_type(ast_type)?)
            }
            swamp_script_ast::Type::Map(_key_type, _value_type) => {
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
