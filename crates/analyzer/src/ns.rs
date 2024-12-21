/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::ResolveError;
use seq_map::SeqMap;
use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;
use swamp_script_semantic::{
    ResolvedEnumTypeRef, ResolvedEnumVariantType, ResolvedEnumVariantTypeRef,
    ResolvedExternalFunctionDefinition, ResolvedExternalFunctionDefinitionRef,
    ResolvedInternalFunctionDefinition, ResolvedInternalFunctionDefinitionRef, ResolvedRustType,
    ResolvedRustTypeRef, ResolvedStructType, ResolvedStructTypeRef, ResolvedType, SemanticError,
};

#[derive(Debug, Clone)]
pub struct ResolvedModulePathStr(pub Vec<String>);

#[derive(Debug)]
pub struct ResolvedModuleNamespace {
    #[allow(unused)]
    structs: SeqMap<String, ResolvedStructTypeRef>,

    #[allow(unused)]
    build_in_rust_types: SeqMap<String, ResolvedRustTypeRef>,

    #[allow(unused)]
    enum_types: SeqMap<String, ResolvedEnumTypeRef>,
    enum_variant_types: SeqMap<String, ResolvedEnumVariantTypeRef>,

    internal_functions: SeqMap<String, ResolvedInternalFunctionDefinitionRef>,
    external_function_declarations: SeqMap<String, ResolvedExternalFunctionDefinitionRef>,

    #[allow(unused)]
    type_aliases: SeqMap<String, ResolvedType>,

    pub path: Vec<String>,
}
impl ResolvedModuleNamespace {
    #[must_use]
    pub fn new(path: &[String]) -> Self {
        Self {
            structs: Default::default(),
            build_in_rust_types: Default::default(),
            enum_types: Default::default(),
            enum_variant_types: Default::default(),
            internal_functions: Default::default(),
            external_function_declarations: Default::default(),
            type_aliases: Default::default(),
            path: path.to_vec(),
        }
    }

    pub fn add_struct(
        &mut self,
        name: &str,
        struct_type: ResolvedStructType,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        let struct_ref = Rc::new(RefCell::new(struct_type));
        self.structs.insert(name.to_string(), struct_ref.clone())?;

        Ok(struct_ref)
    }

    pub fn add_built_in_rust_type(
        &mut self,
        rust_type: ResolvedRustType,
    ) -> Result<ResolvedRustTypeRef, SemanticError> {
        let rust_type_ref = Rc::new(rust_type);
        /* TODO:
        self.build_in_rust_types.insert(
            LocalTypeName(rust_type_ref.type_name.clone()),
            rust_type_ref.clone(),
        )?;

         */

        Ok(rust_type_ref)
    }

    pub fn add_type_alias(
        &mut self,
        name: &str,
        _resolved_type: ResolvedType,
    ) -> Result<(), SemanticError> {
        let _name_str = name.to_string();
        /* TODO:
        self.type_aliases
            .insert(LocalTypeName(name_str.clone()), resolved_type)
            .map_err(|_| SemanticError::DuplicateTypeAlias(name_str))?;

         */
        Ok(())
    }

    pub fn add_enum_type(
        &mut self,
        enum_type_name: &str,
        enum_type_ref: ResolvedEnumTypeRef,
    ) -> Result<ResolvedEnumTypeRef, ResolveError> {
        self.enum_types
            .insert(enum_type_name.to_string(), enum_type_ref.clone())?;

        Ok(enum_type_ref)
    }
    pub fn add_enum_variant(
        &mut self,
        enum_type_name: &str,
        enum_variant_name: &str,
        enum_variant: ResolvedEnumVariantType,
    ) -> Result<ResolvedEnumVariantTypeRef, ResolveError> {
        let enum_variant_ref = Rc::new(enum_variant);
        let full_name = format!("{}::{}", enum_type_name, enum_variant_name);
        self.enum_variant_types
            .insert(full_name, enum_variant_ref.clone())?;
        Ok(enum_variant_ref)
    }

    pub fn add_internal_function(
        &mut self,
        name: &str,
        function: ResolvedInternalFunctionDefinition,
    ) -> Result<ResolvedInternalFunctionDefinitionRef, ResolveError> {
        let function_ref = Rc::new(function);
        self.internal_functions
            .insert(name.to_string(), function_ref.clone())
            .expect("todo: add seqmap error handling");
        Ok(function_ref)
    }

    pub fn get_struct(&self, name: &str) -> Option<&ResolvedStructTypeRef> {
        self.structs.get(&name.to_string())
    }

    pub fn get_enum(&self, name: &str) -> Option<&ResolvedEnumTypeRef> {
        self.enum_types.get(&name.to_string())
    }

    pub fn get_rust_type(&self, name: &str) -> Option<&ResolvedRustTypeRef> {
        self.build_in_rust_types.get(&name.to_string())
    }

    pub fn get_type_alias(&self, name: &str) -> Option<&ResolvedType> {
        self.type_aliases.get(&name.to_string())
    }

    #[must_use]
    pub fn get_enum_variant_type_str(
        &self,
        enum_name: &str,
        enum_variant_name: &str,
    ) -> Option<&ResolvedEnumVariantTypeRef> {
        let complete_name = format!("{}::{}", enum_name, enum_variant_name);
        let result = self.enum_variant_types.get(&complete_name);
        result
    }

    #[must_use]
    pub fn get_internal_function(
        &self,
        name: &str,
    ) -> Option<&ResolvedInternalFunctionDefinitionRef> {
        self.internal_functions.get(&name.to_string())
    }

    #[must_use]
    pub fn get_external_function_declaration(
        &self,
        name: &str,
    ) -> Option<&ResolvedExternalFunctionDefinitionRef> {
        self.external_function_declarations.get(&name.to_string())
    }

    pub fn add_external_function_declaration(
        &mut self,
        name: &str,
        declaration: ResolvedExternalFunctionDefinition,
    ) -> Result<ResolvedExternalFunctionDefinitionRef, ResolveError> {
        let decl_ref = Rc::new(declaration);
        self.external_function_declarations
            .insert(name.to_string(), decl_ref.clone())?;
        Ok(decl_ref)
    }
}
