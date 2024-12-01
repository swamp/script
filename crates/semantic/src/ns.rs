/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::{
    comma_seq_nl, Hash, ResolvedEnumType, ResolvedEnumTypeRef, ResolvedEnumVariantType,
    ResolvedEnumVariantTypeRef, ResolvedExternalFunctionDefinition,
    ResolvedExternalFunctionDefinitionRef, ResolvedInternalFunctionDefinition,
    ResolvedInternalFunctionDefinitionRef, ResolvedStructType, ResolvedStructTypeRef,
    ResolvedTupleType, ResolvedTupleTypeRef, ResolvedType, TypeNumber,
};
use seq_map::{SeqMap, SeqMapError};
use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::rc::Rc;
use swamp_script_ast::LocalTypeIdentifier;
use tracing::info;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct LocalTypeName(pub String);

impl Display for LocalTypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Default, Debug)]
pub struct ResolvedModuleNamespace {
    pub all_owned_types: SeqMap<LocalTypeIdentifier, ResolvedType>,

    structs: SeqMap<LocalTypeName, ResolvedStructTypeRef>, // They are created by the module, so they are owned here
    enum_types: SeqMap<LocalTypeName, ResolvedEnumTypeRef>, // They are created by the module, so they are owned here
    enum_variant_types: SeqMap<LocalTypeName, ResolvedEnumVariantTypeRef>, // They are created by the module, so they are owned here

    tuples: Vec<ResolvedTupleTypeRef>,

    internal_functions: SeqMap<String, ResolvedInternalFunctionDefinitionRef>,
    external_function_declarations: SeqMap<String, ResolvedExternalFunctionDefinitionRef>,
    pub type_aliases: SeqMap<LocalTypeName, ResolvedType>,

    type_number: TypeNumber,
}

impl ResolvedModuleNamespace {
    pub fn allocate_number(&mut self) -> TypeNumber {
        self.type_number += 1;
        self.type_number
    }
}

impl Display for ResolvedModuleNamespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.structs.is_empty() {
            writeln!(f, "structs:")?;
            for (_struct_name, struct_type_ref) in &self.structs {
                let struct_ref = struct_type_ref.borrow();
                writeln!(f, "{}", struct_ref)?;
                writeln!(f, "impl:\n{}", comma_seq_nl(&struct_ref.functions, ".."))?;
            }
        }

        if !self.enum_variant_types.is_empty() {
            writeln!(f, "enum_variants:")?;
            for (_struct_name, enum_variant_ref) in &self.enum_variant_types {
                writeln!(f, "{}", enum_variant_ref)?;
            }
        }

        Ok(())
    }
}

impl Into<LocalTypeName> for &String {
    fn into(self) -> LocalTypeName {
        LocalTypeName(self.clone())
    }
}

impl Into<LocalTypeName> for &str {
    fn into(self) -> LocalTypeName {
        LocalTypeName(self.to_string())
    }
}

#[derive(Debug)]
pub enum SemanticError {
    CouldNotInsertStruct,
    SeqMapError(SeqMapError),
    DuplicateTypeAlias(LocalTypeIdentifier),
}

impl From<SeqMapError> for SemanticError {
    fn from(err: SeqMapError) -> Self {
        Self::SeqMapError(err)
    }
}

impl ResolvedModuleNamespace {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_struct_type(
        &mut self,
        name: &LocalTypeIdentifier,
        struct_type: ResolvedStructType,
    ) -> Result<ResolvedStructTypeRef, SemanticError> {
        let struct_ref = Rc::new(RefCell::new(struct_type));
        self.structs
            .insert((&name.text).into(), struct_ref.clone())?;

        Ok(struct_ref)
    }

    pub fn add_type_alias(
        &mut self,
        name: LocalTypeIdentifier,
        resolved_type: ResolvedType,
    ) -> Result<(), SemanticError> {
        self.type_aliases
            .insert(LocalTypeName(name.text.clone()), resolved_type)
            .map_err(|_| SemanticError::DuplicateTypeAlias(name))?;
        Ok(())
    }

    pub fn get_type_alias(&self, name: &String) -> Option<&ResolvedType> {
        self.type_aliases.get(&(name).into())
    }

    pub fn add_internal_function(
        &mut self,
        name: String,
        function_definition: ResolvedInternalFunctionDefinition,
    ) -> Result<ResolvedInternalFunctionDefinitionRef, SemanticError> {
        let function_ref = Rc::new(function_definition);
        self.internal_functions
            .insert(name.clone(), function_ref.clone())?;
        Ok(function_ref)
    }

    pub fn add_external_function_declaration(
        &mut self,
        name: String,
        function_declaration: ResolvedExternalFunctionDefinition,
    ) -> Result<ResolvedExternalFunctionDefinitionRef, SemanticError> {
        let function_ref = Rc::new(function_declaration);
        self.external_function_declarations
            .insert(name.clone(), function_ref.clone())?;
        Ok(function_ref)
    }

    pub fn add_enum_variant(
        &mut self,
        enum_variant: ResolvedEnumVariantType,
    ) -> Result<ResolvedEnumVariantTypeRef, SemanticError> {
        let enum_variant_ref = Rc::new(enum_variant);
        let complete_name = &enum_variant_ref.complete_name();
        info!("complete_name: {}", complete_name);
        self.enum_variant_types.insert(
            LocalTypeName(complete_name.clone()),
            enum_variant_ref.clone(),
        )?;
        Ok(enum_variant_ref)
    }

    pub fn create_enum_type(
        &mut self,
        enum_type_name: &LocalTypeIdentifier,
        number: TypeNumber,
    ) -> Result<ResolvedEnumTypeRef, SemanticError> {
        let resolved_parent_type = ResolvedEnumType::new(enum_type_name.clone(), number);

        let enum_type_ref = Rc::new(resolved_parent_type);

        self.enum_types.insert(
            LocalTypeName(enum_type_name.text.clone()),
            enum_type_ref.clone(),
        )?;

        Ok(enum_type_ref)
    }

    pub fn get_or_create_tuple(&mut self, types: Vec<ResolvedType>) -> ResolvedTupleTypeRef {
        // TODO: for now, just create new types, in the future we should check if we can reuse a type
        let tuple_type = Rc::new(ResolvedTupleType::new(types));
        self.tuples.push(tuple_type.clone());

        tuple_type
    }

    pub fn get_struct(&self, name: &LocalTypeIdentifier) -> Option<&ResolvedStructTypeRef> {
        // TODO: Add scope support, for now just ignore it
        self.structs.get(&(&name.text).into())
    }

    pub fn get_local_struct(&self, name: &LocalTypeIdentifier) -> Option<&ResolvedStructTypeRef> {
        self.structs.get(&(&name.text).into())
    }

    pub fn get_enum(&self, name: &LocalTypeIdentifier) -> Option<&ResolvedEnumTypeRef> {
        self.enum_types.get(&(&name.text).into())
    }

    pub fn get_enum_variant_type(
        &self,
        enum_name: &LocalTypeIdentifier,
        enum_variant_name: &LocalTypeIdentifier,
    ) -> Option<&ResolvedEnumVariantTypeRef> {
        let complete_name =
            LocalTypeName(format!("{}::{}", enum_name.text, enum_variant_name.text));
        info!("looking up: '{}'", complete_name);
        self.enum_variant_types.get(&complete_name)
    }

    pub fn get_internal_function(
        &self,
        name: &str,
    ) -> Option<&ResolvedInternalFunctionDefinitionRef> {
        self.internal_functions.get(&name.to_string())
    }

    pub fn get_external_function_declaration(
        &self,
        name: &str,
    ) -> Option<&ResolvedExternalFunctionDefinitionRef> {
        self.external_function_declarations.get(&name.to_string())
    }
}
