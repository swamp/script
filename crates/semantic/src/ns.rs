/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::module::Module;

use crate::*;
use seq_map::SeqMap;
use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::rc::Rc;
use swamp_script_ast::{LocalTypeIdentifier, QualifiedTypeIdentifier};

pub struct CanonicalTypeName {
    pub module: Rc<Module>,
    pub name: LocalTypeIdentifier,
}

impl Debug for CanonicalTypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}::{}", self.module, self.name)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct LocalTypeName(pub String);

#[derive(Default, Debug)]
pub struct ResolvedModuleNamespace {
    pub all_owned_types: SeqMap<LocalTypeIdentifier, ResolvedType>,

    structs: SeqMap<LocalTypeName, ResolvedStructTypeRef>, // They are created by the module, so they are owned here
    enum_types: SeqMap<LocalTypeName, ResolvedEnumTypeRef>, // They are created by the module, so they are owned here
    enum_variant_types: SeqMap<LocalTypeName, ResolvedEnumVariantTypeRef>, // They are created by the module, so they are owned here

    tuples: Vec<ResolvedTupleTypeRef>,
    internal_functions: SeqMap<String, ResolvedInternalFunctionDefinitionRef>,

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
                writeln!(f, "impl:\n{}", comma_seq_nl(&struct_ref.impl_members, ".."))?;
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

impl ResolvedModuleNamespace {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn name(&self) -> String {
        "namespace_name".to_string()
    }

    pub fn add_struct_type(
        &mut self,
        name: &LocalTypeIdentifier,
        struct_type: ResolvedStructType,
    ) -> Result<ResolvedStructTypeRef, String> {
        let struct_ref = Rc::new(RefCell::new(struct_type));
        self.structs
            .insert((&name.text).into(), struct_ref.clone())
            .expect("should be able to add struct field");
        Ok(struct_ref)
    }

    pub fn add_internal_function(
        &mut self,
        name: String,
        function_definition: ResolvedInternalFunctionDefinition,
    ) -> Result<ResolvedInternalFunctionDefinitionRef, String> {
        let function_ref = Rc::new(function_definition);
        self.internal_functions
            .insert(name.clone(), function_ref.clone())
            .expect("insert internal function");
        Ok(function_ref)
    }

    pub fn add_enum_variant(
        &mut self,
        enum_variant: ResolvedEnumVariantType,
    ) -> Result<ResolvedEnumVariantTypeRef, String> {
        let enum_variant_ref = Rc::new(enum_variant);
        self.enum_variant_types
            .insert(
                (&enum_variant_ref.complete_name()).into(),
                enum_variant_ref.clone(),
            )
            .expect("should be able to add enum variant");
        Ok(enum_variant_ref)
    }

    pub fn create_enum_type(
        &mut self,
        name: &LocalTypeIdentifier,
        containers: SeqMap<&LocalTypeIdentifier, ResolvedEnumVariantContainerType>,
        type_number: TypeNumber,
    ) -> Result<(), String> {
        let boxed = Rc::new(ResolvedEnumType::new(name.clone(), type_number));

        self.enum_types
            .insert((&name.text).into(), boxed.clone())
            .expect("should be able to add enum type");

        for (ident, variant) in containers.iter() {
            let converted_variant = ResolvedEnumVariantType::new(
                boxed.clone(),
                (*ident).clone(),
                variant.clone(),
                type_number,
            );
            let complete_name = &*(name.to_string() + "::" + &*ident.to_string());
            let _ = self
                .enum_variant_types
                .insert(complete_name.into(), Rc::new(converted_variant));
        }

        Ok(())
    }

    pub fn get_or_create_tuple(&mut self, types: Vec<ResolvedType>) -> ResolvedTupleTypeRef {
        // TODO: for now, just create new types, in the future we should check if we can reuse a type
        let tuple_type = Rc::new(ResolvedTupleType::new(types));
        self.tuples.push(tuple_type.clone());

        tuple_type
    }

    pub fn add_impl(
        &mut self,
        _name: &LocalTypeIdentifier,
        _struct_swamp_type: ResolvedStructTypeRef,
        _methods: ImplType,
    ) -> Result<(), String> {
        // TODO: self.impl_members
        //.insert(ResolvedType::Struct(struct_swamp_type), methods);
        Ok(())
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
        self.enum_variant_types.get(&complete_name)
    }

    pub fn get_function(&self, name: &str) -> Option<&ResolvedInternalFunctionDefinitionRef> {
        self.internal_functions.get(&name.to_string())
    }

    pub fn get_impl(&self, _type_id: &ResolvedType) -> Option<&ImplType> {
        // TODO: self.impl_members.get(type_id)
        todo!()
    }
}
