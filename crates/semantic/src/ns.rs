/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::module::Module;
use crate::resolved::{
    ResolvedFunctionRef, ResolvedInternalFunctionCall, ResolvedInternalFunctionDefinition,
    ResolvedInternalFunctionDefinitionRef, ResolvedType,
};
use crate::ResolvedImplMemberRef;
use seq_map::SeqMap;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::rc::Rc;
use swamp_script_ast::{
    AnonymousStruct, ImplMember, LocalIdentifier, LocalTypeIdentifier, ModulePath, Parameter,
    QualifiedTypeIdentifier, StructType, Type,
};

pub type ResolvedStructTypeRef = Rc<ResolvedStructType>;

pub type TypeNumber = u32;

#[derive(Debug)]
pub struct ResolvedStructType {
    // TODO:  pub defined_in_module: ResolvedModuleRef,
    pub number: TypeNumber,
    pub module_path: ModulePath,
    pub fields: SeqMap<LocalIdentifier, ResolvedType>,
    pub name: LocalTypeIdentifier,
    pub ast_struct: StructType,
    pub impl_members: SeqMap<LocalIdentifier, ResolvedImplMemberRef>,
}

impl Display for ResolvedStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{", self.name)?;
        for (name, value) in self.fields.iter() {
            write!(f, "{}: {}", name, value)?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedAnonymousStructType {
    // TODO:  pub defined_in_module: ResolvedModuleRef,
    pub module_path: ModulePath,
    pub fields: SeqMap<LocalIdentifier, ResolvedType>,
    pub ast_anon_struct: AnonymousStruct,
}

impl ResolvedAnonymousStructType {
    pub fn new(
        // TODO: defined_in_module: ResolvedModuleRef,
        module_path: ModulePath,
        fields: SeqMap<LocalIdentifier, ResolvedType>,
        ast_anon_struct: AnonymousStruct,
    ) -> Self {
        Self {
            //defined_in_module,
            module_path,
            ast_anon_struct,
            fields,
        }
    }
}

impl Display for ResolvedAnonymousStructType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (name, value) in self.fields.iter() {
            write!(f, "{:?}: {}", name, value)?;
        }
        write!(f, "}}")
    }
}

impl ResolvedStructType {
    pub fn new(
        // TODO: defined_in_module: ResolvedModuleRef,
        module_path: ModulePath,
        name: LocalTypeIdentifier,
        fields: SeqMap<LocalIdentifier, ResolvedType>,
        ast_struct: StructType,
        number: TypeNumber,
    ) -> Self {
        Self {
            number,
            //defined_in_module,
            module_path,
            ast_struct,
            fields,
            name,
            impl_members: SeqMap::default(),
        }
    }

    pub fn field_index(&self, field_name: &LocalIdentifier) -> Option<usize> {
        self.fields.get_index(field_name)
    }

    pub fn name(&self) -> &LocalTypeIdentifier {
        &self.name
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ResolvedIntType;
pub type ResolvedIntTypeRef = Rc<ResolvedIntType>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ResolvedFloatType;

pub type ResolvedFloatTypeRef = Rc<ResolvedFloatType>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ResolvedBoolType;
pub type ResolveBoolTypeRef = Rc<ResolvedBoolType>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct StringType;
pub type StringTypeRef = Rc<StringType>;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct UnitType;
pub type UnitTypeRef = Rc<UnitType>;

pub type ResolvedArrayTypeRef = Rc<ResolvedArrayType>;

#[derive(Debug)]
pub struct ResolvedArrayType {
    pub item_type: ResolvedType,
    //pub ast_type: Type,
}

impl PartialEq for ResolvedArrayType {
    fn eq(&self, other: &Self) -> bool {
        self.item_type == other.item_type
    }
}

impl Display for ResolvedArrayType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.item_type)
    }
}

pub type ResolvedTupleTypeRef = Rc<ResolvedTupleType>;

#[derive(Debug)]
pub struct ResolvedTupleType(pub Vec<ResolvedType>);

impl ResolvedTupleType {
    pub fn new(types: Vec<ResolvedType>) -> Self {
        Self(types)
    }
}

pub type ResolvedEnumTypeRef = Rc<ResolvedEnumType>;

#[derive(Debug)]
pub struct ResolvedEnumType {
    pub name: LocalTypeIdentifier,
    pub number: TypeNumber,
}

impl ResolvedEnumType {
    pub fn new(name: LocalTypeIdentifier, number: TypeNumber) -> Self {
        Self { name, number }
    }

    pub fn name(&self) -> &LocalTypeIdentifier {
        &self.name
    }
}

pub type ResolvedEnumVariantTypeRef = Rc<ResolvedEnumVariantType>;

pub struct ResolvedEnumVariantType {
    pub owner: ResolvedEnumTypeRef,
    pub data: ResolvedEnumVariantContainerType,
    pub name: LocalTypeIdentifier,
    pub number: TypeNumber,
}

impl ResolvedEnumVariantType {
    pub fn new(
        owner: ResolvedEnumTypeRef,
        name: LocalTypeIdentifier,
        data: ResolvedEnumVariantContainerType,
        number: TypeNumber,
    ) -> Self {
        Self {
            owner,
            data,
            name,
            number,
        }
    }

    pub fn container(&self) -> &ResolvedEnumVariantContainerType {
        &self.data
    }

    pub fn name(&self) -> &LocalTypeIdentifier {
        &self.name
    }

    pub fn complete_name(&self) -> String {
        self.owner.name.to_string() + "::" + &*self.name.to_string()
    }
}

impl Display for ResolvedEnumVariantType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.owner.name(), self.name)?;

        match &self.data {
            ResolvedEnumVariantContainerType::Struct(_) => {
                write!(f, "{}::{}", self.name, self.complete_name())
            }
            ResolvedEnumVariantContainerType::Tuple(tuple_ref) => write!(f, "{:?}", tuple_ref),
            ResolvedEnumVariantContainerType::Nothing => write!(f, ""),
        }
    }
}

impl Debug for ResolvedEnumVariantType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.owner.name(), self.name)
    }
}

#[derive(Debug, Clone)]
pub enum ResolvedEnumVariantContainerType {
    Struct(ResolvedAnonymousStructType),
    Tuple(ResolvedTupleTypeRef),
    Nothing,
}

#[derive(Debug)]
pub struct ImplType {
    pub members: SeqMap<String, ImplMember>,
    pub associated_with_struct_type: ResolvedStructTypeRef,
}

impl ImplType {
    pub fn new(
        members: SeqMap<String, ImplMember>,
        associated_with_struct_type: ResolvedStructTypeRef,
    ) -> Self {
        Self {
            members,
            associated_with_struct_type,
        }
    }
}

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
    pub impl_members: HashMap<ResolvedType, ImplType>,

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
                writeln!(f, "{}", struct_type_ref)?;
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
        let struct_ref = Rc::new(struct_type);
        self.structs.insert((&name.text).into(), struct_ref.clone());
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

    pub(crate) fn add_enum_variant(
        &mut self,
        enum_variant: ResolvedEnumVariantType,
    ) -> Result<ResolvedEnumVariantTypeRef, String> {
        let enum_variant_ref = Rc::new(enum_variant);
        self.enum_variant_types.insert(
            (&enum_variant_ref.complete_name()).into(),
            enum_variant_ref.clone(),
        );
        Ok(enum_variant_ref)
    }

    pub fn create_enum_type(
        &mut self,
        name: &LocalTypeIdentifier,
        containers: SeqMap<&LocalTypeIdentifier, ResolvedEnumVariantContainerType>,
        type_number: TypeNumber,
    ) -> Result<(), String> {
        let boxed = Rc::new(ResolvedEnumType::new(name.clone(), type_number));

        self.enum_types.insert((&name.text).into(), boxed.clone());

        for (ident, variant) in &containers {
            let converted_variant = ResolvedEnumVariantType::new(
                boxed.clone(),
                (*ident).clone(),
                variant.clone(),
                type_number,
            );
            let complete_name = &*(name.to_string() + "::" + &*ident.to_string());
            self.enum_variant_types
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
        name: &QualifiedTypeIdentifier,
        identifier: LocalTypeIdentifier,
    ) -> Option<&ResolvedEnumVariantTypeRef> {
        // TODO: add scope/module support, ignore for now
        let _full_name = format!("{:?}::{}", name.name, identifier);

        self.enum_variant_types.get(&(&name.name.text).into())
    }

    pub fn get_function(&self, name: &str) -> Option<&ResolvedInternalFunctionDefinitionRef> {
        self.internal_functions.get(&name.to_string())
    }

    pub fn get_impl(&self, _type_id: &ResolvedType) -> Option<&ImplType> {
        // TODO: self.impl_members.get(type_id)
        todo!()
    }
}
