/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::module::Module;
use crate::{ResolvedFunctionRef, ResolvedType};
use seq_map::SeqMap;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::rc::Rc;
use swamp_script_ast::{ImplMember, LocalTypeIdentifier, Parameter, QualifiedTypeIdentifier, Type};

pub type StructTypeRef = Rc<StructType>;

#[derive(Debug)]
pub struct StructType {
    pub fields: SeqMap<LocalTypeIdentifier, ResolvedType>,
    pub name: LocalTypeIdentifier,
}

impl StructType {
    pub fn new(
        name: LocalTypeIdentifier,
        fields: SeqMap<LocalTypeIdentifier, ResolvedType>,
    ) -> Self {
        Self { fields, name }
    }

    pub fn field_index(&self, field_name: &LocalTypeIdentifier) -> Option<usize> {
        self.fields.get_index(field_name)
    }

    pub fn name(&self) -> &LocalTypeIdentifier {
        &self.name
    }
}

#[derive(Debug)]
pub struct IntType;
pub type IntTypeRef = Rc<IntType>;

#[derive(Debug)]
pub struct FloatType;
pub type FloatTypeRef = Rc<FloatType>;

#[derive(Debug)]
pub struct BoolType;
pub type BoolTypeRef = Rc<BoolType>;

#[derive(Debug)]
pub struct StringType;
pub type StringTypeRef = Rc<StringType>;

#[derive(Debug)]
pub struct UnitType;
pub type UnitTypeRef = Rc<UnitType>;

pub type ResolvedArrayTypeRef = Rc<ResolvedArrayType>;

#[derive(Debug)]
pub struct ResolvedArrayType {
    pub(crate) item_type: ResolvedType,
    pub(crate) ast_type: Type,
}

pub type TupleTypeRef = Rc<TupleType>;

#[derive(Debug)]
pub struct TupleType(Vec<ResolvedType>);

impl TupleType {
    pub fn new(types: Vec<ResolvedType>) -> Self {
        Self(types)
    }
}

pub type EnumTypeRef = Rc<EnumType>;

#[derive(Debug)]
pub struct EnumType {
    pub name: LocalTypeIdentifier,
}

impl EnumType {
    pub fn new(name: LocalTypeIdentifier) -> Self {
        Self { name }
    }

    pub fn name(&self) -> &LocalTypeIdentifier {
        &self.name
    }
}

pub type EnumVariantTypeRef = Rc<EnumVariantType>;

pub struct EnumVariantType {
    pub owner: EnumTypeRef,
    pub data: EnumVariantContainerType,
    pub name: LocalTypeIdentifier,
}

impl EnumVariantType {
    pub fn new(
        owner: EnumTypeRef,
        name: LocalTypeIdentifier,
        data: EnumVariantContainerType,
    ) -> Self {
        Self { owner, data, name }
    }

    pub fn container(&self) -> &EnumVariantContainerType {
        &self.data
    }

    pub fn name(&self) -> &LocalTypeIdentifier {
        &self.name
    }
}

impl Display for EnumVariantType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.owner.name(), self.name)
    }
}

impl Debug for EnumVariantType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.owner.name(), self.name)
    }
}

#[derive(Debug)]
pub enum EnumVariantContainerType {
    Struct(StructTypeRef),
    Tuple(TupleTypeRef),
    Nothing,
}

#[derive(Debug)]
pub struct ImplType {
    pub members: SeqMap<String, ImplMember>,
    pub associated_with_struct_type: StructTypeRef,
}

impl ImplType {
    pub fn new(
        members: SeqMap<String, ImplMember>,
        associated_with_struct_type: StructTypeRef,
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

#[derive(Default, Debug)]
pub struct ModuleNamespace {
    all_owned_types: SeqMap<LocalTypeIdentifier, ResolvedType>,

    structs: HashMap<LocalTypeIdentifier, StructTypeRef>, // They are created by the module, so they are owned here
    enum_types: HashMap<LocalTypeIdentifier, EnumTypeRef>, // They are created by the module, so they are owned here
    enum_variant_types: HashMap<LocalTypeIdentifier, EnumVariantTypeRef>, // They are created by the module, so they are owned here

    tuples: Vec<TupleTypeRef>,
    functions: HashMap<String, (Vec<Parameter>, ResolvedType)>,
    impl_members: HashMap<ResolvedType, ImplType>,
}

impl ModuleNamespace {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn name(&self) -> String {
        "namespace_name".to_string()
    }

    pub fn add_struct_type(
        &mut self,
        name: &LocalTypeIdentifier,
        struct_type: StructType,
    ) -> Result<StructTypeRef, String> {
        let struct_ref = Rc::new(struct_type);
        self.structs.insert(name.clone(), struct_ref.clone());
        Ok(struct_ref)
    }

    pub fn create_enum_type(
        &mut self,
        name: &LocalTypeIdentifier,
        containers: SeqMap<&LocalTypeIdentifier, EnumVariantContainerType>,
    ) -> Result<(), String> {
        let boxed = Rc::new(EnumType::new(name.clone()));

        self.enum_types.insert(name.clone(), boxed.clone());

        for (ident, variant) in &containers {
            let converted_variant =
                EnumVariantType::new(boxed.clone(), (*ident).clone(), variant.clone());
            let complete_name = &*(name.to_string() + "::" + &*ident.to_string());
            self.enum_variant_types.insert(
                LocalTypeIdentifier::new(complete_name),
                Rc::new(converted_variant),
            );
        }

        Ok(())
    }

    pub fn get_or_create_tuple(&mut self, types: Vec<ResolvedType>) -> TupleTypeRef {
        // TODO: for now, just create new types, in the future we should check if we can reuse a type
        let tuple_type = Rc::new(TupleType::new(types));
        self.tuples.push(tuple_type.clone());

        tuple_type
    }

    pub fn add_function(
        &mut self,
        name: String,
        signature: (Vec<Parameter>, ResolvedType),
        _func_ref: ResolvedFunctionRef,
    ) -> Result<(), String> {
        // Register the name only once

        // Add both signature and value
        self.functions.insert(name.clone(), signature);
        Ok(())
    }

    pub fn add_impl(
        &mut self,
        _name: &LocalTypeIdentifier,
        struct_swamp_type: StructTypeRef,
        methods: ImplType,
    ) -> Result<(), String> {
        self.impl_members
            .insert(ResolvedType::Struct(struct_swamp_type), methods);
        Ok(())
    }

    pub fn get_struct(&self, name: &QualifiedTypeIdentifier) -> Option<&StructTypeRef> {
        // TODO: Add scope support, for now just ignore it
        self.structs.get(&LocalTypeIdentifier::new(name.name))
    }

    pub fn get_local_struct(&self, name: &LocalTypeIdentifier) -> Option<&StructTypeRef> {
        self.structs.get(&*name)
    }

    pub fn get_enum(&self, name: &QualifiedTypeIdentifier) -> Option<&EnumTypeRef> {
        self.enum_types.get(&LocalTypeIdentifier::new(name.name))
    }

    pub fn get_enum_variant_type(
        &self,
        name: &QualifiedTypeIdentifier,
        identifier: LocalTypeIdentifier,
    ) -> Option<&EnumVariantTypeRef> {
        // TODO: add scope/module support, ignore for now
        let full_name = format!("{:?}::{}", name.name, identifier);

        self.enum_variant_types
            .get(&LocalTypeIdentifier::new(&*full_name))
    }

    pub fn get_function(&self, name: &str) -> Option<&(Vec<Parameter>, ResolvedType)> {
        self.functions.get(name)
    }

    pub fn get_impl(&self, type_id: &ResolvedType) -> Option<&ImplType> {
        self.impl_members.get(type_id)
    }
}
