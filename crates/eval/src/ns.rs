/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::module::Module;
use crate::name::{DefinitionKind, NameRegistry};
use crate::value::{FunctionRef, Value};
use seq_map::SeqMap;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::rc::Rc;
use swamp_script_ast::{ImplMember, LocalTypeIdentifier, Parameter, ScopedTypeIdentifier, Type};

pub type StructTypeRef = Rc<StructType>;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct StructType {
    pub fields: SeqMap<LocalTypeIdentifier, SwampTypeId>,
    pub name: LocalTypeIdentifier,
}

impl StructType {
    pub fn new(
        name: LocalTypeIdentifier,
        fields: SeqMap<LocalTypeIdentifier, SwampTypeId>,
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

pub type TupleTypeRef = Rc<TupleType>;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct TupleType(Vec<SwampTypeId>);

impl TupleType {
    pub fn new(types: Vec<SwampTypeId>) -> Self {
        Self(types)
    }
}

pub type EnumTypeRef = Rc<EnumType>;

#[derive(Debug, Eq, PartialEq, Hash, Default)]
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

#[derive(Eq, PartialEq, Hash)]
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

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
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
    pub(crate) fn new(
        members: SeqMap<String, ImplMember>,
        associated_with_struct_type: StructTypeRef,
    ) -> Self {
        Self {
            members,
            associated_with_struct_type,
        }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub enum SwampTypeId {
    Int,
    Float,
    String,
    Bool,
    Array(Box<SwampTypeId>),
    Tuple(TupleTypeRef),
    Struct(StructTypeRef), // The name of the struct
    Enum(EnumTypeRef), // Combination of the EnumTypeName and the variant name and the type of the data
    EnumVariant(EnumVariantTypeRef),
    Function,
    Void,
    Range,
    Any,
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
    registry: NameRegistry,

    structs: HashMap<LocalTypeIdentifier, StructTypeRef>, // They are created by the module, so they are owned here
    enum_types: HashMap<LocalTypeIdentifier, EnumTypeRef>, // They are created by the module, so they are owned here
    enum_variant_types: HashMap<LocalTypeIdentifier, EnumVariantTypeRef>, // They are created by the module, so they are owned here

    tuples: Vec<TupleTypeRef>,
    functions: HashMap<String, (Vec<Parameter>, Type)>,
    impl_members: HashMap<SwampTypeId, ImplType>,
    pub(crate) values: HashMap<String, Value>,
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
        self.registry.register(&name.0, DefinitionKind::Struct)?;
        let struct_ref = Rc::new(struct_type);
        self.structs.insert(name.clone(), struct_ref.clone());
        Ok(struct_ref)
    }

    pub fn create_enum_type(
        &mut self,
        name: &LocalTypeIdentifier,
        containers: SeqMap<&LocalTypeIdentifier, EnumVariantContainerType>,
    ) -> Result<(), String> {
        self.registry.register(&name.0, DefinitionKind::Enum)?;

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

    pub fn get_or_create_tuple(&mut self, types: Vec<SwampTypeId>) -> TupleTypeRef {
        // TODO: for now, just create new types, in the future we should check if we can reuse a type
        let tuple_type = Rc::new(TupleType::new(types));
        self.tuples.push(tuple_type.clone());

        tuple_type
    }

    pub fn add_function(
        &mut self,
        name: String,
        signature: (Vec<Parameter>, Type),
        func_ref: FunctionRef,
    ) -> Result<(), String> {
        // Register the name only once
        self.registry.register(&name, DefinitionKind::Function)?;

        // Add both signature and value
        self.functions.insert(name.clone(), signature);
        self.values.insert(name, Value::Function(func_ref));
        Ok(())
    }

    pub fn add_impl(
        &mut self,
        name: &LocalTypeIdentifier,
        struct_swamp_type: StructTypeRef,
        methods: ImplType,
    ) -> Result<(), String> {
        for method_name in methods.members.keys() {
            self.registry
                .register(method_name, DefinitionKind::ImplMethod(name.0.clone()))?;
        }

        self.impl_members
            .insert(SwampTypeId::Struct(struct_swamp_type), methods);
        Ok(())
    }

    pub fn add_value(&mut self, name: String, value: Value) -> Result<(), String> {
        self.registry.register(&name, DefinitionKind::Variable)?;
        self.values.insert(name, value);
        Ok(())
    }

    pub fn get_struct(&self, name: &ScopedTypeIdentifier) -> Option<&StructTypeRef> {
        // TODO: Add scope support, for now just ignore it
        self.structs.get(&LocalTypeIdentifier::new(&*name.0))
    }

    pub fn get_local_struct(&self, name: &LocalTypeIdentifier) -> Option<&StructTypeRef> {
        self.structs.get(&*name)
    }

    pub fn get_enum(&self, name: &ScopedTypeIdentifier) -> Option<&EnumTypeRef> {
        self.enum_types.get(&LocalTypeIdentifier::new(&*name.0))
    }

    pub fn get_enum_variant_type(
        &self,
        name: &ScopedTypeIdentifier,
        identifier: LocalTypeIdentifier,
    ) -> Option<&EnumVariantTypeRef> {
        // TODO: add scope/module support, ignore for now
        let full_name = format!("{}::{}", name.0, identifier);

        self.enum_variant_types
            .get(&LocalTypeIdentifier::new(&*full_name))
    }

    pub fn get_function(&self, name: &str) -> Option<&(Vec<Parameter>, Type)> {
        self.functions.get(name)
    }

    pub fn get_impl(&self, type_id: &SwampTypeId) -> Option<&ImplType> {
        self.impl_members.get(type_id)
    }

    pub fn get_value(&self, name: &str) -> Option<&Value> {
        self.values.get(name)
    }
}
