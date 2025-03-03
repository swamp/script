/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::intr::IntrinsicFunctionDefinitionRef;
use crate::modules::ModuleRef;
use crate::{
    AliasType, AliasTypeRef, AnonymousStructType, Constant, ConstantRef, EnumType, EnumTypeRef,
    EnumVariantType, EnumVariantTypeRef, ExternalFunctionDefinition, ExternalFunctionDefinitionRef,
    ExternalType, ExternalTypeRef, InternalFunctionDefinition, InternalFunctionDefinitionRef,
    NamedStructType, Node, SemanticError, StructTypeField, StructTypeRef, Type,
};
use seq_map::SeqMap;
use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;
use tiny_ver::TinyVersion;
use tracing::info;

#[derive(Debug, Clone)]
pub enum FuncDef {
    Internal(InternalFunctionDefinitionRef),
    Intrinsic(IntrinsicFunctionDefinitionRef),
    External(ExternalFunctionDefinitionRef),
}

#[derive(Clone, Debug)]
pub enum GeneratorKind {
    Slice,
    SlicePair,
    Sparse,
    Map,
    Vec,
}

#[derive(Clone, Debug)]
pub struct TypeGenerator {
    pub kind: GeneratorKind,
    pub arity: usize,
}

#[derive(Clone, Debug)]
pub enum Symbol {
    Type(Type),
    Module(ModuleRef),
    PackageVersion(TinyVersion),
    Constant(ConstantRef),
    FunctionDefinition(FuncDef),
    Alias(AliasTypeRef),
    //TypeGenerator(TypeGenerator),
}

impl Symbol {
    #[must_use]
    pub const fn is_basic_type(&self) -> bool {
        matches!(self, Self::Type(..))
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    symbols: SeqMap<String, Symbol>,
}

pub type SymbolTableRef = Rc<SymbolTable>;

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    #[must_use]
    pub fn new() -> Self {
        Self {
            symbols: SeqMap::default(),
        }
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }

    #[must_use]
    pub const fn symbols(&self) -> &SeqMap<String, Symbol> {
        &self.symbols
    }

    pub fn structs(&self) -> SeqMap<String, StructTypeRef> {
        let mut structs = SeqMap::new();

        for (name, symbol) in &self.symbols {
            if let Symbol::Type(ty) = symbol {
                if let Type::NamedStruct(struct_ref) = ty {
                    structs
                        .insert(name.to_string(), struct_ref.clone())
                        .unwrap();
                }
            }
        }

        structs
    }

    /// # Errors
    ///
    pub fn extend_from(&mut self, symbol_table: &Self) -> Result<(), SemanticError> {
        for (name, symbol) in symbol_table.symbols() {
            self.add_symbol(name, symbol.clone())?;
        }
        Ok(())
    }

    /// # Errors
    ///
    pub fn extend_basic_from(&mut self, symbol_table: &Self) -> Result<(), SemanticError> {
        for (name, symbol) in symbol_table.symbols() {
            if symbol.is_basic_type() {
                self.add_symbol(name, symbol.clone())?;
            }
        }
        Ok(())
    }

    #[must_use]
    pub fn get_package_version(&self, name: &str) -> Option<String> {
        match self.get_symbol(name)? {
            Symbol::PackageVersion(name) => Some(name.to_string()),
            _ => None,
        }
    }

    /// # Errors
    ///
    pub fn add_constant(&mut self, constant: Constant) -> Result<ConstantRef, SemanticError> {
        let constant_ref = Rc::new(constant);

        self.add_constant_link(constant_ref.clone())?;

        Ok(constant_ref)
    }

    /// # Errors
    ///
    pub fn add_constant_link(&mut self, constant_ref: ConstantRef) -> Result<(), SemanticError> {
        let name = constant_ref.assigned_name.clone();

        self.symbols
            .insert(name.to_string(), Symbol::Constant(constant_ref))
            .map_err(|_| SemanticError::DuplicateConstName(name.to_string()))?;

        Ok(())
    }

    /// # Errors
    ///
    pub fn add_alias(&mut self, alias_type: AliasType) -> Result<AliasTypeRef, SemanticError> {
        let alias_ref = Rc::new(alias_type);
        self.add_alias_link(alias_ref.clone())?;
        Ok(alias_ref)
    }

    /// # Errors
    ///
    pub fn add_alias_link(&mut self, alias_type_ref: AliasTypeRef) -> Result<(), SemanticError> {
        let name = alias_type_ref.assigned_name.clone();
        self.symbols
            .insert(name.clone(), Symbol::Alias(alias_type_ref.clone()))
            .map_err(|_| SemanticError::DuplicateStructName(name))?;

        Ok(())
    }

    pub fn add_external_type(
        &mut self,
        external: ExternalType,
    ) -> Result<ExternalTypeRef, SemanticError> {
        let external_ref = Rc::new(external);
        self.add_external_type_link(external_ref.clone())?;
        Ok(external_ref)
    }

    /// # Errors
    ///
    pub fn add_external_type_link(
        &mut self,
        external_type_ref: ExternalTypeRef,
    ) -> Result<(), SemanticError> {
        let name = external_type_ref.type_name.clone();
        self.symbols
            .insert(
                name.clone(),
                Symbol::Type(Type::External(external_type_ref)),
            )
            .map_err(|_| SemanticError::DuplicateStructName(name))?;
        Ok(())
    }

    /// # Errors
    ///
    pub fn add_struct(
        &mut self,
        struct_type: NamedStructType,
    ) -> Result<StructTypeRef, SemanticError> {
        let struct_ref = Rc::new(RefCell::new(struct_type));
        self.add_struct_link(struct_ref.clone())?;
        Ok(struct_ref)
    }

    /// # Errors
    ///
    pub fn add_generated_struct(
        &mut self,
        name: &str,
        fields: &[(&str, Type)],
    ) -> Result<StructTypeRef, SemanticError> {
        let mut defined_fields = SeqMap::new();
        for (name, field_type) in fields {
            defined_fields
                .insert(
                    name.to_string(),
                    StructTypeField {
                        identifier: None,
                        field_type: field_type.clone(),
                    },
                )
                .unwrap();
        }

        let struct_type = NamedStructType {
            name: Node::default(),
            assigned_name: name.to_string(),
            anon_struct_type: AnonymousStructType::new(defined_fields),
            functions: SeqMap::default(),
        };

        let struct_ref = Rc::new(RefCell::new(struct_type));

        self.add_struct_link(struct_ref.clone())?;

        Ok(struct_ref)
    }

    /// # Errors
    ///
    pub fn add_struct_link(&mut self, struct_type_ref: StructTypeRef) -> Result<(), SemanticError> {
        let name = struct_type_ref.borrow().assigned_name.clone();
        self.symbols
            .insert(
                name.clone(),
                Symbol::Type(Type::NamedStruct(struct_type_ref)),
            )
            .map_err(|_| SemanticError::DuplicateStructName(name))?;
        Ok(())
    }

    pub fn add_enum_type(&mut self, enum_type: EnumType) -> Result<EnumTypeRef, SemanticError> {
        let enum_type_ref = Rc::new(RefCell::new(enum_type));
        self.add_enum_type_link(enum_type_ref.clone())?;
        Ok(enum_type_ref)
    }

    pub fn add_enum_type_link(&mut self, enum_type_ref: EnumTypeRef) -> Result<(), SemanticError> {
        let ty = Type::Enum(enum_type_ref.clone());
        self.symbols
            .insert(
                enum_type_ref.borrow().assigned_name.clone(),
                Symbol::Type(ty),
            )
            .map_err(|_| {
                SemanticError::DuplicateEnumType(enum_type_ref.borrow().assigned_name.clone())
            })?;

        Ok(())
    }

    pub fn add_enum_variant(
        &mut self,
        enum_type_name: EnumTypeRef,
        enum_variant: EnumVariantType,
    ) -> Result<EnumVariantTypeRef, SemanticError> {
        let enum_variant_ref = Rc::new(enum_variant);
        enum_type_name
            .borrow_mut()
            .variants
            .insert(
                enum_variant_ref.common().assigned_name.clone(),
                enum_variant_ref.clone(),
            )
            .map_err(|_err| {
                SemanticError::DuplicateEnumVariantType(
                    enum_type_name.borrow().assigned_name.clone(),
                    enum_variant_ref.common().assigned_name.clone(),
                )
            })?;

        Ok(enum_variant_ref)
    }

    pub fn add_internal_function(
        &mut self,
        name: &str,
        function: InternalFunctionDefinition,
    ) -> Result<InternalFunctionDefinitionRef, SemanticError> {
        let function_ref = Rc::new(function);
        self.symbols
            .insert(
                name.to_string(),
                Symbol::FunctionDefinition(FuncDef::Internal(function_ref.clone())),
            )
            .expect("todo: add seqmap error handling");
        Ok(function_ref)
    }

    pub fn add_internal_function_link(
        &mut self,
        name: &str,
        function_ref: InternalFunctionDefinitionRef,
    ) -> Result<(), SemanticError> {
        self.symbols
            .insert(
                name.to_string(),
                Symbol::FunctionDefinition(FuncDef::Internal(function_ref.clone())),
            )
            .expect("todo: add seqmap error handling");
        Ok(())
    }

    pub fn get_symbol(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(&name.to_string())
    }

    pub fn add_symbol(&mut self, name: &str, symbol: Symbol) -> Result<(), SemanticError> {
        info!(name, ?symbol, "add_symbol");
        self.symbols
            .insert(name.to_string(), symbol)
            .map_err(|_| SemanticError::DuplicateSymbolName)
    }

    pub fn get_type(&self, name: &str) -> Option<&Type> {
        if let Some(found_symbol) = self.get_symbol(name) {
            if let Symbol::Type(type_ref) = found_symbol {
                return Some(type_ref);
            }
        }

        None
    }

    pub fn get_struct(&self, name: &str) -> Option<&StructTypeRef> {
        match self.get_type(name)? {
            Type::NamedStruct(struct_ref) => Some(struct_ref),
            _ => None,
        }
    }

    pub fn get_enum(&self, name: &str) -> Option<&EnumTypeRef> {
        match self.get_type(name)? {
            Type::Enum(enum_type) => Some(enum_type),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_enum_variant_type(
        &self,
        enum_type_name: &str,
        variant_name: &str,
    ) -> Option<EnumVariantTypeRef> {
        self.get_enum(enum_type_name).as_ref().map_or_else(
            || None,
            |found_enum| {
                found_enum
                    .borrow()
                    .variants
                    .get(&variant_name.to_string())
                    .cloned()
            },
        )
    }

    pub fn get_constant(&self, name: &str) -> Option<&ConstantRef> {
        match self.get_symbol(name)? {
            Symbol::Constant(constant) => Some(constant),
            _ => None,
        }
    }
    #[must_use]
    pub fn get_module_link(&self, name: &str) -> Option<&ModuleRef> {
        match self.get_symbol(name)? {
            Symbol::Module(module_ref) => Some(module_ref),
            _ => None,
        }
    }

    // Functions

    #[must_use]
    pub fn get_function(&self, name: &str) -> Option<&FuncDef> {
        match self.get_symbol(name)? {
            Symbol::FunctionDefinition(func_def) => Some(func_def),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_internal_function(&self, name: &str) -> Option<&InternalFunctionDefinitionRef> {
        match self.get_function(name)? {
            FuncDef::Internal(internal_fn) => Some(internal_fn),
            FuncDef::External(_) => None,
            FuncDef::Intrinsic(_) => None,
        }
    }

    #[must_use]
    pub fn get_intrinsic_function(&self, name: &str) -> Option<&IntrinsicFunctionDefinitionRef> {
        match self.get_function(name)? {
            FuncDef::Intrinsic(intrinsic_fn) => Some(intrinsic_fn),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_external_function_declaration(
        &self,
        name: &str,
    ) -> Option<&ExternalFunctionDefinitionRef> {
        match self.get_function(name)? {
            FuncDef::External(external_def) => Some(external_def),
            _ => None,
        }
    }

    pub fn get_external_type(&self, name: &str) -> Option<&ExternalTypeRef> {
        match self.get_type(name)? {
            Type::External(ext_type) => Some(ext_type),
            _ => None,
        }
    }

    fn insert_symbol(&mut self, name: &str, symbol: Symbol) -> Result<(), SemanticError> {
        self.symbols
            .insert(name.to_string(), symbol)
            .map_err(|_| SemanticError::DuplicateSymbolName)
    }

    pub fn add_external_function_declaration(
        &mut self,
        decl: ExternalFunctionDefinition,
    ) -> Result<ExternalFunctionDefinitionRef, SemanticError> {
        let decl_ref = Rc::new(decl);

        self.add_external_function_declaration_link(decl_ref.clone())?;

        Ok(decl_ref)
    }

    pub fn add_external_function_declaration_link(
        &mut self,
        decl_ref: ExternalFunctionDefinitionRef,
    ) -> Result<(), SemanticError> {
        self.insert_symbol(
            &decl_ref.assigned_name,
            Symbol::FunctionDefinition(FuncDef::External(decl_ref.clone())),
        )
        .map_err(|_| {
            SemanticError::DuplicateExternalFunction(decl_ref.assigned_name.to_string())
        })?;
        Ok(())
    }

    pub fn add_module_link(&mut self, name: &str, ns: ModuleRef) -> Result<(), SemanticError> {
        self.insert_symbol(name, Symbol::Module(ns))
            .map_err(|_| SemanticError::DuplicateNamespaceLink(name.to_string()))?;
        Ok(())
    }
}
