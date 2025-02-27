/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::modules::ResolvedModuleRef;
use crate::{
    ResolvedConstant, ResolvedConstantRef, ResolvedEnumType, ResolvedEnumTypeRef,
    ResolvedEnumVariantType, ResolvedEnumVariantTypeRef, ResolvedExternalFunctionDefinition,
    ResolvedExternalFunctionDefinitionRef, ResolvedInternalFunctionDefinition,
    ResolvedInternalFunctionDefinitionRef, ResolvedStructType, ResolvedStructTypeRef, ResolvedType,
    SemanticError,
};
use seq_map::SeqMap;
use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;
use tiny_ver::TinyVersion;

#[derive(Debug, Clone)]
pub enum FuncDef {
    Internal(ResolvedInternalFunctionDefinitionRef),
    //Intrinsic(ResolvedIntrinsicFunctionDefinitionRef),
    External(ResolvedExternalFunctionDefinitionRef),
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
    Type(ResolvedType),
    Module(ResolvedModuleRef),
    PackageVersion(TinyVersion),
    Constant(ResolvedConstantRef),
    FunctionDefinition(FuncDef),
    //Alias(ResolvedAliasTypeRef),
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
    pub fn add_constant(
        &mut self,
        constant: ResolvedConstant,
    ) -> Result<ResolvedConstantRef, SemanticError> {
        let constant_ref = Rc::new(constant);

        self.add_constant_link(constant_ref.clone())?;

        Ok(constant_ref)
    }

    /// # Errors
    ///
    pub fn add_constant_link(
        &mut self,
        constant_ref: ResolvedConstantRef,
    ) -> Result<(), SemanticError> {
        let name = constant_ref.assigned_name.clone();

        self.symbols
            .insert(name.to_string(), Symbol::Constant(constant_ref))
            .map_err(|_| SemanticError::DuplicateConstName(name.to_string()))?;

        Ok(())
    }

    /// # Errors
    ///
    pub fn add_struct(
        &mut self,
        struct_type: ResolvedStructType,
    ) -> Result<ResolvedStructTypeRef, SemanticError> {
        let struct_ref = Rc::new(RefCell::new(struct_type));
        self.add_struct_link(struct_ref.clone())?;
        Ok(struct_ref)
    }

    /// # Errors
    ///
    pub fn add_struct_link(
        &mut self,
        struct_type_ref: ResolvedStructTypeRef,
    ) -> Result<(), SemanticError> {
        let name = struct_type_ref.borrow().assigned_name.clone();
        self.symbols
            .insert(
                name.clone(),
                Symbol::Type(ResolvedType::Struct(struct_type_ref)),
            )
            .map_err(|_| SemanticError::DuplicateStructName(name))?;
        Ok(())
    }

    pub fn add_enum_type(
        &mut self,
        enum_type: ResolvedEnumType,
    ) -> Result<ResolvedEnumTypeRef, SemanticError> {
        let enum_type_ref = Rc::new(RefCell::new(enum_type));
        self.add_enum_type_link(enum_type_ref.clone())?;
        Ok(enum_type_ref)
    }

    pub fn add_enum_type_link(
        &mut self,
        enum_type_ref: ResolvedEnumTypeRef,
    ) -> Result<(), SemanticError> {
        let ty = ResolvedType::Enum(enum_type_ref.clone());
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
        enum_type_name: ResolvedEnumTypeRef,
        enum_variant: ResolvedEnumVariantType,
    ) -> Result<ResolvedEnumVariantTypeRef, SemanticError> {
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
        function: ResolvedInternalFunctionDefinition,
    ) -> Result<ResolvedInternalFunctionDefinitionRef, SemanticError> {
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
        function_ref: ResolvedInternalFunctionDefinitionRef,
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
        self.symbols
            .insert(name.to_string(), symbol)
            .map_err(|_| SemanticError::DuplicateSymbolName)
    }

    pub fn get_type(&self, name: &str) -> Option<&ResolvedType> {
        if let Some(found_symbol) = self.get_symbol(name) {
            if let Symbol::Type(type_ref) = found_symbol {
                return Some(type_ref);
            }
        }

        None
    }

    pub fn get_struct(&self, name: &str) -> Option<&ResolvedStructTypeRef> {
        match self.get_type(name)? {
            ResolvedType::Struct(struct_ref) => Some(struct_ref),
            _ => None,
        }
    }

    pub fn get_enum(&self, name: &str) -> Option<&ResolvedEnumTypeRef> {
        match self.get_type(name)? {
            ResolvedType::Enum(enum_type) => Some(enum_type),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_enum_variant_type(
        &self,
        enum_type_name: &str,
        variant_name: &str,
    ) -> Option<ResolvedEnumVariantTypeRef> {
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

    pub fn get_constant(&self, name: &str) -> Option<&ResolvedConstantRef> {
        match self.get_symbol(name)? {
            Symbol::Constant(constant) => Some(constant),
            _ => None,
        }
    }
    #[must_use]
    pub fn get_module_link(&self, name: &str) -> Option<&ResolvedModuleRef> {
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
    pub fn get_internal_function(
        &self,
        name: &str,
    ) -> Option<&ResolvedInternalFunctionDefinitionRef> {
        match self.get_function(name)? {
            FuncDef::Internal(internal_fn) => Some(internal_fn),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_external_function_declaration(
        &self,
        name: &str,
    ) -> Option<&ResolvedExternalFunctionDefinitionRef> {
        match self.get_function(name)? {
            FuncDef::External(external_def) => Some(external_def),
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
        decl: ResolvedExternalFunctionDefinition,
    ) -> Result<ResolvedExternalFunctionDefinitionRef, SemanticError> {
        let decl_ref = Rc::new(decl);

        self.add_external_function_declaration_link(decl_ref.clone())?;

        Ok(decl_ref)
    }

    pub fn add_external_function_declaration_link(
        &mut self,
        decl_ref: ResolvedExternalFunctionDefinitionRef,
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

    pub fn add_module_link(
        &mut self,
        name: &str,
        ns: ResolvedModuleRef,
    ) -> Result<(), SemanticError> {
        self.insert_symbol(name, Symbol::Module(ns))
            .map_err(|_| SemanticError::DuplicateNamespaceLink(name.to_string()))?;
        Ok(())
    }
}
