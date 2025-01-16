/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use crate::ResolveError;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_semantic::modules::ResolvedModules;
use swamp_script_semantic::ns::ResolvedModuleNamespaceRef;
use swamp_script_semantic::{
    ResolvedConstant, ResolvedConstantRef, ResolvedEnumType, ResolvedEnumTypeRef,
    ResolvedEnumVariantType, ResolvedEnumVariantTypeRef, ResolvedExternalFunctionDefinitionRef,
    ResolvedInternalFunctionDefinition, ResolvedInternalFunctionDefinitionRef, ResolvedRustTypeRef,
    ResolvedStructType, ResolvedStructTypeRef, SemanticError,
};

#[derive()]
pub struct NameLookup<'a> {
    default_path: Vec<String>,
    modules: &'a mut ResolvedModules,
}

impl<'a> NameLookup<'a> {
    /// # Panics
    ///
    pub fn new(default_path: Vec<String>, modules: &'a mut ResolvedModules) -> Self {
        assert!(!default_path.is_empty(), "own path must exist");
        Self {
            default_path,
            modules,
        }
    }
    fn get_namespace(&self, path: &[String]) -> Option<ResolvedModuleNamespaceRef> {
        let resolved_path = if path.is_empty() {
            self.default_path.clone()
        } else {
            path.to_vec()
        };
        self.modules
            .modules
            .get(&resolved_path)
            .map(|module| module.borrow().namespace.clone())
    }

    fn own_namespace(&self) -> ResolvedModuleNamespaceRef {
        self.get_namespace(&[]).unwrap_or_else(|| {
            panic!(
                "{}",
                format!("could not find own namespace {:?}", self.default_path)
            )
        })
    }

    #[must_use]
    pub fn get_internal_function(
        &self,
        path: &[String],
        name: &str,
    ) -> Option<ResolvedInternalFunctionDefinitionRef> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(
            || None,
            |found_ns| found_ns.borrow().get_internal_function(name).cloned(),
        )
    }

    #[must_use]
    pub fn get_external_function_declaration(
        &self,
        path: &[String],
        name: &str,
    ) -> Option<ResolvedExternalFunctionDefinitionRef> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(
            || None,
            |found_ns| {
                found_ns
                    .borrow()
                    .get_external_function_declaration(name)
                    .cloned()
            },
        )
    }

    #[must_use]
    pub fn get_struct(&self, path: &[String], name: &str) -> Option<ResolvedStructTypeRef> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(
            || None,
            |found_ns| found_ns.borrow().get_struct(name).cloned(),
        )
    }

    #[must_use]
    pub fn get_enum(&self, path: &[String], name: &str) -> Option<ResolvedEnumTypeRef> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(
            || None,
            |found_ns| found_ns.borrow().get_enum(name).cloned(),
        )
    }

    #[must_use]
    pub fn get_constant(&self, path: &[String], name: &str) -> Option<ResolvedConstantRef> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(
            || None,
            |found_ns| found_ns.borrow().get_constant(name).cloned(),
        )
    }

    #[must_use]
    pub fn get_enum_variant_type(
        &self,
        path: &[String],
        enum_type_name: &str,
        variant_name: &str,
    ) -> Option<ResolvedEnumVariantTypeRef> {
        let namespace = self.get_namespace(path)?;
        let borrowed_namespace = namespace.borrow();
        if let Some(found_enum) = &borrowed_namespace.get_enum(enum_type_name) {
            found_enum
                .borrow()
                .variants
                .get(&variant_name.to_string())
                .cloned()
        } else {
            None
        }
    }

    #[must_use]
    pub fn get_rust_type(&self, path: &[String], name: &str) -> Option<ResolvedRustTypeRef> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(
            || None,
            |found_ns| found_ns.borrow().get_rust_type(name).cloned(),
        )
    }

    #[must_use]
    pub fn get_path(&self) -> Vec<String> {
        self.default_path.clone()
    }

    /// # Errors
    ///
    pub fn add_constant(
        &mut self,
        constant: ResolvedConstant,
    ) -> Result<ResolvedConstantRef, ResolveError> {
        let constant_ref = self.modules.add_constant(constant);
        Ok(self
            .own_namespace()
            .borrow_mut()
            .add_constant_ref(constant_ref)?)
    }

    /// # Errors
    ///
    pub fn add_struct(
        &self,
        struct_type: ResolvedStructType,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        Ok(self.own_namespace().borrow_mut().add_struct(struct_type)?)
    }

    /// # Errors
    ///
    pub fn add_enum_type(
        &mut self,
        mut enum_type: ResolvedEnumType,
    ) -> Result<ResolvedEnumTypeRef, ResolveError> {
        enum_type
            .module_path
            .clone_from(&self.own_namespace().borrow().path);

        Ok(self.own_namespace().borrow_mut().add_enum_type(enum_type)?)
    }

    /// # Errors
    ///
    pub fn add_internal_function_ref(
        &mut self,
        function_name: &str,
        function: ResolvedInternalFunctionDefinition,
    ) -> Result<ResolvedInternalFunctionDefinitionRef, ResolveError> {
        Ok(self
            .own_namespace()
            .borrow_mut()
            .add_internal_function(function_name, function)?)
    }

    pub(crate) fn add_enum_link(
        &self,
        _enum_type: ResolvedEnumTypeRef,
    ) -> Result<(), SemanticError> {
        //self.namespace.borrow_mut().add_enum_type()
        todo!()
    }

    pub(crate) fn add_struct_link(
        &self,
        struct_type: ResolvedStructTypeRef,
    ) -> Result<(), SemanticError> {
        self.own_namespace()
            .borrow_mut()
            .add_struct_ref(struct_type)
    }

    pub(crate) fn add_external_function_declaration_link(
        &self,
        name: &str,
        external_fn_def: ResolvedExternalFunctionDefinitionRef,
    ) -> Result<(), SemanticError> {
        self.own_namespace()
            .borrow_mut()
            .add_external_function_declaration_link(name, external_fn_def)
    }

    pub(crate) fn add_internal_function_link(
        &self,
        name: &str,
        internal_fn: ResolvedInternalFunctionDefinitionRef,
    ) -> Result<(), SemanticError> {
        self.own_namespace()
            .borrow_mut()
            .add_internal_function_link(name, internal_fn)
    }
}
