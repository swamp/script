/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Error;
use std::rc::Rc;
use swamp_script_semantic::modules::Modules;
use swamp_script_semantic::ns::{ModuleNamespaceRef, TypeGenerator};
use swamp_script_semantic::{
    AliasType, AliasTypeRef, Constant, ConstantRef, EnumType, EnumTypeRef, EnumVariantTypeRef,
    ExternalFunctionDefinition, ExternalFunctionDefinitionRef, InternalFunctionDefinition,
    InternalFunctionDefinitionRef, RustTypeRef, SemanticError, StructType, StructTypeRef, Type,
};

#[derive()]
pub struct NameLookup<'a> {
    default_path: Vec<String>,
    modules: &'a mut Modules,
}

impl<'a> NameLookup<'a> {}

impl<'a> NameLookup<'a> {}

impl<'a> NameLookup<'a> {
    /// # Panics
    ///
    pub fn new(default_path: Vec<String>, modules: &'a mut Modules) -> Self {
        assert!(!default_path.is_empty(), "own path must exist");
        Self {
            default_path,
            modules,
        }
    }
    pub(crate) fn get_namespace(&self, path: &[String]) -> Option<ModuleNamespaceRef> {
        let resolved_path = if path.is_empty() {
            self.default_path.clone()
        } else {
            path.to_vec()
        };

        if path.len() == 1 {
            let first = &path[0];
            if let Some(found_alias) = self.get_namespace_link(first) {
                return Some(found_alias);
            }
        }

        self.modules
            .modules
            .get(&resolved_path)
            .map(|module| module.borrow().namespace.clone())
    }

    pub(crate) fn get_namespace_link(&self, name: &str) -> Option<ModuleNamespaceRef> {
        self.own_namespace().borrow().get_namespace_link(name)
    }

    pub fn own_namespace(&self) -> ModuleNamespaceRef {
        self.get_namespace(&[]).unwrap_or_else(|| {
            panic!(
                "{}",
                format!("could not find own namespace {:?}", self.default_path)
            )
        })
    }

    pub(crate) fn modules(&self) -> &Modules {
        self.modules
    }

    #[must_use]
    pub fn get_internal_function(
        &self,
        path: &[String],
        name: &str,
    ) -> Option<InternalFunctionDefinitionRef> {
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
    ) -> Option<ExternalFunctionDefinitionRef> {
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

    pub(crate) fn get_type_generator(
        &self,
        path: &[String],
        name: &str,
    ) -> Option<Rc<dyn TypeGenerator>> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(
            || None,
            |found_ns| found_ns.borrow().get_type_generator(name),
        )
    }

    #[must_use]
    pub fn get_struct(&self, path: &[String], name: &str) -> Option<StructTypeRef> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(|| None, |found_ns| found_ns.borrow().get_struct(name))
    }

    #[must_use]
    pub fn get_alias(&self, path: &[String], name: &str) -> Option<AliasTypeRef> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(|| None, |found_ns| found_ns.borrow().get_alias(name))
    }

    #[must_use]
    pub fn get_alias_referred_type(&self, path: &[String], name: &str) -> Option<Type> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(
            || None,
            |found_ns| found_ns.borrow().get_alias_referred_type(name),
        )
    }

    #[must_use]
    pub fn get_enum(&self, path: &[String], name: &str) -> Option<EnumTypeRef> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(
            || None,
            |found_ns| found_ns.borrow().get_enum(name).cloned(),
        )
    }

    #[must_use]
    pub fn get_constant(&self, path: &[String], name: &str) -> Option<ConstantRef> {
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
    ) -> Option<EnumVariantTypeRef> {
        let namespace = self.get_namespace(path)?;
        let borrowed_namespace = namespace.borrow();
        borrowed_namespace
            .get_enum(enum_type_name)
            .as_ref()
            .map_or_else(
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

    #[must_use]
    pub fn get_rust_type(&self, path: &[String], name: &str) -> Option<RustTypeRef> {
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
    pub fn add_constant(&mut self, constant: Constant) -> Result<ConstantRef, Error> {
        let constant_ref = self.modules.add_constant(constant);
        Ok(self
            .own_namespace()
            .borrow_mut()
            .add_constant_ref(constant_ref)?)
    }

    pub fn add_alias(&self, alias_type: AliasType) -> Result<AliasTypeRef, Error> {
        Ok(self.own_namespace().borrow_mut().add_alias(alias_type)?)
    }

    /// # Errors
    ///
    pub fn add_struct(&self, struct_type: StructType) -> Result<StructTypeRef, Error> {
        Ok(self.own_namespace().borrow_mut().add_struct(struct_type)?)
    }

    /// # Errors
    ///
    pub fn add_enum_type(&mut self, mut enum_type: EnumType) -> Result<EnumTypeRef, Error> {
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
        function: InternalFunctionDefinition,
    ) -> Result<InternalFunctionDefinitionRef, Error> {
        Ok(self
            .own_namespace()
            .borrow_mut()
            .add_internal_function(function_name, function)?)
    }

    /// # Errors
    ///
    pub fn add_external_function_declaration_ref(
        &mut self,
        function: ExternalFunctionDefinition,
    ) -> Result<ExternalFunctionDefinitionRef, Error> {
        Ok(self
            .own_namespace()
            .borrow_mut()
            .add_external_function_declaration(&function.assigned_name.clone(), function)?)
    }

    pub(crate) fn add_namespace_link(
        &self,
        name: &str,
        source_module_path: ModuleNamespaceRef,
    ) -> Result<(), SemanticError> {
        self.own_namespace()
            .borrow_mut()
            .add_namespace_link(name, source_module_path)
    }

    pub(crate) fn add_enum_link(&self, _enum_type: EnumTypeRef) -> Result<(), SemanticError> {
        //self.namespace.borrow_mut().add_enum_type()
        todo!()
    }

    pub(crate) fn add_struct_link(&self, struct_type: StructTypeRef) -> Result<(), SemanticError> {
        self.own_namespace()
            .borrow_mut()
            .add_struct_ref(struct_type)
    }

    pub(crate) fn add_type_generator_link(
        &self,
        name: &str,
        generator_ref: Rc<dyn TypeGenerator>,
    ) -> Result<(), SemanticError> {
        self.own_namespace()
            .borrow_mut()
            .add_type_generator_ref(name, generator_ref)
    }

    pub(crate) fn add_external_function_declaration_link(
        &self,
        name: &str,
        external_fn_def: ExternalFunctionDefinitionRef,
    ) -> Result<(), SemanticError> {
        self.own_namespace()
            .borrow_mut()
            .add_external_function_declaration_link(name, external_fn_def)
    }

    pub(crate) fn add_internal_function_link(
        &self,
        name: &str,
        internal_fn: InternalFunctionDefinitionRef,
    ) -> Result<(), SemanticError> {
        self.own_namespace()
            .borrow_mut()
            .add_internal_function_link(name, internal_fn)
    }
}
