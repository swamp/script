/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::Error;
use seq_map::SeqMap;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_modules::modules::Modules;
use swamp_script_modules::ns::{GenericAwareType, GenericType, GenericTypeRef, ModuleNamespaceRef};
use swamp_script_semantic::{
    AliasType, AliasTypeRef, Constant, ConstantRef, EnumType, EnumTypeRef, EnumVariantTypeRef,
    ExternalFunctionDefinition, ExternalFunctionDefinitionRef, ExternalTypeRef,
    InternalFunctionDefinition, InternalFunctionDefinitionRef, IntrinsicFunctionDefinitionRef,
    SemanticError, StructType, StructTypeRef, Type, TypeParameterName,
};
use swamp_script_source_map::FileId;
use tracing::info;

#[derive(Debug)]
pub struct TypeParameter {
    pub ty: Type,
    pub debug_name: String,
}

#[derive(Debug)]
pub struct TypeParameterScope {
    pub type_parameters: SeqMap<String, TypeParameter>,
}

#[derive()]
pub struct NameLookup<'a> {
    pub default_path: Vec<String>,
    pub type_parameter_scope_stack: Vec<TypeParameterScope>,
    modules: &'a mut Modules,
    //pub type_parameter_names_stack: Vec<TypeParameterNameScope>,
}

impl<'a> NameLookup<'a> {
    pub(crate) fn get_intrinsic_function(
        &self,
        intrinsic_fn_name: &str,
    ) -> Option<&IntrinsicFunctionDefinitionRef> {
        let ns_borrow = self.own_namespace().borrow();
        ns_borrow.get_intrinsic_function(intrinsic_fn_name)
    }
}

impl<'a> NameLookup<'a> {
    pub(crate) fn get_type_parameter(&self, name: &str) -> Option<&TypeParameter> {
        for scope in self.type_parameter_scope_stack.iter().rev() {
            if let Some(found) = scope.type_parameters.get(&name.to_string()) {
                return Some(found);
            }
        }
        None
    }
}

impl<'a> NameLookup<'a> {
    /*
    pub(crate) fn get_type_parameter_reference(&self, name: &str) -> Option<(usize, usize)> {
        for scope_index in (0..self.type_parameter_names_stack.len()).rev() {
            let scope = &self.type_parameter_names_stack[scope_index];
            for (param_index, (key_name, type_param_name)) in
                scope.type_parameters.iter().enumerate()
            {
                if key_name == name {
                    return Some((scope_index, param_index));
                }
            }
        }

        None
    }

     */
}

impl<'a> NameLookup<'a> {
    /// # Panics
    ///
    pub fn new(default_path: &[String], modules: &'a mut Modules) -> Self {
        assert!(!default_path.is_empty(), "own path must exist");
        Self {
            default_path: default_path.to_vec(),
            modules,
            type_parameter_scope_stack: vec![],
        }
    }

    pub fn get_full_path(&self, path: &[String]) -> Vec<String> {
        if path.is_empty() {
            self.default_path.clone()
        } else {
            path.to_vec()
        }
    }

    pub(crate) fn get_namespace(&self, path_or_empty: &[String]) -> Option<ModuleNamespaceRef> {
        let resolved_path = self.get_full_path(path_or_empty);

        if path_or_empty.len() == 1 {
            let first = &path_or_empty[0];
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

    #[must_use]
    pub fn own_namespace(&self) -> ModuleNamespaceRef {
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

    /*
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

     */

    /*
    pub fn get_type_parameter(&self, name: &str) -> Option<Type> {
        self.type_parameter_scope_stack
            .iter()
            .rev()
            .find_map(|scope| scope.type_parameters.get(&name.to_string()))
            .cloned()
    }

     */

    pub fn push_type_parameter_scope(
        &mut self,
        parameter_name_to_analyzed_type: SeqMap<String, TypeParameter>,
    ) {
        for ty in &parameter_name_to_analyzed_type {
            info!(?ty, "pushing scope!")
        }
        self.type_parameter_scope_stack.push(TypeParameterScope {
            type_parameters: parameter_name_to_analyzed_type,
        });
    }

    /// Pops the most recent type parameter scope off the stack.
    pub fn pop_type_parameter_scope(&mut self) -> Option<TypeParameterScope> {
        self.type_parameter_scope_stack.pop()
    }

    #[must_use]
    pub fn get_struct(&self, path: &[String], name: &str) -> Option<StructTypeRef> {
        info!(?path, ?name, "looking for existing struct");
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
    pub fn get_rust_type(&self, path: &[String], name: &str) -> Option<ExternalTypeRef> {
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
    pub fn add_generated_struct(
        &self,
        path: &[String],
        struct_type: StructType,
    ) -> Result<StructTypeRef, Error> {
        info!(?path, name=?struct_type.assigned_name, "inserting generated struct");
        let namespace = self.get_namespace(path);
        if let Some(found_namespace) = namespace {
            Ok(found_namespace.borrow_mut().add_struct(struct_type)?)
        } else {
            Err(SemanticError::ResolveNotStruct)?
        }
    }

    /// # Errors
    ///
    pub fn add_generic(
        &mut self,
        name: &str,
        parameter_names: SeqMap<String, TypeParameterName>,
        inner_type: GenericAwareType,
        file_id: FileId,
    ) -> Result<(), SemanticError> {
        let generic_type = GenericType {
            type_parameters: parameter_names,
            base_type: inner_type,
            ast_functions: SeqMap::default(),
            file_id,
            defined_in_path: self.default_path.clone(),
        };

        self.own_namespace()
            .borrow_mut()
            .add_generic(name, Rc::new(RefCell::new(generic_type)))
    }

    #[must_use]
    pub fn get_generic(&self, path: &[String], name: &str) -> Option<GenericTypeRef> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(
            || None,
            |found_ns| {
                if let Some(found_generic) = found_ns.borrow().get_generic(name) {
                    Some(found_generic)
                } else {
                    None
                }
            },
        )
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

    pub(crate) fn add_enum_link(&self, _enum_type: &EnumTypeRef) -> Result<(), SemanticError> {
        //self.namespace.borrow_mut().add_enum_type()
        todo!()
    }

    pub(crate) fn add_struct_link(&self, struct_type: StructTypeRef) -> Result<(), SemanticError> {
        self.own_namespace()
            .borrow_mut()
            .add_struct_ref(struct_type)
    }

    pub(crate) fn add_generic_link(
        &mut self,
        name: &str,
        generic_type: GenericTypeRef,
    ) -> Result<(), SemanticError> {
        self.own_namespace()
            .borrow_mut()
            .add_generic_link(name, generic_type)
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
