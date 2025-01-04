/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::{
    ConstantId, ResolvedAnonymousStructFieldType, ResolvedAnonymousStructType, ResolvedConstant,
    ResolvedConstantRef, ResolvedEnumTypeRef, ResolvedEnumVariantType, ResolvedEnumVariantTypeRef,
    ResolvedExpression, ResolvedExternalFunctionDefinition, ResolvedExternalFunctionDefinitionRef,
    ResolvedInternalFunctionDefinition, ResolvedInternalFunctionDefinitionRef, ResolvedNode,
    ResolvedRustType, ResolvedRustTypeRef, ResolvedStructType, ResolvedStructTypeRef, ResolvedType,
    SemanticError, TypeNumber,
};
use seq_map::SeqMap;
use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ResolvedModulePathStr(pub Vec<String>);

#[derive(Debug)]
pub struct ResolvedModuleNamespace {
    #[allow(unused)]
    structs: SeqMap<String, ResolvedStructTypeRef>,

    constants: SeqMap<String, ResolvedConstantRef>,

    #[allow(unused)]
    build_in_rust_types: SeqMap<String, ResolvedRustTypeRef>,

    #[allow(unused)]
    enum_types: SeqMap<String, ResolvedEnumTypeRef>,
    enum_variant_types: SeqMap<String, ResolvedEnumVariantTypeRef>,

    internal_functions: SeqMap<String, ResolvedInternalFunctionDefinitionRef>,
    external_function_declarations: SeqMap<String, ResolvedExternalFunctionDefinitionRef>,

    pub path: Vec<String>,
}

pub type ResolvedModuleNamespaceRef = Rc<RefCell<ResolvedModuleNamespace>>;

pub struct UtilResolvedParameter {
    pub name: String, // Not used,
    pub is_mutable: bool,
    pub resolved_type: ResolvedType,
}

impl ResolvedModuleNamespace {
    #[must_use]
    pub fn new(path: &[String]) -> Self {
        Self {
            structs: Default::default(),
            build_in_rust_types: Default::default(),
            enum_types: Default::default(),
            enum_variant_types: Default::default(),
            internal_functions: Default::default(),
            external_function_declarations: Default::default(),
            constants: Default::default(),
            path: path.to_vec(),
        }
    }

    pub fn add_constant_ref(
        &mut self,
        constant_ref: ResolvedConstantRef,
    ) -> Result<ResolvedConstantRef, SemanticError> {
        let name = constant_ref.assigned_name.clone();

        self.constants
            .insert(name.to_string(), constant_ref.clone())
            .map_err(|_| SemanticError::DuplicateConstName(name.to_string()))?;

        Ok(constant_ref)
    }

    pub fn add_struct(
        &mut self,
        struct_type: ResolvedStructType,
    ) -> Result<ResolvedStructTypeRef, SemanticError> {
        let name = struct_type.assigned_name.clone();
        let struct_ref = Rc::new(RefCell::new(struct_type));
        self.structs
            .insert(name.clone(), struct_ref.clone())
            .map_err(|_| SemanticError::DuplicateStructName(name))?;

        Ok(struct_ref)
    }

    pub fn add_struct_ref(
        &mut self,
        struct_type_ref: ResolvedStructTypeRef,
    ) -> Result<(), SemanticError> {
        let name = struct_type_ref.borrow().assigned_name.clone();
        self.structs
            .insert(name.clone(), struct_type_ref)
            .map_err(|_| SemanticError::DuplicateStructName(name))?;
        Ok(())
    }

    pub fn add_generated_struct(
        &mut self,
        name: &str,
        fields: &[(&str, ResolvedType)],
    ) -> Result<ResolvedStructTypeRef, SemanticError> {
        let mut resolved_fields = SeqMap::new();

        for (_index, (field_name, field_type)) in fields.iter().enumerate() {
            let af = ResolvedAnonymousStructFieldType {
                identifier: None,
                field_type: field_type.clone(),
            };

            resolved_fields
                .insert(field_name.to_string(), af)
                .map_err(|_| SemanticError::DuplicateFieldName(field_name.to_string()))?;
        }

        let anon_struct_type = ResolvedAnonymousStructType {
            defined_fields: resolved_fields,
        };

        let resolved_struct_type = ResolvedStructType {
            name: ResolvedNode::default(),
            assigned_name: name.to_string(),
            anon_struct_type,
            functions: SeqMap::default(),
        };

        self.add_struct(resolved_struct_type)
    }

    pub fn add_built_in_rust_type(
        &mut self,
        rust_type: ResolvedRustType,
    ) -> Result<ResolvedRustTypeRef, SemanticError> {
        let rust_type_ref = Rc::new(rust_type);
        self.build_in_rust_types
            .insert(rust_type_ref.type_name.clone(), rust_type_ref.clone())
            .map_err(|_| {
                SemanticError::DuplicateRustType(rust_type_ref.clone().type_name.clone())
            })?;

        Ok(rust_type_ref)
    }

    pub fn add_enum_type(
        &mut self,
        //        enum_type_name: &str,
        enum_type_ref: ResolvedEnumTypeRef,
    ) -> Result<ResolvedEnumTypeRef, SemanticError> {
        assert!(!enum_type_ref.module_path.is_empty());
        self.enum_types
            .insert(enum_type_ref.assigned_name.clone(), enum_type_ref.clone())
            .map_err(|_| SemanticError::DuplicateEnumType(enum_type_ref.assigned_name.clone()))?;

        Ok(enum_type_ref)
    }

    pub fn add_enum_variant(
        &mut self,
        enum_type_name: &str,
        enum_variant_name: &str,
        enum_variant: ResolvedEnumVariantType,
    ) -> Result<ResolvedEnumVariantTypeRef, SemanticError> {
        let enum_variant_ref = Rc::new(enum_variant);
        let full_name = format!("{}::{}", enum_type_name, enum_variant_name);
        self.enum_variant_types
            .insert(full_name, enum_variant_ref.clone())
            .map_err(|_| {
                SemanticError::DuplicateEnumVariantType(
                    enum_variant_name.to_string(),
                    enum_variant_name.to_string(),
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
        self.internal_functions
            .insert(name.to_string(), function_ref.clone())
            .expect("todo: add seqmap error handling");
        Ok(function_ref)
    }

    pub fn add_internal_function_link(
        &mut self,
        name: &str,
        function_ref: ResolvedInternalFunctionDefinitionRef,
    ) -> Result<(), SemanticError> {
        self.internal_functions
            .insert(name.to_string(), function_ref.clone())
            .expect("todo: add seqmap error handling");
        Ok(())
    }

    pub fn get_struct(&self, name: &str) -> Option<&ResolvedStructTypeRef> {
        self.structs.get(&name.to_string())
    }

    pub fn get_enum(&self, name: &str) -> Option<&ResolvedEnumTypeRef> {
        self.enum_types.get(&name.to_string())
    }

    pub fn get_constant(&self, name: &str) -> Option<&ResolvedConstantRef> {
        self.constants.get(&name.to_string())
    }

    pub fn get_rust_type(&self, name: &str) -> Option<&ResolvedRustTypeRef> {
        self.build_in_rust_types.get(&name.to_string())
    }

    #[must_use]
    pub fn get_enum_variant_type_str(
        &self,
        enum_name: &str,
        enum_variant_name: &str,
    ) -> Option<&ResolvedEnumVariantTypeRef> {
        let complete_name = format!("{}::{}", enum_name, enum_variant_name);
        let result = self.enum_variant_types.get(&complete_name);
        result
    }

    #[must_use]
    pub fn get_internal_function(
        &self,
        name: &str,
    ) -> Option<&ResolvedInternalFunctionDefinitionRef> {
        self.internal_functions.get(&name.to_string())
    }

    #[must_use]
    pub fn get_external_function_declaration(
        &self,
        name: &str,
    ) -> Option<&ResolvedExternalFunctionDefinitionRef> {
        self.external_function_declarations.get(&name.to_string())
    }

    pub fn add_external_function_declaration(
        &mut self,
        name: &str,
        declaration: ResolvedExternalFunctionDefinition,
    ) -> Result<ResolvedExternalFunctionDefinitionRef, SemanticError> {
        let decl_ref = Rc::new(declaration);
        self.external_function_declarations
            .insert(name.to_string(), decl_ref.clone())
            .map_err(|_| SemanticError::DuplicateExternalFunction(name.to_string()))?;
        Ok(decl_ref)
    }

    pub fn add_external_function_declaration_link(
        &mut self,
        name: &str,
        decl_ref: ResolvedExternalFunctionDefinitionRef,
    ) -> Result<(), SemanticError> {
        self.external_function_declarations
            .insert(name.to_string(), decl_ref.clone())
            .map_err(|_| SemanticError::DuplicateExternalFunction(name.to_string()))?;
        Ok(())
    }
}
