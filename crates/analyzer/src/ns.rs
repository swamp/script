/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use seq_map::SeqMap;
use std::fmt::Debug;
use std::rc::Rc;
use swamp_script_semantic::{
    ResolvedEnumTypeRef, ResolvedEnumVariantTypeRef, ResolvedExternalFunctionDefinitionRef,
    ResolvedInternalFunctionDefinitionRef, ResolvedModulePath, ResolvedRustType,
    ResolvedRustTypeRef, ResolvedStructTypeRef, ResolvedType, SemanticError,
};

#[derive(Debug, Clone)]
pub struct ResolvedModulePathStr(pub Vec<String>);

#[derive(Debug)]
pub struct ResolvedModuleNamespace {
    #[allow(unused)]
    structs: SeqMap<String, ResolvedStructTypeRef>,

    #[allow(unused)]
    build_in_rust_types: SeqMap<String, ResolvedRustTypeRef>,

    #[allow(unused)]
    enum_types: SeqMap<String, ResolvedEnumTypeRef>,
    enum_variant_types: SeqMap<String, ResolvedEnumVariantTypeRef>,

    internal_functions: SeqMap<String, ResolvedInternalFunctionDefinitionRef>,
    external_function_declarations: SeqMap<String, ResolvedExternalFunctionDefinitionRef>,

    #[allow(unused)]
    type_aliases: SeqMap<String, ResolvedType>,

    pub path: ResolvedModulePath,
}

impl ResolvedModuleNamespace {
    /*
    pub fn util_insert_struct_type(
        &mut self,
        number: TypeNumber,
        fields: &[(&str, ResolvedType)],
    ) -> Result<ResolvedStructTypeRef, SemanticError> {
        let mut seq_map = SeqMap::new();
        for (name, resolved_type) in fields {
            seq_map.insert(
                name.to_string(),
                resolved_type.clone(),
            )?;
        }

        let resolved_definition = ResolvedStructType {
            module_path: ResolvedModulePath(vec![]),
            functions: SeqMap::default(),
            fields: Default::default(),
            number: 0,
            name: ResolvedLocalTypeIdentifier(Node::new_unknown()),
        };

        self.add_struct_type(resolved_definition)
    }


    pub fn util_create_external_function(
        &mut self,
        name: &str,
        unique_id: ExternalFunctionId,
        parameters: &[ResolvedParameter],
        return_type: ResolvedType,
    ) -> Result<ResolvedExternalFunctionDefinitionRef, SemanticError> {
        let ext = ResolvedExternalFunctionDefinition {
            name: ResolvedLocalIdentifier(),
            signature: ResolvedFunctionSignature {
                parameters: parameters.to_vec(),
                return_type,
            },
            id: unique_id,
        };
        let ext_ref = Rc::new(ext);

        Ok(ext_ref)
    }

    pub fn util_add_external_function(
        &mut self,
        name: &str,
        unique_id: ExternalFunctionId,
        parameters: &[ResolvedParameter],
        return_type: ResolvedType,
    ) -> Result<(), SemanticError> {
        let fn_ref =
            self.util_create_external_function(name, unique_id, parameters, return_type)?;

        self.add_external_function_declaration_ref(fn_ref)?;

        Ok(())
    }

    pub fn util_add_member_external_function(
        &mut self,
        resolved_type: &ResolvedType,
        name: &str,
        unique_id: ExternalFunctionId,
        parameters: &[ResolvedParameter],
        return_type: ResolvedType,
    ) -> Result<ResolvedExternalFunctionDefinitionRef, SemanticError> {
        let ext_ref =
            self.util_create_external_function(name, unique_id, parameters, return_type)?;

        let function = ResolvedFunction::External(ext_ref.clone());
        let function_ref = Rc::new(function);
        match resolved_type {
            ResolvedType::Struct(struct_type_ref) => {
                struct_type_ref
                    .borrow_mut()
                    .functions
                    .insert(name.to_string(), function_ref.clone())?;
            }
            _ => return Err(SemanticError::CanOnlyUseStructForMemberFunctions),
        }
        self.add_external_function_declaration_ref(ext_ref.clone())?;

        Ok(ext_ref)
    }

    */
}

impl ResolvedModuleNamespace {
    #[must_use]
    pub fn new(path: ResolvedModulePath) -> Self {
        Self {
            structs: Default::default(),
            build_in_rust_types: Default::default(),
            enum_types: Default::default(),
            enum_variant_types: Default::default(),
            internal_functions: Default::default(),
            external_function_declarations: Default::default(),
            type_aliases: Default::default(),
            path,
        }
    }

    /*
    pub fn add_struct_type(
        &mut self,
        struct_type: ResolvedStructType,
    ) -> Result<ResolvedStructTypeRef, SemanticError> {
        let struct_ref = Rc::new(RefCell::new(struct_type));
        self.structs.insert(
            LocalTypeName(struct_ref.borrow().name.text.clone()),
            struct_ref.clone(),
        )?;

        Ok(struct_ref)
    }


     */
    pub fn add_built_in_rust_type(
        &mut self,
        rust_type: ResolvedRustType,
    ) -> Result<ResolvedRustTypeRef, SemanticError> {
        let rust_type_ref = Rc::new(rust_type);
        /* TODO:
        self.build_in_rust_types.insert(
            LocalTypeName(rust_type_ref.type_name.clone()),
            rust_type_ref.clone(),
        )?;

         */

        Ok(rust_type_ref)
    }

    pub fn add_type_alias(
        &mut self,
        name: &str,
        _resolved_type: ResolvedType,
    ) -> Result<(), SemanticError> {
        let _name_str = name.to_string();
        /* TODO:
        self.type_aliases
            .insert(LocalTypeName(name_str.clone()), resolved_type)
            .map_err(|_| SemanticError::DuplicateTypeAlias(name_str))?;

         */
        Ok(())
    }

    /*

        pub fn get_type_alias(&self, name: &String) -> Option<&ResolvedType> {
            self.type_aliases.get(&(name).into())
        }

        pub fn add_internal_function(
            &mut self,
            function_definition: ResolvedInternalFunctionDefinition,
        ) -> Result<ResolvedInternalFunctionDefinitionRef, SemanticError> {
            let function_ref = Rc::new(function_definition);
            self.add_internal_function_ref(&function_ref)?;
            Ok(function_ref)
        }

        pub fn add_internal_function_ref(
            &mut self,
            function_ref: &ResolvedInternalFunctionDefinitionRef,
        ) -> Result<(), SemanticError> {
            self.internal_functions
                .insert(function_ref.name.text.clone(), function_ref.clone())?;
            Ok(())
        }

        pub fn add_external_function_declaration(
            &mut self,
            name: String,
            function_declaration: ResolvedExternalFunctionDefinition,
        ) -> Result<ResolvedExternalFunctionDefinitionRef, SemanticError> {
            let function_ref = Rc::new(function_declaration);
            self.external_function_declarations
                .insert(name.clone(), function_ref.clone())?;
            Ok(function_ref)
        }

        pub fn add_external_function_declaration_ref(
            &mut self,
            function_ref: ResolvedExternalFunctionDefinitionRef,
        ) -> Result<ResolvedExternalFunctionDefinitionRef, SemanticError> {
            self.external_function_declarations
                .insert(function_ref.name.text.clone(), function_ref.clone())?;
            Ok(function_ref)
        }

        pub fn add_enum_variant(
            &mut self,
            enum_variant: ResolvedEnumVariantType,
        ) -> Result<ResolvedEnumVariantTypeRef, SemanticError> {
            let enum_variant_ref = Rc::new(enum_variant);
            let complete_name = &enum_variant_ref.complete_name();
            self.enum_variant_types.insert(
                LocalTypeName(complete_name.clone()),
                enum_variant_ref.clone(),
            )?;
            Ok(enum_variant_ref)
        }

        pub fn create_enum_type(
            &mut self,
            enum_type_name: &ResolvedLocalTypeIdentifier,
            number: TypeNumber,
        ) -> Result<ResolvedEnumTypeRef, SemanticError> {
            let resolved_parent_type =
                ResolvedEnumType::new(enum_type_name.clone(), self.path.clone(), number);

            let enum_type_ref = Rc::new(resolved_parent_type);

            self.enum_types.insert(
                LocalTypeName(enum_type_name.text.clone()),
                enum_type_ref.clone(),
            )?;

            Ok(enum_type_ref)
        }

        pub fn get_or_create_tuple(&mut self, types: Vec<ResolvedType>) -> ResolvedTupleTypeRef {
            // TODO: for now, just create new types, in the future we should check if we can reuse a type
            let tuple_type = Rc::new(ResolvedTupleType::new(types));
            self.tuples.push(tuple_type.clone());

            tuple_type
        }

        pub fn get_struct(&self, name: &ResolvedLocalTypeIdentifier) -> Option<&ResolvedStructTypeRef> {
            // TODO: Add scope support, for now just ignore it
            self.structs.get(&(&name.text).into())
        }

        pub fn get_local_struct(
            &self,
            name: &ResolvedLocalTypeIdentifier,
        ) -> Option<&ResolvedStructTypeRef> {
            self.structs.get(&(&name.text).into())
        }

        pub fn get_enum(&self, name: &ResolvedLocalTypeIdentifier) -> Option<&ResolvedEnumTypeRef> {
            self.enum_types.get(&(&name.text).into())
        }

        pub fn get_built_in_rust_type(
            &self,
            name: &ResolvedLocalTypeIdentifier,
        ) -> Option<&ResolvedRustTypeRef> {
            self.build_in_rust_types.get(&(&name.text).into())
        }
    */

    pub fn get_enum_variant_type_str(
        &self,
        enum_name: &str,
        enum_variant_name: &str,
    ) -> Option<&ResolvedEnumVariantTypeRef> {
        let complete_name = format!("{}::{}", enum_name, enum_variant_name);
        let result = self.enum_variant_types.get(&complete_name);
        result
    }

    pub fn get_internal_function(
        &self,
        name: &str,
    ) -> Option<&ResolvedInternalFunctionDefinitionRef> {
        self.internal_functions.get(&name.to_string())
    }

    pub fn get_external_function_declaration(
        &self,
        name: &str,
    ) -> Option<&ResolvedExternalFunctionDefinitionRef> {
        self.external_function_declarations.get(&name.to_string())
    }
}