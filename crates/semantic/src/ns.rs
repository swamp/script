/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::{
    comma_seq_nl, ExternalFunctionId, Hash, ResolvedEnumType, ResolvedEnumTypeRef,
    ResolvedEnumVariantType, ResolvedEnumVariantTypeRef, ResolvedExternalFunctionDefinition,
    ResolvedExternalFunctionDefinitionRef, ResolvedFunction, ResolvedFunctionSignature,
    ResolvedInternalFunctionDefinition, ResolvedInternalFunctionDefinitionRef, ResolvedParameter,
    ResolvedStructType, ResolvedStructTypeRef, ResolvedTupleType, ResolvedTupleTypeRef,
    ResolvedType, TypeNumber,
};
use seq_map::{SeqMap, SeqMapError};
use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::rc::Rc;
use swamp_script_ast::{
    IdentifierName, LocalIdentifier, LocalTypeIdentifier, ModulePath, Node, Position, Span,
    StructType,
};
use tracing::info;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct LocalTypeName(pub String);

impl Display for LocalTypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub struct ResolvedModuleNamespace {
    pub all_owned_types: SeqMap<LocalTypeIdentifier, ResolvedType>,

    structs: SeqMap<LocalTypeName, ResolvedStructTypeRef>, // They are created by the module, so they are owned here
    enum_types: SeqMap<LocalTypeName, ResolvedEnumTypeRef>, // They are created by the module, so they are owned here
    enum_variant_types: SeqMap<LocalTypeName, ResolvedEnumVariantTypeRef>, // They are created by the module, so they are owned here

    tuples: Vec<ResolvedTupleTypeRef>,

    internal_functions: SeqMap<String, ResolvedInternalFunctionDefinitionRef>,
    external_function_declarations: SeqMap<String, ResolvedExternalFunctionDefinitionRef>,
    pub type_aliases: SeqMap<LocalTypeName, ResolvedType>,

    type_number: TypeNumber,

    pub path: ModulePath,
}

impl ResolvedModuleNamespace {
    pub fn allocate_number(&mut self) -> TypeNumber {
        self.type_number += 1;
        self.type_number
    }

    pub fn util_insert_struct_type(
        &mut self,
        name: &str,
        fields: &[(&str, ResolvedType)],
    ) -> Result<ResolvedStructTypeRef, SemanticError> {
        let mut seq_map = SeqMap::new();
        for (name, resolved_type) in fields {
            seq_map.insert(IdentifierName(name.to_string()), resolved_type.clone())?;
        }

        let resolved_definition = ResolvedStructType {
            number: 0,
            module_path: self.path.clone(),
            fields: seq_map.clone(),
            name: LocalTypeIdentifier {
                node: Node {
                    span: Span {
                        start: Position {
                            offset: 0,
                            line: 0,
                            column: 0,
                        },
                        end: Position {
                            offset: 0,
                            line: 0,
                            column: 0,
                        },
                    },
                },
                text: name.to_string(),
            },
            ast_struct: StructType {
                identifier: LocalTypeIdentifier {
                    node: Node {
                        span: Span {
                            start: Position {
                                offset: 0,
                                line: 0,
                                column: 0,
                            },
                            end: Position {
                                offset: 0,
                                line: 0,
                                column: 0,
                            },
                        },
                    },
                    text: name.to_string(),
                },
                fields: SeqMap::default(),
            },
            functions: SeqMap::default(),
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
            name: LocalIdentifier {
                node: Node {
                    span: Span {
                        start: Position {
                            offset: 0,
                            line: 0,
                            column: 0,
                        },
                        end: Position {
                            offset: 0,
                            line: 0,
                            column: 0,
                        },
                    },
                },
                text: name.to_string(),
            },
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
                    .insert(IdentifierName(name.to_string()), function_ref.clone())?;
            }
            _ => return Err(SemanticError::CanOnlyUseStructForMemberFunctions),
        }
        self.add_external_function_declaration_ref(ext_ref.clone())?;

        Ok(ext_ref)
    }
}

impl Display for ResolvedModuleNamespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.structs.is_empty() {
            writeln!(f, "structs:")?;
            for (_struct_name, struct_type_ref) in &self.structs {
                let struct_ref = struct_type_ref.borrow();
                writeln!(f, "{}", struct_ref)?;
                writeln!(f, "impl:\n{}", comma_seq_nl(&struct_ref.functions, ".."))?;
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

#[derive(Debug)]
pub enum SemanticError {
    CouldNotInsertStruct,
    SeqMapError(SeqMapError),
    DuplicateTypeAlias(String),
    CanOnlyUseStructForMemberFunctions,
    ResolveNotStruct,
}

impl From<SeqMapError> for SemanticError {
    fn from(err: SeqMapError) -> Self {
        Self::SeqMapError(err)
    }
}

impl ResolvedModuleNamespace {
    #[must_use]
    pub fn new(path: ModulePath) -> Self {
        Self {
            all_owned_types: Default::default(),
            structs: Default::default(),
            enum_types: Default::default(),
            enum_variant_types: Default::default(),
            tuples: vec![],
            internal_functions: Default::default(),
            external_function_declarations: Default::default(),
            type_aliases: Default::default(),
            type_number: 0,
            path,
        }
    }

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

    pub fn add_type_alias(
        &mut self,
        name: &str,
        resolved_type: ResolvedType,
    ) -> Result<(), SemanticError> {
        let name_str = name.to_string();
        self.type_aliases
            .insert(LocalTypeName(name_str.clone()), resolved_type)
            .map_err(|_| SemanticError::DuplicateTypeAlias(name_str))?;
        Ok(())
    }

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
        info!(
            "Adding variant: '{}' with number {}",
            complete_name, enum_variant_ref.number
        );
        self.enum_variant_types.insert(
            LocalTypeName(complete_name.clone()),
            enum_variant_ref.clone(),
        )?;
        Ok(enum_variant_ref)
    }

    pub fn create_enum_type(
        &mut self,
        enum_type_name: &LocalTypeIdentifier,
        number: TypeNumber,
    ) -> Result<ResolvedEnumTypeRef, SemanticError> {
        let resolved_parent_type = ResolvedEnumType::new(enum_type_name.clone(), number);

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
        enum_name: &LocalTypeIdentifier,
        enum_variant_name: &LocalTypeIdentifier,
    ) -> Option<&ResolvedEnumVariantTypeRef> {
        let complete_name =
            LocalTypeName(format!("{}::{}", enum_name.text, enum_variant_name.text));
        info!("Looking up variant: '{}' in namespace", complete_name);
        let result = self.enum_variant_types.get(&complete_name);
        if let Some(variant) = result {
            info!("Found variant with number: {}", variant.number);
        } else {
            info!("Variant not found!");
        }
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
