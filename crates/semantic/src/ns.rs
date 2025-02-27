/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::symtbl::{SymbolTable, SymbolTableRef};
use crate::Type;
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ModulePathStr(pub Vec<String>);

pub type NamespacePath = Vec<String>;

#[derive(Debug)]
pub struct Namespace {
    /*
    #[allow(unused)]
    structs: SeqMap<String, StructTypeRef>,

    constants: SeqMap<String, ConstantRef>,

    #[allow(unused)]
    build_in_rust_types: SeqMap<String, RustTypeRef>,

    #[allow(unused)]
    enum_types: SeqMap<String, EnumTypeRef>,
    //enum_variant_types: SeqMap<String, EnumVariantTypeRef>,
    internal_functions: SeqMap<String, InternalFunctionDefinitionRef>,
    external_function_declarations: SeqMap<String, ExternalFunctionDefinitionRef>,

     */
    pub symbol_table: SymbolTableRef,

    pub path: Vec<String>,
}

pub struct UtilParameter {
    pub name: String, // Not used,
    pub is_mutable: bool,
    pub resolved_type: Type,
}

impl Namespace {
    #[must_use]
    pub fn new(path: NamespacePath, symbol_table: SymbolTable) -> Self {
        Self {
            path,
            symbol_table: Rc::new(symbol_table),
        }
    }

    /*
       pub fn structs(&self) -> &SeqMap<String, StructTypeRef> {
           &self.structs
       }

       pub fn add_constant_ref(
           &mut self,
           constant_ref: ConstantRef,
       ) -> Result<ConstantRef, SemanticError> {
           let name = constant_ref.assigned_name.clone();

           self.constants
               .insert(name.to_string(), constant_ref.clone())
               .map_err(|_| SemanticError::DuplicateConstName(name.to_string()))?;

           Ok(constant_ref)
       }

       pub fn add_struct(
           &mut self,
           struct_type: StructType,
       ) -> Result<StructTypeRef, SemanticError> {
           let name = struct_type.assigned_name.clone();
           let struct_ref = Rc::new(RefCell::new(struct_type));
           self.structs
               .insert(name.clone(), struct_ref.clone())
               .map_err(|_| SemanticError::DuplicateStructName(name))?;

           Ok(struct_ref)
       }

       pub fn add_struct_ref(
           &mut self,
           struct_type_ref: StructTypeRef,
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
           fields: &[(&str, Type)],
       ) -> Result<StructTypeRef, SemanticError> {
           let mut resolved_fields = SeqMap::new();

           for (_index, (field_name, field_type)) in fields.iter().enumerate() {
               let af = AnonymousStructFieldType {
                   identifier: None,
                   field_type: field_type.clone(),
               };

               resolved_fields
                   .insert(field_name.to_string(), af)
                   .map_err(|_| SemanticError::DuplicateFieldName(field_name.to_string()))?;
           }

           let anon_struct_type = AnonymousStructType {
               defined_fields: resolved_fields,
           };

           let resolved_struct_type = StructType {
               name: Node::default(),
               assigned_name: name.to_string(),
               anon_struct_type,
               functions: SeqMap::default(),
           };

           self.add_struct(resolved_struct_type)
       }

       pub fn add_built_in_rust_type(
           &mut self,
           rust_type: RustType,
       ) -> Result<RustTypeRef, SemanticError> {
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
           enum_type: EnumType,
       ) -> Result<EnumTypeRef, SemanticError> {
           let enum_type_ref = Rc::new(RefCell::new(enum_type));

           assert!(!enum_type_ref.borrow().module_path.is_empty());
           self.enum_types
               .insert(
                   enum_type_ref.borrow().assigned_name.clone(),
                   enum_type_ref.clone(),
               )
               .map_err(|_| {
                   SemanticError::DuplicateEnumType(enum_type_ref.borrow().assigned_name.clone())
               })?;

           Ok(enum_type_ref)
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
           self.internal_functions
               .insert(name.to_string(), function_ref.clone())
               .expect("todo: add seqmap error handling");
           Ok(function_ref)
       }

       pub fn add_internal_function_link(
           &mut self,
           name: &str,
           function_ref: InternalFunctionDefinitionRef,
       ) -> Result<(), SemanticError> {
           self.internal_functions
               .insert(name.to_string(), function_ref.clone())
               .expect("todo: add seqmap error handling");
           Ok(())
       }

       pub fn get_struct(&self, name: &str) -> Option<&StructTypeRef> {
           self.structs.get(&name.to_string())
       }

       pub fn get_enum(&self, name: &str) -> Option<&EnumTypeRef> {
           self.enum_types.get(&name.to_string())
       }

       pub fn get_constant(&self, name: &str) -> Option<&ConstantRef> {
           self.constants.get(&name.to_string())
       }

       pub fn get_rust_type(&self, name: &str) -> Option<&RustTypeRef> {
           self.build_in_rust_types.get(&name.to_string())
       }

       #[must_use]
       pub fn get_internal_function(
           &self,
           name: &str,
       ) -> Option<&InternalFunctionDefinitionRef> {
           self.internal_functions.get(&name.to_string())
       }

       #[must_use]
       pub fn get_external_function_declaration(
           &self,
           name: &str,
       ) -> Option<&ExternalFunctionDefinitionRef> {
           self.external_function_declarations.get(&name.to_string())
       }

       pub fn add_external_function_declaration(
           &mut self,
           name: &str,
           declaration: ExternalFunctionDefinition,
       ) -> Result<ExternalFunctionDefinitionRef, SemanticError> {
           let decl_ref = Rc::new(declaration);
           self.external_function_declarations
               .insert(name.to_string(), decl_ref.clone())
               .map_err(|_| SemanticError::DuplicateExternalFunction(name.to_string()))?;
           Ok(decl_ref)
       }

       pub fn add_external_function_declaration_link(
           &mut self,
           name: &str,
           decl_ref: ExternalFunctionDefinitionRef,
       ) -> Result<(), SemanticError> {
           self.external_function_declarations
               .insert(name.to_string(), decl_ref.clone())
               .map_err(|_| SemanticError::DuplicateExternalFunction(name.to_string()))?;
           Ok(())
       }

    */
}
