/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::modules::Modules;
use crate::AliasTypeRef;
use crate::{
    AliasType, AnonymousStructFieldType, AnonymousStructType, ConstantRef, EnumType, EnumTypeRef,
    EnumVariantType, EnumVariantTypeRef, ExternalFunctionDefinition, ExternalFunctionDefinitionRef,
    InternalFunctionDefinition, InternalFunctionDefinitionRef, Node, RustType, RustTypeRef,
    SemanticError, StructType, StructTypeRef, Type,
};
use seq_map::SeqMap;
use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

pub trait TypeGenerator: 'static + Debug {
    fn generate_type(
        &self,
        namespace: &mut ModuleNamespace,
        modules: &Modules,
        type_arguments: Vec<Type>,
    ) -> Result<Type, SemanticError>;
}

pub struct ClosureTypeGenerator<F> {
    generator_fn: F,
}

impl<F> ClosureTypeGenerator<F> {
    pub fn new(generator_fn: F) -> Self {
        ClosureTypeGenerator { generator_fn }
    }
}

impl<F> Debug for ClosureTypeGenerator<F>
where
    F: Fn(&mut ModuleNamespace, &Modules, &[Type]) -> Result<Type, SemanticError>,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "some debug")
    }
}

impl<F> TypeGenerator for ClosureTypeGenerator<F>
where
    F: Fn(&mut ModuleNamespace, &Modules, &[Type]) -> Result<Type, SemanticError> + 'static, // Constraint on F: It must be a closure with the correct signature
{
    fn generate_type(
        &self,
        _module_namespace: &mut ModuleNamespace,
        _modules: &Modules,
        type_arguments: Vec<Type>,
    ) -> Result<Type, SemanticError> {
        (self.generator_fn)(_module_namespace, _modules, &type_arguments)
    }
}

#[derive(Debug)]
pub struct ModuleNamespace {
    structs: SeqMap<String, StructTypeRef>,
    aliases: SeqMap<String, AliasTypeRef>,
    constants: SeqMap<String, ConstantRef>,
    type_generators: SeqMap<String, Rc<dyn TypeGenerator>>,

    build_in_rust_types: SeqMap<String, RustTypeRef>,

    enum_types: SeqMap<String, EnumTypeRef>,

    internal_functions: SeqMap<String, InternalFunctionDefinitionRef>,
    external_function_declarations: SeqMap<String, ExternalFunctionDefinitionRef>,

    namespaces: SeqMap<String, ModuleNamespaceRef>,

    pub path: Vec<String>,
}

pub type ModuleNamespaceRef = Rc<RefCell<ModuleNamespace>>;

impl ModuleNamespace {
    #[must_use]
    pub fn new(path: &[String]) -> Self {
        Self {
            structs: SeqMap::default(),
            aliases: SeqMap::default(),
            build_in_rust_types: SeqMap::default(),
            enum_types: SeqMap::default(),
            internal_functions: SeqMap::default(),
            external_function_declarations: SeqMap::default(),
            constants: SeqMap::default(),
            type_generators: SeqMap::default(),
            namespaces: SeqMap::default(),
            path: path.to_vec(),
        }
    }

    #[must_use]
    pub const fn structs(&self) -> &SeqMap<String, StructTypeRef> {
        &self.structs
    }

    #[must_use]
    pub const fn constants(&self) -> &SeqMap<String, ConstantRef> {
        &self.constants
    }

    #[must_use]
    pub const fn aliases(&self) -> &SeqMap<String, AliasTypeRef> {
        &self.aliases
    }

    #[must_use]
    pub const fn namespaces(&self) -> &SeqMap<String, ModuleNamespaceRef> {
        &self.namespaces
    }

    #[must_use]
    pub const fn enums(&self) -> &SeqMap<String, EnumTypeRef> {
        &self.enum_types
    }

    #[must_use]
    pub const fn internal_functions(&self) -> &SeqMap<String, InternalFunctionDefinitionRef> {
        &self.internal_functions
    }

    #[must_use]
    pub const fn external_function_declarations(
        &self,
    ) -> &SeqMap<String, ExternalFunctionDefinitionRef> {
        &self.external_function_declarations
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

    pub fn add_alias(&mut self, alias_type: AliasType) -> Result<AliasTypeRef, SemanticError> {
        let name = alias_type.assigned_name.clone();
        let alias_ref = Rc::new(alias_type);
        self.aliases
            .insert(name.clone(), alias_ref.clone())
            .map_err(|_| SemanticError::DuplicateStructName(name))?;

        Ok(alias_ref)
    }

    pub fn add_struct(&mut self, struct_type: StructType) -> Result<StructTypeRef, SemanticError> {
        let name = struct_type.assigned_name.clone();
        let struct_ref = Rc::new(RefCell::new(struct_type));
        self.structs
            .insert(name.clone(), struct_ref.clone())
            .map_err(|_| SemanticError::DuplicateStructName(name))?;

        Ok(struct_ref)
    }

    pub fn add_generator(
        &mut self,
        name: &str,
        generator: Rc<dyn TypeGenerator>,
    ) -> Result<(), SemanticError> {
        self.type_generators
            .insert(name.to_string(), generator)
            .map_err(|_| SemanticError::DuplicateStructName(name.to_string()))?;
        Ok(())
    }

    pub fn add_struct_ref(&mut self, struct_type_ref: StructTypeRef) -> Result<(), SemanticError> {
        let name = struct_type_ref.borrow().assigned_name.clone();
        self.structs
            .insert(name.clone(), struct_type_ref)
            .map_err(|_| SemanticError::DuplicateStructName(name))?;
        Ok(())
    }

    pub fn add_type_generator_ref(
        &mut self,
        name: &str,
        type_generator: Rc<dyn TypeGenerator>,
    ) -> Result<(), SemanticError> {
        self.type_generators
            .insert(name.parse().unwrap(), type_generator)
            .map_err(|_| SemanticError::DuplicateStructName(name.to_string()))?;
        Ok(())
    }

    pub fn add_generated_struct(
        &mut self,
        name: &str,
        fields: &[(&str, Type)],
    ) -> Result<StructTypeRef, SemanticError> {
        let mut _fields = SeqMap::new();

        for (_index, (field_name, field_type)) in fields.iter().enumerate() {
            let af = AnonymousStructFieldType {
                identifier: None,
                field_type: field_type.clone(),
            };

            _fields
                .insert(field_name.to_string(), af)
                .map_err(|_| SemanticError::DuplicateFieldName(field_name.to_string()))?;
        }

        let anon_struct_type = AnonymousStructType {
            defined_fields: _fields,
        };

        let _struct_type = StructType {
            name: Node::default(),
            assigned_name: name.to_string(),
            anon_struct_type,
            functions: SeqMap::default(),
        };

        self.add_struct(_struct_type)
    }

    /// # Errors
    ///
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

    pub fn add_enum_type(&mut self, enum_type: EnumType) -> Result<EnumTypeRef, SemanticError> {
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

    pub fn get_struct(&self, name: &str) -> Option<StructTypeRef> {
        if let Some(found_alias) = self.aliases.get(&name.to_string()) {
            let alias_type = found_alias.referenced_type.clone();
            if let Type::Struct(found_data) = alias_type {
                return Some(found_data);
            }
        }
        self.structs.get(&name.to_string()).cloned()
    }

    pub fn get_alias(&self, name: &str) -> Option<AliasTypeRef> {
        if let Some(found_alias) = self.aliases.get(&name.to_string()) {
            return Some(found_alias.clone());
        }

        None
    }

    pub fn get_alias_referred_type(&self, name: &str) -> Option<Type> {
        if let Some(found_alias) = self.aliases.get(&name.to_string()) {
            let alias_type = found_alias.referenced_type.clone();
            return Some(alias_type);
        }

        None
    }

    pub fn fetch_struct(&self, name: &str) -> StructTypeRef {
        self.get_struct(name)
            .expect(&format!("should have the struct {}", name))
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
    pub fn get_internal_function(&self, name: &str) -> Option<&InternalFunctionDefinitionRef> {
        self.internal_functions.get(&name.to_string())
    }

    #[must_use]
    pub fn get_namespace_link(&self, name: &str) -> Option<ModuleNamespaceRef> {
        self.namespaces.get(&name.to_string()).cloned()
    }

    #[must_use]
    pub fn get_external_function_declaration(
        &self,
        name: &str,
    ) -> Option<&ExternalFunctionDefinitionRef> {
        self.external_function_declarations.get(&name.to_string())
    }

    #[must_use]
    pub fn get_type_generator(&self, name: &str) -> Option<Rc<dyn TypeGenerator>> {
        self.type_generators.get(&name.to_string()).map(Rc::clone)
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

    pub fn add_namespace_link(
        &mut self,
        name: &str,
        ns: ModuleNamespaceRef,
    ) -> Result<(), SemanticError> {
        self.namespaces
            .insert(name.to_string(), ns)
            .map_err(|_| SemanticError::DuplicateExternalFunction(name.to_string()))?;
        Ok(())
    }
}
