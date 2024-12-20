use crate::ns::ResolvedModuleNamespace;
use crate::NamespaceError;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use swamp_script_ast::StructType;
use swamp_script_semantic::{
    ResolvedDefinition, ResolvedEnumType, ResolvedEnumTypeRef, ResolvedEnumVariantType,
    ResolvedEnumVariantTypeRef, ResolvedExternalFunctionDefinitionRef,
    ResolvedInternalFunctionDefinitionRef, ResolvedModulePath, ResolvedRustTypeRef,
    ResolvedStatement, ResolvedStructTypeRef, ResolvedType,
};

#[derive(Debug)]
pub struct ResolvedModules {
    pub modules: HashMap<ResolvedModulePath, ResolvedModuleRef>,
}

impl Default for ResolvedModules {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct ResolvedModule {
    pub definitions: Vec<ResolvedDefinition>,
    pub statements: Vec<ResolvedStatement>,
    pub namespace: ResolvedModuleNamespace,
}

pub type ResolvedModuleRef = Rc<RefCell<ResolvedModule>>;

impl ResolvedModule {
    pub fn new(module_path: ResolvedModulePath) -> Self {
        Self {
            definitions: Vec::new(),
            namespace: ResolvedModuleNamespace::new(module_path),
            statements: Vec::new(),
        }
    }
}

impl ResolvedModules {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }
    /*

    pub fn add_module(&mut self, module_ref: ResolvedModuleRef) -> Result<(), SemanticError> {
        let name = module_ref.borrow().namespace.path.0.clone();
        self.modules.insert(name, module_ref);
        Ok(())
    }

    pub fn add_linked_module(
        &mut self,
        module_path: ResolvedModulePath,
        module: ResolvedModuleRef,
    ) -> Result<(), SemanticError> {
        self.modules.insert(module_path.clone(), module);
        Ok(())
    }

    pub fn get(&self, module_path: &ResolvedModulePath) -> Option<&ResolvedModuleRef> {
        self.modules.get(module_path)
    }

    pub fn get_mut(&mut self, module_path: &ResolvedModulePath) -> Option<&mut ResolvedModuleRef> {
        self.modules.get_mut(module_path)
    }

    pub fn contains_key(&self, module_path: ResolvedModulePath) -> bool {
        self.modules.contains_key(&module_path)
    }

     */

    pub fn get_struct(&self, path: &Vec<String>, name: &str) -> Option<ResolvedStructTypeRef> {
        todo!()
    }
    pub fn get_type_alias(&self, path: &Vec<String>, name: &str) -> Option<ResolvedType> {
        todo!()
    }

    pub fn get_enum(&self, path: &Vec<String>, name: &str) -> Option<ResolvedEnumTypeRef> {
        todo!()
    }

    pub fn get_enum_variant_type(
        &self,
        path: &Vec<String>,
        name: &str,
    ) -> Option<ResolvedEnumVariantTypeRef> {
        todo!()
    }

    pub fn get_external_function_declaration(
        &self,
        path: &[String],
        name: &str,
    ) -> Option<ResolvedExternalFunctionDefinitionRef> {
        todo!()
    }

    pub fn get_internal_function(
        &self,
        path: &[String],
        name: &str,
    ) -> Option<ResolvedInternalFunctionDefinitionRef> {
        todo!()
    }

    pub fn get_rust_type(&self, name: &str) -> Option<ResolvedRustTypeRef> {
        todo!()
    }

    pub fn add_enum_type(
        &self,
        enum_type: &ResolvedEnumType,
    ) -> Result<ResolvedEnumTypeRef, NamespaceError> {
        todo!()
    }

    pub fn add_enum_variant(
        &self,
        enum_variant: &ResolvedEnumVariantType,
    ) -> Result<(), NamespaceError> {
        todo!()
    }

    pub fn add_struct_type(&self, struct_type: StructType) -> Result<(), NamespaceError> {
        todo!()
    }

    pub fn add_internal_function_ref(
        &self,
        internal_func: ResolvedInternalFunctionDefinitionRef,
    ) -> Result<(), NamespaceError> {
        todo!()
    }
}
