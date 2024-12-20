use crate::lookup::ResolvedModuleNamespaceRef;
use crate::ns::ResolvedModuleNamespace;
use crate::{NamespaceError, ResolveError};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;
use swamp_script_ast::StructType;
use swamp_script_semantic::{
    ResolvedDefinition, ResolvedEnumType, ResolvedEnumTypeRef, ResolvedEnumVariantType,
    ResolvedEnumVariantTypeRef, ResolvedExternalFunctionDefinitionRef,
    ResolvedInternalFunctionDefinitionRef, ResolvedModulePathRef, ResolvedStatement,
};

#[derive(Debug)]
pub struct ResolvedModules {
    pub modules: HashMap<Vec<String>, ResolvedModuleRef>,
}

impl Default for ResolvedModules {
    fn default() -> Self {
        Self::new()
    }
}

pub struct ResolvedModule {
    pub definitions: Vec<ResolvedDefinition>,
    pub statements: Vec<ResolvedStatement>,
    pub namespace: ResolvedModuleNamespaceRef,
}

impl Debug for ResolvedModule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for resolved_def in &self.definitions {
            writeln!(f, "{resolved_def:?}")?;
        }

        if !self.definitions.is_empty() && !self.statements.is_empty() {
            writeln!(f, "---\n")?;
        }

        for resolved_statement in &self.statements {
            writeln!(f, "{resolved_statement:?}")?;
        }

        Ok(())
    }
}

pub type ResolvedModuleRef = Rc<RefCell<ResolvedModule>>;

impl ResolvedModule {
    pub fn new(module_path: &[String]) -> Self {
        let ns_ref = Rc::new(RefCell::new(ResolvedModuleNamespace::new(module_path)));
        Self {
            definitions: Vec::new(),
            namespace: ns_ref,
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

    pub fn add_empty_module(&mut self, module_path: &[String]) -> ResolvedModuleRef {
        let ns_ref = Rc::new(RefCell::new(ResolvedModuleNamespace::new(module_path)));
        let module = ResolvedModule {
            definitions: vec![],
            statements: vec![],
            namespace: ns_ref,
        };
        let module_ref = Rc::new(RefCell::new(module));

        self.modules
            .insert(Vec::from(module_path), module_ref.clone());

        module_ref
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
    pub fn find_module(&self, _module_path: &Vec<String>) -> Option<ResolvedModulePathRef> {
        todo!()
    }

    pub fn get_enum_variant_type(
        &self,
        _path: &Vec<String>,
        _name: &str,
    ) -> Option<ResolvedEnumVariantTypeRef> {
        todo!()
    }

    fn get_module(&self, path: &[String]) -> Option<ResolvedModuleRef> {
        self.modules.get(path).cloned()
    }

    fn get_namespace(&self, path: &[String]) -> Option<ResolvedModuleNamespaceRef> {
        self.modules
            .get(path)
            .map(|module| module.borrow().namespace.clone())
    }

    fn get_module_mut(&self, path: &[String]) -> Option<ResolvedModuleRef> {
        self.modules.get(path).cloned()
    }

    fn get_namespace_mut(&mut self, path: &[String]) -> Option<ResolvedModuleNamespaceRef> {
        self.modules
            .get(path)
            .map(|module| module.borrow_mut().namespace.clone())
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

    pub fn add_enum_type(
        &mut self,
        path: &[String],
        name: &str,
        enum_type: ResolvedEnumType,
    ) -> Result<ResolvedEnumTypeRef, ResolveError> {
        let mut namespace = self
            .get_namespace_mut(path)
            .expect("tried to insert in a wrong module path");
        let enum_type_ref = Rc::new(enum_type);
        namespace
            .borrow_mut()
            .add_enum_type(name, enum_type_ref.clone())?;

        Ok(enum_type_ref)
    }

    pub fn add_enum_variant(
        &self,
        _enum_variant: &ResolvedEnumVariantType,
    ) -> Result<(), NamespaceError> {
        todo!()
    }

    pub fn add_struct_type(&self, _struct_type: StructType) -> Result<(), NamespaceError> {
        todo!()
    }
}
