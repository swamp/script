use crate::modules::ResolvedModules;
use crate::ns::ResolvedModuleNamespace;
use crate::ResolveError;
use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_semantic::{
    ResolvedEnumType, ResolvedEnumTypeRef, ResolvedEnumVariantType, ResolvedEnumVariantTypeRef,
    ResolvedExternalFunctionDefinitionRef, ResolvedInternalFunctionDefinition,
    ResolvedInternalFunctionDefinitionRef, ResolvedRustTypeRef, ResolvedStructType,
    ResolvedStructTypeRef, ResolvedType,
};

pub type ResolvedModuleNamespaceRef = Rc<RefCell<ResolvedModuleNamespace>>;

#[derive()]
pub struct NameLookup<'a> {
    namespace: Rc<RefCell<ResolvedModuleNamespace>>,
    modules: &'a ResolvedModules,
}

impl<'a> NameLookup<'a> {
    pub fn new(
        namespace: Rc<RefCell<ResolvedModuleNamespace>>,
        modules: &'a ResolvedModules,
    ) -> Self {
        Self { namespace, modules }
    }
    fn get_namespace(&self, path: &[String]) -> Option<ResolvedModuleNamespaceRef> {
        if path.is_empty() {
            Some(self.namespace.clone())
        } else {
            self.modules
                .modules
                .get(path)
                .map(|module| module.borrow_mut().namespace.clone())
        }
    }

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

    pub fn get_struct(&self, path: &Vec<String>, name: &str) -> Option<ResolvedStructTypeRef> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(
            || None,
            |found_ns| found_ns.borrow().get_struct(name).cloned(),
        )
    }

    pub fn get_enum(&self, path: &Vec<String>, name: &str) -> Option<ResolvedEnumTypeRef> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(
            || None,
            |found_ns| found_ns.borrow().get_enum(name).cloned(),
        )
    }

    pub fn get_enum_variant_type(
        &self,
        path: &Vec<String>,
        enum_type_name: &str,
        variant_name: &str,
    ) -> Option<ResolvedEnumVariantTypeRef> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(
            || None,
            |found_ns| {
                found_ns
                    .borrow()
                    .get_enum_variant_type_str(enum_type_name, variant_name)
                    .cloned()
            },
        )
    }

    pub fn get_rust_type(&self, path: &Vec<String>, name: &str) -> Option<ResolvedRustTypeRef> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(
            || None,
            |found_ns| found_ns.borrow().get_rust_type(name).cloned(),
        )
    }

    pub fn get_type_alias(&self, path: &Vec<String>, name: &str) -> Option<ResolvedType> {
        let namespace = self.get_namespace(path);
        namespace.map_or_else(
            || None,
            |found_ns| found_ns.borrow().get_type_alias(name).cloned(),
        )
    }

    pub fn add_struct(
        &self,
        struct_type_name: &str,
        struct_type: ResolvedStructType,
    ) -> Result<ResolvedStructTypeRef, ResolveError> {
        //let struct_type_ref = Rc::new(struct_type);
        self.namespace
            .borrow_mut()
            .add_struct(struct_type_name, struct_type)
    }

    pub fn add_enum_type(
        &mut self,
        name: &str,
        enum_type: ResolvedEnumType,
    ) -> Result<ResolvedEnumTypeRef, ResolveError> {
        let enum_type_ref = Rc::new(enum_type);
        self.namespace
            .borrow_mut()
            .add_enum_type(name, enum_type_ref)
    }

    pub fn add_enum_variant(
        &mut self,
        enum_name: &String,
        variant_name: &str,
        variant_type: ResolvedEnumVariantType,
    ) -> Result<ResolvedEnumVariantTypeRef, ResolveError> {
        self.namespace
            .borrow_mut()
            .add_enum_variant(enum_name, variant_name, variant_type)
    }

    pub fn add_internal_function_ref(
        &mut self,
        function_name: &str,
        function: ResolvedInternalFunctionDefinition,
    ) -> Result<ResolvedInternalFunctionDefinitionRef, ResolveError> {
        self.namespace
            .borrow_mut()
            .add_internal_function(function_name, function)
    }
}