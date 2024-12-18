use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub mod ns;

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
}

#[derive(Debug)]
pub struct ResolvedProgram {
    pub types: ResolvedProgramTypes,
    pub state: ResolvedProgramState,
    pub modules: ResolvedModules,
}

impl ResolvedProgram {
    pub fn new() -> Self {
        Self {
            types: ResolvedProgramTypes::new(),
            state: ResolvedProgramState::new(),
            modules: ResolvedModules::new(),
        }
    }
}
