use crate::ns::{ResolvedModuleNamespace, ResolvedModuleNamespaceRef};
use crate::{ResolvedDefinition, ResolvedExpression};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

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
    pub expression: Option<ResolvedExpression>,
    pub namespace: ResolvedModuleNamespaceRef,
}

impl Debug for ResolvedModule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for resolved_def in &self.definitions {
            writeln!(f, "{resolved_def:?}")?;
        }

        if !self.definitions.is_empty() && !self.expression.is_some() {
            writeln!(f, "---\n")?;
        }

        if let Some(resolved_statement) = &self.expression {
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
            expression: None,
        }
    }
}

impl ResolvedModules {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }
    pub fn add(&mut self, module: ResolvedModuleRef) {
        self.modules.insert(
            module.clone().borrow().namespace.borrow().path.clone(),
            module,
        );
    }

    pub fn add_empty_module(&mut self, module_path: &[String]) -> ResolvedModuleRef {
        let ns_ref = Rc::new(RefCell::new(ResolvedModuleNamespace::new(module_path)));
        let module = ResolvedModule {
            definitions: vec![],
            expression: None,
            namespace: ns_ref,
        };
        let module_ref = Rc::new(RefCell::new(module));

        self.modules
            .insert(Vec::from(module_path), module_ref.clone());

        module_ref
    }

    #[must_use]
    pub fn contains_key(&self, module_path: &[String]) -> bool {
        self.modules.contains_key(module_path)
    }

    pub fn get(&self, module_path: &[String]) -> Option<ResolvedModuleRef> {
        self.modules.get(module_path).clone().cloned()
    }
}
