/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use crate::ns::{ResolvedModuleNamespace, ResolvedModuleNamespaceRef};
use crate::{
    ConstantId, ResolvedConstant, ResolvedConstantRef, ResolvedDefinition, ResolvedExpression,
    SemanticError,
};
use seq_map::SeqMap;
use seq_set::SeqSet;
use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

#[derive(Debug)]
pub struct ResolvedModules {
    pub modules: HashMap<Vec<String>, ResolvedModuleRef>,
    pub constants: Vec<ResolvedConstantRef>,
    pub constants_in_eval_order: Vec<ResolvedConstantRef>,
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

        if !self.definitions.is_empty() && self.expression.is_some() {
            writeln!(f, "---\n")?;
        }

        if let Some(resolved_expression) = &self.expression {
            match resolved_expression {
                ResolvedExpression::Block(expressions) => {
                    for expression in expressions {
                        writeln!(f, "{expression:?}")?;
                    }
                }
                _ => writeln!(f, "{resolved_expression:?}")?,
            }
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
            constants: Vec::new(),
            constants_in_eval_order: Vec::new(),
        }
    }
    pub fn add(&mut self, module: ResolvedModuleRef) {
        self.modules.insert(
            module.clone().borrow().namespace.borrow().path.clone(),
            module,
        );
    }

    pub fn add_constant(&mut self, resolved_constant: ResolvedConstant) -> ResolvedConstantRef {
        let id = self.constants.len();
        let mut copy = resolved_constant;
        copy.id = id as ConstantId;
        let constant_ref = Rc::new(copy);
        self.constants.push(constant_ref.clone());

        constant_ref
    }

    pub fn finalize(&mut self) -> Result<(), SemanticError> {
        self.constants_in_eval_order = self.eval_ordered_constants()?;

        Ok(())
    }

    fn eval_ordered_constants(&self) -> Result<Vec<ResolvedConstantRef>, SemanticError> {
        Self::topological_sort_constants(&self.constants)
    }

    pub fn topological_sort_constants(
        constants: &[ResolvedConstantRef],
    ) -> Result<Vec<ResolvedConstantRef>, SemanticError> {
        let mut id_to_constant: SeqMap<ConstantId, ResolvedConstantRef> = SeqMap::new();
        for const_ref in constants {
            id_to_constant
                .insert(const_ref.id, Rc::clone(const_ref))
                .map_err(|_| SemanticError::DuplicateConstantId(const_ref.id))?;
        }

        let mut adjacency: SeqMap<ConstantId, SeqSet<ConstantId>> = SeqMap::new();
        let mut number_of_dependencies: SeqMap<ConstantId, usize> = SeqMap::new();

        for const_ref in constants {
            number_of_dependencies
                .insert(const_ref.id, 0)
                .map_err(|_| SemanticError::DuplicateConstantId(const_ref.id))?;

            let mut deps = SeqSet::new();
            const_ref.expr.collect_constant_dependencies(&mut deps);

            for dep_id in &deps {
                assert!(id_to_constant.contains_key(dep_id));

                if let Some(dependents) = adjacency.get_mut(dep_id) {
                    dependents.insert(const_ref.id);
                } else {
                    let mut dependents = SeqSet::new();
                    dependents.insert(const_ref.id);
                    adjacency
                        .insert(*dep_id, dependents)
                        .map_err(|_| SemanticError::DuplicateConstantId(const_ref.id))?;
                }

                let dependency_count = number_of_dependencies
                    .get_mut(&const_ref.id)
                    .expect("dependency count should have been inserted earlier");
                *dependency_count += 1;
            }
        }

        let mut queue: VecDeque<ConstantId> = number_of_dependencies
            .iter()
            .filter_map(|(id, &dependency_count)| {
                if dependency_count == 0 {
                    Some(*id)
                } else {
                    None
                }
            })
            .collect();

        let mut sorted_constants: Vec<ResolvedConstantRef> = Vec::new();

        while let Some(current_id) = queue.pop_front() {
            let current_const = id_to_constant
                .get(&current_id)
                .expect("should have a id to constant lookup");

            sorted_constants.push(Rc::clone(current_const));

            if let Some(dependents) = adjacency.get(&current_id) {
                for dependent_id in dependents {
                    let dependency_count = number_of_dependencies
                        .get_mut(dependent_id)
                        .expect("should have number of dependencies");
                    assert!(*dependency_count > 0);
                    *dependency_count -= 1;
                    if *dependency_count == 0 {
                        queue.push_back(*dependent_id);
                    }
                }
            }
        }

        if sorted_constants.len() != constants.len() {
            let unsorted_ids: Vec<ConstantId> = constants
                .iter()
                .map(|c| c.id)
                .filter(|id| !sorted_constants.iter().any(|c| c.id == *id))
                .collect();

            return Err(SemanticError::CircularConstantDependency(unsorted_ids));
        }

        Ok(sorted_constants)
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

    #[must_use]
    pub fn get(&self, module_path: &[String]) -> Option<ResolvedModuleRef> {
        self.modules.get(module_path).cloned()
    }
}
