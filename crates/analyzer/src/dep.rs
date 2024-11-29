use crate::{ParseModule, ParseRoot, ParseRootError};
use seq_map::SeqMap;
use std::collections::HashSet;
use swamp_script_ast::{Definition, ModulePath};
use tracing::{debug, info};

#[derive(Eq, PartialEq, Hash, Clone)]
pub struct ModuleInfo {
    path: ModulePath,
    imports: Vec<ModulePath>,
    parsed: bool,
    analyzed: bool,
}

pub struct DependencyParser {
    pub import_scanned_modules: SeqMap<ModulePath, ModuleInfo>,
    already_parsed_modules: SeqMap<ModulePath, ParseModule>,
}

impl Default for DependencyParser {
    fn default() -> Self {
        Self::new()
    }
}

impl DependencyParser {
    pub fn new() -> Self {
        Self {
            import_scanned_modules: SeqMap::new(),
            already_parsed_modules: SeqMap::new(),
        }
    }

    pub fn add_ast_module(&mut self, module_path: ModulePath, parsed_module: ParseModule) {
        debug!(
            "Adding ast module parsed outside of graph resolver {:?}",
            module_path
        );
        self.already_parsed_modules
            .insert(module_path, parsed_module)
            .expect("can not add parsed module")
    }
}

#[derive(Debug)]
pub enum DependencyError {
    CircularDependency(ModulePath),
    ParseRootError(ParseRootError),
}

impl From<ParseRootError> for DependencyError {
    fn from(err: ParseRootError) -> Self {
        Self::ParseRootError(err)
    }
}

fn get_all_import_paths(parsed_module: &ParseModule) -> Vec<ModulePath> {
    let mut imports = vec![];

    for def in parsed_module.ast_program.definitions() {
        match def {
            Definition::Import(import) => imports.push(import.module_path.clone()),
            _ => continue,
        }
    }

    imports
}

impl DependencyParser {
    pub fn parse_all_dependant_modules(
        &mut self,
        parse_root: ParseRoot,
        module_path: ModulePath,
    ) -> Result<(), DependencyError> {
        let mut to_parse = vec![module_path];

        while let Some(path) = to_parse.pop() {
            if self.import_scanned_modules.contains_key(&path) {
                continue;
            }

            let parsed_module_to_scan =
                if let Some(parsed_module) = self.already_parsed_modules.get(&path) {
                    parsed_module
                } else {
                    info!("a module we haven't seen before: {path:?}");
                    let parse_module = parse_root.parse(&path)?;
                    info!("module parsed: {parse_module:?}");

                    self.already_parsed_modules
                        .insert(path.clone(), parse_module)
                        .expect("TODO: panic message");

                    self.already_parsed_modules
                        .get(&path.clone())
                        .expect("we just inserted it")
                };

            let imports = get_all_import_paths(parsed_module_to_scan);
            for import in &imports {
                info!("..found import: {import:?}");
            }

            self.import_scanned_modules
                .insert(
                    path.clone(),
                    ModuleInfo {
                        path,
                        imports: imports.clone(),
                        parsed: false,
                        analyzed: false,
                    },
                )
                .expect("TODO: panic message");

            to_parse.extend(imports);
        }
        Ok(())
    }

    pub fn get_parsed_module(&self, path: &ModulePath) -> Option<&ParseModule> {
        self.already_parsed_modules.get(path)
    }

    pub fn get_parsed_module_mut(&mut self, path: &ModulePath) -> Option<&mut ParseModule> {
        self.already_parsed_modules.get_mut(path)
    }

    pub(crate) fn get_analysis_order(&self) -> Result<Vec<ModulePath>, DependencyError> {
        let mut order = Vec::new();
        let mut visited = HashSet::new();
        let mut temp_visited = HashSet::new();

        fn visit(
            graph: &DependencyParser,
            path: &ModulePath,
            visited: &mut HashSet<ModulePath>,
            temp_visited: &mut HashSet<ModulePath>,
            order: &mut Vec<ModulePath>,
        ) -> Result<(), DependencyError> {
            if temp_visited.contains(path) {
                return Err(DependencyError::CircularDependency(path.clone()));
            }

            if visited.contains(path) {
                return Ok(());
            }

            temp_visited.insert(path.clone());

            if let Some(module) = graph.import_scanned_modules.get(path) {
                for import in &module.imports {
                    visit(graph, import, visited, temp_visited, order)?;
                }
            }

            temp_visited.remove(path);
            visited.insert(path.clone());
            order.push(path.clone());

            Ok(())
        }

        for path in self.import_scanned_modules.keys() {
            if !visited.contains(path) {
                visit(self, path, &mut visited, &mut temp_visited, &mut order)?;
            }
        }

        Ok(order)
    }
}
