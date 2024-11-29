/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use pest::error::Error;
use seq_map::SeqMap;
use std::collections::HashSet;
use std::path::PathBuf;
use std::{env, fs};
use swamp_script_ast::prelude::*;
use swamp_script_parser::{AstParser, Rule};
use tracing::{debug, info, trace};

pub struct ParseRoot {
    pub base_path: PathBuf,
}

#[derive(Debug)]
pub enum ParseRootError {
    IoError(std::io::Error),
    ParseRule(Error<Rule>),
}

impl From<std::io::Error> for ParseRootError {
    fn from(err: std::io::Error) -> Self {
        Self::IoError(err)
    }
}

impl From<pest::error::Error<Rule>> for ParseRootError {
    fn from(value: Error<Rule>) -> Self {
        Self::ParseRule(value)
    }
}

#[derive(Debug)]
pub struct ParseModule {
    pub ast_program: swamp_script_ast::Module,
}

impl ParseModule {
    pub fn declare_external_function(
        &mut self,
        name: String,
        parameters: Vec<Parameter>,
        return_type: Type,
    ) {
        self.ast_program.definitions.insert(
            0,
            Definition::ExternalFunctionDef(
                // TODO: Workaround to push external declarations so they come before internal functions
                LocalIdentifier {
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
                    text: name,
                },
                FunctionSignature {
                    params: parameters,
                    return_type,
                },
            ),
        );
    }
}

#[derive(Debug)]
pub struct RelativePath(pub String);

fn to_relative_path(path: &ModulePath) -> RelativePath {
    RelativePath(
        path.0
            .iter()
            .map(|local_type_identifier| local_type_identifier.as_str())
            .collect::<Vec<_>>()
            .join("/"),
    )
}

impl ParseRoot {
    pub fn new(base_path: PathBuf) -> Self {
        Self { base_path }
    }

    fn to_file_system_path(&self, path: RelativePath) -> PathBuf {
        info!("converting from {path:?}");
        let mut path_buf = self.base_path.to_path_buf();

        path_buf.push(path.0);
        path_buf.set_extension("swamp");

        info!("converted to {path_buf:?}");
        path_buf
    }
    pub fn parse(&self, module_path: &ModulePath) -> Result<ParseModule, ParseRootError> {
        let path_buf = self.to_file_system_path(to_relative_path(module_path));
        let contents = fs::read_to_string(path_buf)?;

        let parser = AstParser::new();

        let ast_program = parser.parse_script(&*contents)?;

        Ok(ParseModule { ast_program })
    }
}

#[derive(Clone)]
#[allow(unused)]
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

fn get_current_dir() -> Result<PathBuf, std::io::Error> {
    let path = env::current_dir()?;

    //let cargo_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    Ok(path)
}

#[derive(Debug)]
pub enum DepLoaderError {
    DependencyError(DependencyError),
}

impl From<DependencyError> for DepLoaderError {
    fn from(e: DependencyError) -> Self {
        Self::DependencyError(e)
    }
}

pub fn parse_dependant_modules_and_resolve(
    base_path: PathBuf,
    module_path: ModulePath,
    dependency_parser: &mut DependencyParser,
) -> Result<Vec<ModulePath>, DepLoaderError> {
    debug!(current_directory=?get_current_dir().expect("failed to get current directory"), "current directory");
    let parse_root = ParseRoot::new(base_path);

    dependency_parser.parse_all_dependant_modules(parse_root, module_path)?;

    let module_paths_in_order = dependency_parser.get_analysis_order()?;

    Ok(module_paths_in_order)
}

pub fn create_parsed_modules(
    script: &str,
    root_path: PathBuf,
) -> Result<DependencyParser, pest::error::Error<Rule>> {
    let parser = AstParser::new();
    let ast_program = parser.parse_script(script)?;
    trace!("ast_program:\n{:#?}", ast_program);

    let parse_module = ParseModule { ast_program };

    let mut graph = DependencyParser::new();
    let root = ModulePath(vec!["test".to_string()]);
    graph.add_ast_module(root.clone(), parse_module);

    debug!("root path is {root_path:?}");

    Ok(graph)
}
