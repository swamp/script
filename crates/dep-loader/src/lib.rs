/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;
use dirs::home_dir;
use seq_map::SeqMap;
use std::collections::HashSet;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use std::{env, io};
use swamp_script_ast::Function;
use swamp_script_ast::prelude::*;
use swamp_script_parser::{AstParser, SpecificError};
use swamp_script_source_map::{FileId, SourceMap};
use tracing::debug;
pub struct ParseRoot;

#[derive(Debug)]
pub enum ParseRootError {
    IoError(std::io::Error),

    ParserError(ParserError),
}

impl From<std::io::Error> for ParseRootError {
    fn from(err: std::io::Error) -> Self {
        Self::IoError(err)
    }
}

#[derive(Debug)]
pub struct ParsedAstModule {
    pub ast_module: swamp_script_ast::Module,
    pub file_id: FileId,
}

impl ParsedAstModule {
    // TODO: HACK: declare_external_function() should be removed
    pub fn declare_external_function(
        &mut self,
        parameters: Vec<Parameter>,
        return_type: Option<Type>,
    ) {
        let fake_identifier = Node::default();

        let signature = FunctionDeclaration {
            name: fake_identifier.clone(),
            params: parameters,
            self_parameter: None,
            return_type,
        };
        let external_signature = Function::External(signature);

        self.ast_module.definitions.insert(
            0, // add it first
            Definition::FunctionDef(external_signature),
        );
    }
}

#[derive(Debug)]
pub struct RelativePath(pub String);

#[derive(Debug)]
pub struct ParserError {
    pub node: Node,
    pub specific: SpecificError,
    pub file_id: FileId,
}

impl ParseRoot {
    pub fn new() -> Self {
        Self {}
    }

    pub fn parse(
        &self,
        contents: String,
        file_id: FileId,
    ) -> Result<ParsedAstModule, ParseRootError> {
        let parser = AstParser {};

        let ast_program = parser.parse_module(&contents).map_err(|err| {
            let new_err = ParserError {
                node: Node { span: err.span },
                specific: err.specific,
                file_id,
            };
            ParseRootError::ParserError(new_err)
        })?;

        Ok(ParsedAstModule {
            ast_module: ast_program,
            file_id,
        })
    }
}

#[derive(Clone)]
#[allow(unused)]
pub struct ModuleInfo {
    path: Vec<String>,
    imports: Vec<Vec<String>>,
    uses: Vec<Vec<String>>,
    parsed: bool,
    analyzed: bool,
}

pub struct DependencyParser {
    pub import_scanned_modules: SeqMap<Vec<String>, ModuleInfo>,
    already_parsed_modules: SeqMap<Vec<String>, ParsedAstModule>,
    pub already_resolved_modules: HashSet<Vec<String>>,
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
            already_resolved_modules: HashSet::new(),
        }
    }

    pub fn add_resolved_module(&mut self, module_path: Vec<String>) {
        self.already_resolved_modules.insert(module_path);
    }

    pub fn add_ast_module(&mut self, module_path: Vec<String>, parsed_module: ParsedAstModule) {
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
    CircularDependency(Vec<String>),
    ParseRootError(ParseRootError),
    IoError(io::Error),
}

impl From<ParseRootError> for DependencyError {
    fn from(err: ParseRootError) -> Self {
        Self::ParseRootError(err)
    }
}

impl From<io::Error> for DependencyError {
    fn from(value: io::Error) -> Self {
        Self::IoError(value)
    }
}

pub const LOCAL_ROOT_PACKAGE_PATH: &str = "crate";

pub fn get_all_local_paths(
    source_map: &SourceMap,
    parsed_module: &ParsedAstModule,
) -> (Vec<Vec<String>>, Vec<Vec<String>>) {
    let mut imports = vec![];
    let mut uses = vec![];

    for def in parsed_module.ast_module.definitions() {
        match def {
            Definition::Mod(import) => {
                let mut sections = Vec::new();
                sections.push(LOCAL_ROOT_PACKAGE_PATH.to_string());
                for section_node in &import.module_path.0 {
                    let import_path = source_map
                        .get_span_source(
                            parsed_module.file_id,
                            section_node.span.offset as usize,
                            section_node.span.length.into(),
                        )
                        .to_string();
                    sections.push(import_path);
                }

                imports.push(sections);
            }

            Definition::Use(import) => {
                let mut sections = Vec::new();
                for section_node in &import.module_path.0 {
                    let import_path = source_map
                        .get_span_source(
                            parsed_module.file_id,
                            section_node.span.offset as usize,
                            section_node.span.length.into(),
                        )
                        .to_string();
                    sections.push(import_path);
                }
                uses.push(sections);
            }
            _ => continue,
        }
    }

    (imports, uses)
}

pub fn module_path_to_relative_swamp_file(module_path_vec: &[String]) -> PathBuf {
    let mut path_buf = PathBuf::new();

    let orig_len = module_path_vec.len();

    let converted_path = if module_path_vec[0] == "crate" {
        &module_path_vec[1..]
    } else {
        module_path_vec
    };

    path_buf.push(converted_path.join("/"));
    if orig_len == 1 {
        path_buf.push("lib"); // lib is default if the path only contains the package root
    }

    path_buf.set_extension("swamp");

    path_buf
}

pub fn module_path_to_relative_swamp_file_string(module_path_vec: &[String]) -> String {
    module_path_to_relative_swamp_file(module_path_vec)
        .to_str()
        .unwrap()
        .into()
}

pub fn mount_name_from_path(path: &[String]) -> &str {
    if path[0] == "crate" {
        "crate"
    } else {
        "registry"
    }
}

/// Parses just a single module. Any `mod` keywords in this file will be ignored. So this
/// is mainly for internal use.
pub fn parse_single_module(
    source_map: &mut SourceMap,
    module_path: &[String],
) -> Result<ParsedAstModule, DependencyError> {
    let mount_name = mount_name_from_path(&module_path);

    let (file_id, script) = source_map.read_file_relative(
        mount_name,
        &module_path_to_relative_swamp_file_string(module_path),
    )?;

    let parse_module = ParseRoot.parse(script, file_id)?;

    Ok(parse_module)
}

impl DependencyParser {
    pub fn parse_local_modules(
        &mut self,
        module_path: &[String],
        source_map: &mut SourceMap,
    ) -> Result<(), DependencyError> {
        let mut to_parse = vec![module_path.to_vec()];

        while let Some(path) = to_parse.pop() {
            let module_path_vec = &path.clone();
            if self.import_scanned_modules.contains_key(module_path_vec) {
                continue;
            }

            let parsed_module_to_scan =
                if let Some(parsed_module) = self.already_parsed_modules.get(module_path_vec) {
                    parsed_module
                } else if self.already_resolved_modules.contains(module_path_vec) {
                    continue;
                } else {
                    let parsed_ast_module = parse_single_module(source_map, &path)?;

                    self.already_parsed_modules
                        .insert(path.clone(), parsed_ast_module)
                        .expect("TODO: panic message");

                    self.already_parsed_modules
                        .get(&path.clone())
                        .expect("we just inserted it")
                };

            let (imports, uses) = get_all_local_paths(source_map, parsed_module_to_scan);
            let filtered_imports: Vec<Vec<String>> = imports
                .into_iter()
                .filter(|import| !self.already_resolved_modules.contains(import))
                .collect();

            let filtered_uses: Vec<Vec<String>> = uses
                .into_iter()
                .filter(|import| !self.already_resolved_modules.contains(import))
                .collect();

            self.import_scanned_modules
                .insert(
                    path.clone(),
                    ModuleInfo {
                        path: path.clone(),
                        imports: filtered_imports.clone(),
                        uses: filtered_uses.clone(),
                        parsed: false,
                        analyzed: false,
                    },
                )
                .expect("TODO: panic message");

            to_parse.extend(filtered_imports.clone());

            to_parse.extend(filtered_uses.clone());
        }
        Ok(())
    }

    pub fn get_parsed_module(&self, path: &[String]) -> Option<&ParsedAstModule> {
        self.already_parsed_modules.get(&path.to_vec())
    }

    pub fn get_parsed_module_mut(&mut self, path: &[String]) -> Option<&mut ParsedAstModule> {
        self.already_parsed_modules.get_mut(&path.to_vec())
    }

    pub(crate) fn get_analysis_order(&self) -> Result<Vec<Vec<String>>, DependencyError> {
        let mut order = Vec::new();
        let mut visited = HashSet::new();
        let mut temp_visited = HashSet::new();

        fn visit(
            graph: &DependencyParser,
            path: &[String],
            visited: &mut HashSet<Vec<String>>,
            temp_visited: &mut HashSet<Vec<String>>,
            order: &mut Vec<Vec<String>>,
        ) -> Result<(), DependencyError> {
            if temp_visited.contains(path) {
                return Err(DependencyError::CircularDependency(Vec::from(path)));
            }

            if visited.contains(path) {
                return Ok(());
            }

            temp_visited.insert(Vec::from(path));

            if let Some(module) = graph.import_scanned_modules.get(&path.to_vec()) {
                for import in &module.uses {
                    visit(graph, import, visited, temp_visited, order)?;
                }
                for import in &module.imports {
                    visit(graph, import, visited, temp_visited, order)?;
                }
            }

            order.push(Vec::from(path));
            visited.insert(Vec::from(path));

            temp_visited.remove(path);

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

/// # Errors
///
pub fn os_home_relative_path(project_name: &str) -> io::Result<PathBuf> {
    home_dir().map_or_else(
        || {
            Err(io::Error::new(
                io::ErrorKind::Other,
                "Could not determine home directory",
            ))
        },
        |home_path| {
            let mut path = home_path;
            path.push(format!(".{project_name}"));
            Ok(path)
        },
    )
}

pub fn path_from_environment_variable() -> io::Result<PathBuf> {
    if let Ok(string_value) = &env::var("SWAMP_HOME") {
        Ok(Path::new(string_value).to_path_buf())
    } else {
        Err(io::Error::new(ErrorKind::InvalidData, "missing SWAMP_HOME"))
    }
}
pub fn swamp_home() -> io::Result<PathBuf> {
    if let Ok(found_path) = path_from_environment_variable() {
        Ok(found_path)
    } else {
        os_home_relative_path("swamp")
    }
}

/// # Errors
///
pub fn swamp_registry_path() -> io::Result<PathBuf> {
    let mut swamp_home = swamp_home()?;
    swamp_home.push("packages");
    Ok(swamp_home)
}

pub fn parse_local_modules_and_get_order(
    module_path: Vec<String>,
    dependency_parser: &mut DependencyParser,
    source_map: &mut SourceMap,
) -> Result<Vec<Vec<String>>, DepLoaderError> {
    debug!(current_directory=?get_current_dir().expect("failed to get current directory"), "current directory");
    dependency_parser.parse_local_modules(&module_path, source_map)?;

    let module_paths_in_order = dependency_parser.get_analysis_order()?;

    Ok(module_paths_in_order)
}

/*
pub fn create_parsed_modules(
    script: &str,
    source_map: &mut SourceMap,
    root_path: PathBuf,
) -> Result<DependencyParser, ParseError> {
    let parser = AstParser {};

    let file_id = source_map.add_manual_no_id(&*root_path, script);
    let ast_module_result = parser.parse_module(script);
    if let Err(some) = ast_module_result {
        return Err(some);
    }
    let ast_module = ast_module_result.unwrap();
    trace!("ast_module:\n{:?}", ast_module);

    let parse_module = ParseModule {
        ast_module,
        file_id,
    };

    let mut graph = DependencyParser::new();
    let root = vec!["test".to_string()];
    graph.add_ast_module(root, parse_module);

    debug!("root path is {root_path:?}");

    Ok(graph)
}

 */
