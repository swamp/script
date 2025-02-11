/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::path::Path;
use swamp_script_analyzer::prelude::ResolveError;
use swamp_script_analyzer::prelude::ResolvedProgram;
use swamp_script_dep_loader::{
    parse_dependant_modules_and_resolve, DepLoaderError, DependencyParser, ParseModule,
};
use swamp_script_eval_loader::resolve_program;
use swamp_script_source_map::SourceMap;
pub mod prelude;

#[derive(Debug)]
pub enum ScriptResolveError {
    ResolveError(ResolveError),
    DepLoaderError(DepLoaderError),
}

impl From<ResolveError> for ScriptResolveError {
    fn from(err: ResolveError) -> Self {
        Self::ResolveError(err)
    }
}

impl From<DepLoaderError> for ScriptResolveError {
    fn from(err: DepLoaderError) -> Self {
        Self::DepLoaderError(err)
    }
}

pub fn compile_and_resolve(
    root_path: &Path,
    module_path: &[String],
    parse_module: ParseModule,
    source_map: &mut SourceMap,
) -> Result<ResolvedProgram, ScriptResolveError> {
    let mut dependency_parser = DependencyParser::new();
    dependency_parser.add_ast_module(module_path.to_vec(), parse_module);

    let module_paths_in_order = parse_dependant_modules_and_resolve(
        root_path.to_owned(),
        module_path.to_vec(),
        &mut dependency_parser,
        source_map,
    )?;

    let mut resolved_program = ResolvedProgram::new();
    resolve_program(
        &mut resolved_program.state,
        &mut resolved_program.modules,
        source_map,
        &module_paths_in_order,
        &dependency_parser,
    )?;

    Ok(resolved_program)
}
