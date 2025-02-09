/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_script_analyzer::prelude::ResolveError;
use swamp_script_analyzer::prelude::ResolvedProgram;
use swamp_script_dep_loader::{
    parse_dependant_modules_and_resolve, DepLoaderError, DependencyError, DependencyParser,
    ParseModule,
};
use swamp_script_eval_loader::resolve_program;
use swamp_script_source_map::SourceMap;

#[derive(Debug)]
pub enum ScriptResolveError {
    ResolveError(ResolveError),
    DepLoaderError(DepLoaderError),
    DependencyError(DependencyError),
}

impl From<DependencyError> for ScriptResolveError {
    fn from(err: DependencyError) -> Self {
        Self::DependencyError(err)
    }
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
    module_path: &[String],
    source_map: &mut SourceMap,
    parse_module: ParseModule,
) -> Result<ResolvedProgram, ScriptResolveError> {
    let mut dependency_parser = DependencyParser::new();
    dependency_parser.add_ast_module(module_path.to_vec(), parse_module);

    let module_paths_in_order = parse_dependant_modules_and_resolve(
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

pub fn compile_and_resolve_to_program(
    module_path: &[String],
    resolved_program: &mut ResolvedProgram,
    source_map: &mut SourceMap,
) -> Result<(), ScriptResolveError> {
    let mut dependency_parser = DependencyParser::new();

    let module_paths_in_order = parse_dependant_modules_and_resolve(
        module_path.to_vec(),
        &mut dependency_parser,
        source_map,
    )?;

    resolve_program(
        &mut resolved_program.state,
        &mut resolved_program.modules,
        source_map,
        &module_paths_in_order,
        &dependency_parser,
    )?;

    Ok(())
}

/// # Errors
///
pub fn compile_and_analyze(
    root_module_path: &[String],
    resolved_program: &mut ResolvedProgram,
    source_map: &mut SourceMap,
) -> Result<(), ScriptResolveError> {
    compile_and_resolve_to_program(root_module_path, resolved_program, source_map)
}
