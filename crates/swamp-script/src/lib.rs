/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::path::Path;
use swamp_script_analyzer::ResolveError;
use swamp_script_ast::ModulePath;
use swamp_script_dep_loader::{
    parse_dependant_modules_and_resolve, DepLoaderError, DependencyParser, ParseModule,
};
use swamp_script_eval_loader::resolve_program;
use swamp_script_semantic::ResolvedProgram;

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
    module_path: &ModulePath,
    parse_module: ParseModule,
) -> Result<ResolvedProgram, ScriptResolveError> {
    let mut dependency_parser = DependencyParser::new();
    dependency_parser.add_ast_module(module_path.clone(), parse_module);

    let module_paths_in_order = parse_dependant_modules_and_resolve(
        root_path.to_owned(),
        module_path.clone(),
        &mut dependency_parser,
    )?;

    let mut resolved_program = ResolvedProgram::new();
    resolve_program(
        &resolved_program.types,
        &mut resolved_program.state,
        &mut resolved_program.modules,
        &module_paths_in_order,
        &dependency_parser,
    )?;

    Ok(resolved_program)
}
