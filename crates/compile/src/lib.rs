/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use regex::Regex;
use swamp_script_analyzer::prelude::ResolvedProgram;
use swamp_script_dep_loader::{parse_local_modules_and_get_order, DependencyParser, ParseModule};
use swamp_script_error_report::{show_script_resolve_error, ScriptResolveError};
use swamp_script_eval_loader::resolve_program;
use swamp_script_source_map::SourceMap;

pub fn compile_and_resolve(
    module_path: &[String],
    source_map: &mut SourceMap,
    parse_module: ParseModule,
) -> Result<ResolvedProgram, ScriptResolveError> {
    let mut dependency_parser = DependencyParser::new();
    dependency_parser.add_ast_module(module_path.to_vec(), parse_module);

    let module_paths_in_order = parse_local_modules_and_get_order(
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

    let module_paths_in_order = parse_local_modules_and_get_order(
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

pub fn remove_version_from_package_name_regex(package_name_with_version: &str) -> String {
    let re = Regex::new(
        r"-(?P<version>[0-9]+(?:\.[0-9]+)*(?:-[0-9A-Za-z-]+(?:\.[0-9A-Za-z-]+)*)?)?(?:\+.*)?$",
    )
    .unwrap();
    re.replace(package_name_with_version, "").to_string()
}

/// # Errors
///
/// # Panics
///
pub fn compile_analyze_and_link_without_version(
    root_module_path: &[String],
    resolved_program: &mut ResolvedProgram,
    source_map: &mut SourceMap,
) -> Result<(), ScriptResolveError> {
    let mangrove_render_result =
        compile_and_analyze(root_module_path, resolved_program, source_map);

    match mangrove_render_result {
        Ok(..) => {}
        Err(err) => {
            show_script_resolve_error(&err, &source_map);
            Err(err)?;
        }
    }
    let mangrove_render_module = resolved_program.modules.get(root_module_path).unwrap();

    let first_part = remove_version_from_package_name_regex(&root_module_path[0]);
    let mut without_version_path: Vec<String> = root_module_path.to_vec();
    without_version_path[0] = first_part;

    resolved_program
        .modules
        .link_module(&without_version_path, mangrove_render_module);
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
