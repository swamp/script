/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use regex::Regex;
use std::path::Path;
use std::rc::Rc;
use std::str::FromStr;
use swamp_script_analyzer::prelude::{Error, Program};
use swamp_script_analyzer::Analyzer;
use swamp_script_dep_loader::{
    create_source_map, parse_local_modules_and_get_order, parse_single_module, DependencyParser,
    ParsedAstModule,
};
use swamp_script_error_report::{show_error, show_script_resolve_error, ScriptResolveError};
use swamp_script_eval_loader::analyze_modules_in_order;
use swamp_script_modules::modules::Modules;
use swamp_script_modules::symtbl::SymbolTable;
use swamp_script_semantic::ProgramState;
use swamp_script_source_map::{FileId, SourceMap};
use tiny_ver::TinyVersion;
use tracing::trace;

const COMPILER_VERSION: &str = "0.0.0";

pub fn analyze_ast_module_skip_expression(
    analyzer: &mut Analyzer,
    parsed_ast_module: ParsedAstModule,
) -> Result<(), Error> {
    for definition in &parsed_ast_module.ast_module.definitions {
        analyzer.analyze_definition(definition)?;
    }
    Ok(())
}

pub fn analyze_single_module(
    state: &mut ProgramState,
    default_symbol_table: SymbolTable,
    modules: &Modules,
    parsed_ast_module: ParsedAstModule,
    source_map: &SourceMap,
    versioned_module_path: &[String],
) -> Result<SymbolTable, Error> {
    let mut analyzer = Analyzer::new(
        state,
        modules,
        source_map,
        versioned_module_path,
        parsed_ast_module.file_id,
    );

    analyzer.shared.lookup_table = default_symbol_table;

    analyze_ast_module_skip_expression(&mut analyzer, parsed_ast_module)?;

    Ok(analyzer.shared.definition_table)
}

/// Bootstraps the core and ffi modules and creates a default symbol table
///
/// # Panics
/// In theory it can panic, but should be safe.
pub fn bootstrap_modules(
    packages_root_path: &Path,
) -> Result<(Modules, SymbolTable), ScriptResolveError> {
    let compiler_version = TinyVersion::from_str(COMPILER_VERSION).unwrap();
    trace!(?compiler_version, "booting up compiler");

    let mut modules = Modules::new();

    let mut core_module = swamp_script_core::create_module(&compiler_version);

    let mut default_symbol_table = SymbolTable::new();

    // Prelude for the core module
    // Expose the basic primitive types, like `Int`, `String`, `Float`, `Bool`
    // so they can be references without a `use core::{Int, String, Float, Bool}` statement.
    default_symbol_table
        .extend_basic_from(&core_module.namespace.symbol_table)
        .unwrap();

    let mut source_map = create_source_map(packages_root_path).unwrap();
    let core_ast_module = parse_single_module(&mut source_map, &core_module.namespace.path)?;

    let mut state = ProgramState::new();

    let core_analyzed_symbol_table = analyze_single_module(
        &mut state,
        default_symbol_table.clone(),
        &modules,
        core_ast_module,
        &source_map,
        &core_module.namespace.path,
    )?;

    core_module.namespace.symbol_table = Rc::new(core_analyzed_symbol_table);

    // core module is done, so add it read only to the modules
    modules.add(Rc::new(core_module));

    // Add `core` module without the version number, so they can be referenced from code
    default_symbol_table
        .add_package_version(swamp_script_core::PACKAGE_NAME, compiler_version.clone())
        .expect("should work");

    let ffi_module = swamp_script_ffi::create_module(&compiler_version);
    let ffi_module_ref = Rc::new(ffi_module);
    modules.add(ffi_module_ref.clone());

    // Add `ffi` module without the version number, so they can be referenced from code
    default_symbol_table
        .add_package_version(swamp_script_ffi::PACKAGE_NAME, compiler_version)
        .expect("should work");

    Ok((modules, default_symbol_table))
}

pub fn init(root_path: &Path) -> Result<Program, ScriptResolveError> {
    let mut resolved_program = Program::new();

    Ok(resolved_program)
}

pub fn compile_and_resolve(
    module_path: &[String],
    source_map: &mut SourceMap,
    parse_module: ParsedAstModule,
) -> Result<Program, ScriptResolveError> {
    let mut dependency_parser = DependencyParser::new();
    dependency_parser.add_ast_module(module_path.to_vec(), parse_module);

    let module_paths_in_order = parse_local_modules_and_get_order(
        module_path.to_vec(),
        &mut dependency_parser,
        source_map,
    )?;

    let mut resolved_program = Program::new();
    analyze_modules_in_order(
        &mut resolved_program.state,
        &resolved_program.auto_use_modules,
        &mut resolved_program.modules,
        source_map,
        &module_paths_in_order,
        &dependency_parser,
    )?;

    Ok(resolved_program)
}

pub fn compile_and_analyze_all_modules(
    module_path: &[String],
    resolved_program: &mut Program,
    source_map: &mut SourceMap,
) -> Result<(), ScriptResolveError> {
    let mut dependency_parser = DependencyParser::new();

    let module_paths_in_order = parse_local_modules_and_get_order(
        module_path.to_vec(),
        &mut dependency_parser,
        source_map,
    )?;

    analyze_modules_in_order(
        &mut resolved_program.state,
        &resolved_program.auto_use_modules,
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
    resolved_program: &mut Program,
    source_map: &mut SourceMap,
) -> Result<(), ScriptResolveError> {
    let mangrove_render_result =
        compile_and_analyze(root_module_path, resolved_program, source_map);

    match mangrove_render_result {
        Ok(..) => {}
        Err(err) => {
            show_script_resolve_error(&err, source_map);
            Err(err)?;
        }
    }
    let mangrove_render_module = resolved_program.modules.get(root_module_path).unwrap();

    let first_part = remove_version_from_package_name_regex(&root_module_path[0]);
    let mut without_version_path: Vec<String> = root_module_path.to_vec();
    without_version_path[0] = first_part;

    resolved_program
        .modules
        .link_module(&without_version_path, mangrove_render_module.clone());

    Ok(())
}

/// # Errors
///
pub fn compile_and_analyze(
    root_module_path: &[String],
    resolved_program: &mut Program,
    source_map: &mut SourceMap,
) -> Result<(), ScriptResolveError> {
    compile_and_analyze_all_modules(root_module_path, resolved_program, source_map)
}
