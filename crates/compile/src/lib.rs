/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use regex::Regex;
use seq_map::SeqMap;
use std::env::current_dir;
use std::io;
use std::path::Path;
use std::path::PathBuf;
use std::rc::Rc;
use std::str::FromStr;
use swamp_script_analyzer::Analyzer;
use swamp_script_analyzer::prelude::{Error, Program};
use swamp_script_dep_loader::{
    DependencyParser, ParsedAstModule, parse_local_modules_and_get_order, parse_single_module,
    swamp_registry_path,
};
use swamp_script_error_report::{ScriptResolveError, prelude::show_script_resolve_error};
use swamp_script_eval_loader::analyze_modules_in_order;
use swamp_script_modules::modules::{Module, Modules};
use swamp_script_modules::symtbl::{SymbolTable, SymbolTableRef};
use swamp_script_pretty_print::{SourceMapDisplay, SymbolTableDisplay};
use swamp_script_semantic::ProgramState;
use swamp_script_source_map::SourceMap;
use swamp_script_source_map_lookup::SourceMapWrapper;
use tiny_ver::TinyVersion;
use tracing::{debug, info, trace};

const COMPILER_VERSION: &str = "0.0.0";

/// # Errors
///
pub fn analyze_ast_module_skip_expression(
    analyzer: &mut Analyzer,
    parsed_ast_module: &ParsedAstModule,
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
    core_symbol_table: SymbolTableRef,
    parsed_ast_module: &ParsedAstModule,
    source_map: &SourceMap,
    versioned_module_path: &[String],
) -> Result<SymbolTable, Error> {
    let mut analyzer = Analyzer::new(
        state,
        modules,
        core_symbol_table,
        source_map,
        versioned_module_path,
        parsed_ast_module.file_id,
    );

    analyzer.shared.lookup_table = default_symbol_table;

    //    trace!(lookup_table=?analyzer.shared.lookup_table, "analyzer lookup_table");

    analyze_ast_module_skip_expression(&mut analyzer, parsed_ast_module)?;

    Ok(analyzer.shared.definition_table)
}

pub fn create_source_map(registry_path: &Path, local_path: &Path) -> io::Result<SourceMap> {
    trace!(?registry_path, ?local_path, "mounting source map");

    let mut mounts = SeqMap::new();
    mounts
        .insert("crate".to_string(), local_path.to_path_buf())
        .unwrap();

    mounts
        .insert("registry".to_string(), registry_path.to_path_buf())
        .unwrap();

    SourceMap::new(&mounts)
}

pub fn create_registry_source_map(registry_path: &Path) -> io::Result<SourceMap> {
    trace!(?registry_path, "mounting registry path source map");

    let mut mounts = SeqMap::new();
    mounts
        .insert("registry".to_string(), registry_path.to_path_buf())
        .unwrap();

    SourceMap::new(&mounts)
}

#[derive(Debug)]
pub struct BootstrapResult {
    pub program: Program,
    pub core_module_path: Vec<String>,
}

/// Bootstraps the core and ffi modules and creates a default symbol table
///
/// # Errors
///
/// # Panics
/// In theory it can panic, but should be safe.
pub fn bootstrap_modules(
    mut source_map: &mut SourceMap,
) -> Result<BootstrapResult, ScriptResolveError> {
    let compiler_version = TinyVersion::from_str(COMPILER_VERSION).unwrap();
    trace!(%compiler_version, "booting up compiler");

    let mut modules = Modules::new();

    let mut core_module_with_intrinsics = swamp_script_core::create_module(&compiler_version);

    let core_ast_module = parse_single_module(
        source_map,
        &core_module_with_intrinsics.symbol_table.module_path(),
    )?;

    let mut state = ProgramState::new();

    let half_completed_core_symbol_table = core_module_with_intrinsics.symbol_table.clone();
    let default_symbol_table_for_core_with_intrinsics = half_completed_core_symbol_table.clone();

    let mut core_analyzed_definition_table = analyze_single_module(
        &mut state,
        default_symbol_table_for_core_with_intrinsics.clone(),
        &modules,
        half_completed_core_symbol_table.clone().into(),
        &core_ast_module,
        source_map,
        &core_module_with_intrinsics.symbol_table.module_path(),
    )?;
    // Overwrite the default lookup table to the definition table
    core_analyzed_definition_table
        .extend_intrinsic_functions_from(&default_symbol_table_for_core_with_intrinsics);
    core_analyzed_definition_table
        .extend_basic_from(&default_symbol_table_for_core_with_intrinsics);

    let source_map_lookup = SourceMapWrapper { source_map };
    let pretty_printer = SourceMapDisplay {
        source_map: &source_map_lookup,
    };

    let display_core_analyzed_definition_table = SymbolTableDisplay {
        symbol_table: &core_analyzed_definition_table,
        source_map_display: &pretty_printer,
    };

    info!(%display_core_analyzed_definition_table, "core analyzed symbol table");

    core_module_with_intrinsics.symbol_table = core_analyzed_definition_table;

    // core module is done, so add it read only to the modules
    let core_module_ref = Rc::new(core_module_with_intrinsics);
    modules.add(core_module_ref.clone());

    let mut default_symbol_table_for_others = SymbolTable::new(&[]);

    // Prelude for the core module
    // Expose the basic primitive types, like `Int`, `String`, `Float`, `Bool`
    // so they can be references without a `use core::{Int, String, Float, Bool}` statement.
    default_symbol_table_for_others
        .extend_alias_from(&core_module_ref.symbol_table)
        .unwrap();

    // Add `core` module without the version number, so they can be referenced from code
    default_symbol_table_for_others
        .add_package_version(swamp_script_core::PACKAGE_NAME, compiler_version)
        .expect("should work");

    let source_map_lookup = SourceMapWrapper { source_map };
    let pretty_printer = SourceMapDisplay {
        source_map: &source_map_lookup,
    };

    let symbol_table_display = SymbolTableDisplay {
        symbol_table: &default_symbol_table_for_others,
        source_map_display: &pretty_printer,
    };

    info!(%symbol_table_display, "default_symbol_table");

    let program = Program::new(state, modules, default_symbol_table_for_others);

    let result = BootstrapResult {
        program,
        core_module_path: core_module_ref.symbol_table.module_path().clone(),
    };
    Ok(result)
}

pub fn compile_and_analyze_all_modules(
    module_path: &[String],
    resolved_program: &mut Program,
    source_map: &mut SourceMap,
    core_symbol_table: SymbolTableRef,
) -> Result<(), ScriptResolveError> {
    let mut dependency_parser = DependencyParser::new();

    let module_paths_in_order = parse_local_modules_and_get_order(
        module_path.to_vec(),
        &mut dependency_parser,
        source_map,
    )?;

    analyze_modules_in_order(
        &mut resolved_program.state,
        &resolved_program.default_symbol_table,
        &mut resolved_program.modules,
        core_symbol_table,
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
    core_symbol_table: SymbolTableRef,
) -> Result<(), ScriptResolveError> {
    let mangrove_render_result = compile_and_analyze(
        root_module_path,
        resolved_program,
        source_map,
        core_symbol_table,
    );

    match mangrove_render_result {
        Ok(..) => {}
        Err(err) => {
            show_script_resolve_error(&err, source_map, &Path::new(""));
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
    core_symbol_table: SymbolTableRef,
) -> Result<(), ScriptResolveError> {
    compile_and_analyze_all_modules(
        root_module_path,
        resolved_program,
        source_map,
        core_symbol_table,
    )
}

pub fn current_path() -> PathBuf {
    current_dir().unwrap()
}

/// # Errors
///
/// # Panics
///
pub fn bootstrap_and_compile(
    source_map: &mut SourceMap,
    //root_dependencies: SeqMap<String, TinyVersion>,
    root_path: &[String],
) -> Result<Program, ScriptResolveError> {
    let bootstrap_result = bootstrap_modules(source_map).inspect_err(|err| {
        show_script_resolve_error(err, source_map, &current_path());
    })?;

    let mut program = bootstrap_result.program;

    let core_symbol_table = program
        .modules
        .get(&bootstrap_result.core_module_path)
        .unwrap()
        .symbol_table
        .clone();

    /*
    for (package_name, tiny_version) in root_dependencies {
        program
            .default_symbol_table
            .add_package_version(&package_name, tiny_version)
            .unwrap()
    }

     */

    compile_and_analyze_all_modules(
        root_path,
        &mut program,
        source_map,
        core_symbol_table.into(),
    )
    .inspect_err(|err| {
        show_script_resolve_error(err, source_map, &current_path());
    })?;

    // debug_all_modules(&program.modules, source_map);

    Ok(program)
}

pub fn debug_all_modules(modules: &Modules, source_map: &SourceMap) {
    for (_name, module) in modules.modules() {
        debug_module(&module.symbol_table, source_map)
    }
}
pub fn debug_module(symbol_table: &SymbolTable, source_map: &SourceMap) {
    let source_map_lookup = SourceMapWrapper { source_map };
    let pretty_printer = SourceMapDisplay {
        source_map: &source_map_lookup,
    };

    let symbol_table_display = SymbolTableDisplay {
        symbol_table: &symbol_table,
        source_map_display: &pretty_printer,
    };

    info!(%symbol_table_display, "{:?}", symbol_table.module_path());
}
