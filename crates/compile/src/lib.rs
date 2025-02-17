/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use regex::Regex;
use std::path::Path;
use swamp_script_analyzer::prelude::{Error, Program};
use swamp_script_dep_loader::{
    create_source_map, parse_local_modules_and_get_order, DependencyParser, ParsedAstModule,
};
use swamp_script_error_report::{show_script_resolve_error, ScriptResolveError};
use swamp_script_eval_loader::analyze_modules_in_order;
use swamp_script_modules::modules::{Module, ModuleRef};
use swamp_script_modules::symtbl::SymbolTable;
use swamp_script_parser::Rule::program;
use swamp_script_semantic::{AliasType, Node, Type};
use swamp_script_source_map::SourceMap;

pub fn bootstrap(root_path: &Path) -> Result<Program, ScriptResolveError> {
    let mut source_map = create_source_map(root_path).unwrap();
    let mut resolved_program = Program::new();

    let swamp_core_module_path = &["core-0.0.0".to_string()];
    let mut dependency_parser = DependencyParser::new();

    let module_paths_in_order = parse_local_modules_and_get_order(
        swamp_core_module_path.to_vec(),
        &mut dependency_parser,
        &mut source_map,
    )?;

    let core_module = resolved_program
        .modules
        .get(swamp_core_module_path)
        .unwrap();

    let mut core_ns = core_module.borrow_mut().namespace.borrow_mut();

    let int_alias = AliasType {
        name: Node::default(),
        assigned_name: "Int".to_string(),
        referenced_type: Type::Int,
    };
    core_ns.add_alias(int_alias).expect("TODO: panic message");

    let float_alias = AliasType {
        name: Node::default(),
        assigned_name: "Float".to_string(),
        referenced_type: Type::Float,
    };
    core_ns.add_alias(float_alias).expect("TODO: panic message");

    let string_alias = AliasType {
        name: Node::default(),
        assigned_name: "String".to_string(),
        referenced_type: Type::String,
    };
    core_ns
        .add_alias(string_alias)
        .expect("TODO: panic message");

    let bool_alias = AliasType {
        name: Node::default(),
        assigned_name: "Bool".to_string(),
        referenced_type: Type::Bool,
    };
    core_ns.add_alias(bool_alias).expect("TODO: panic message");

    // core::

    compile_analyze_and_link_without_version(
        swamp_core_module_path,
        &mut resolved_program,
        &mut source_map,
    )?;

    resolved_program.add_auto_use(swamp_core_module_path);

    // std::
    let mangrove_std_module_path = &["std-0.0.0".to_string()];
    compile_analyze_and_link_without_version(
        mangrove_std_module_path,
        &mut resolved_program,
        &mut source_map,
    )?;

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
        .link_module(&without_version_path, mangrove_render_module);

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
