/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use seq_map::SeqMap;
use std::fmt::{Debug, Formatter};
use std::path::Path;
use swamp_script_analyzer::Analyzer;
use swamp_script_analyzer::prelude::Error;
use swamp_script_compile::bootstrap_modules;
use swamp_script_dep_loader::swamp_registry_path;
use swamp_script_error_report::{ScriptResolveError, show_error, show_script_resolve_error};
use swamp_script_modules::modules::Modules;
use swamp_script_modules::symtbl::SymbolTable;
use swamp_script_parser::AstParser;
use swamp_script_pretty_print::{ExpressionDisplay, SourceMapDisplay, SymbolTableDisplay};
use swamp_script_semantic::{Expression, MonomorphizationCache, ProgramState};
use swamp_script_source_map::SourceMap;
use swamp_script_source_map_lookup::SourceMapWrapper;
use test_log::tracing_subscriber::registry;
use tracing::warn;

fn internal_compile(
    script: &str,
) -> Result<
    (
        SymbolTable,
        MonomorphizationCache,
        Option<Expression>,
        SourceMap,
    ),
    ScriptResolveError,
> {
    let parser = AstParser;

    let program = parser.parse_module(script).expect("Failed to parse script");

    let mut mount_maps = SeqMap::new();

    mount_maps
        .insert("crate".to_string(), Path::new(".").to_path_buf())
        .unwrap();

    let mut source_map = SourceMap::new(&mount_maps).unwrap();
    let file_id = 0xffff;

    let canonical_path = ["some_path".to_string(), "main".to_string()];
    source_map.add_manual(file_id, "crate", Path::new("some_path/main"), script);

    let registry_path = swamp_registry_path().unwrap();
    source_map.add_mount("registry", &registry_path).unwrap();

    let mut bootstrap_result = bootstrap_modules(&mut source_map).inspect_err(|err| {
        show_script_resolve_error(err, &source_map, Path::new(""));
    })?;

    let mut analyzer = Analyzer::new(
        &mut bootstrap_result.state,
        &bootstrap_result.modules,
        &source_map,
        &canonical_path,
        file_id,
    );
    let current_dir = Path::new("");

    analyzer.shared.lookup_table = bootstrap_result.default_symbol_table;

    for definition in &program.definitions {
        let result = analyzer.analyze_definition(definition);
        match result {
            Ok(_analyzed_definition) => {}
            Err(err) => {
                show_error(&err, &source_map, current_dir);
                Err(err)?;
            }
        }
    }

    let expression = &program.expression;
    let maybe_resolved_expression = match expression {
        Some(unwrapped_expression) => {
            let result = analyzer.analyze_expression(unwrapped_expression, None);
            if let Ok(expression) = result {
                Some(expression)
            } else {
                let err = result.err().unwrap();
                show_error(&err, &source_map, current_dir);
                return Err(err)?;
            }
        }
        None => None,
    };

    Ok((
        analyzer.shared.definition_table,
        analyzer.shared.state.monomorphization_cache.clone(),
        maybe_resolved_expression,
        source_map,
    ))
}

/// # Panics
/// Intentionally panics if error message is not equal to the error encountered.
pub fn check_fail(script: &str, expected_error_message: &str) {
    let error = internal_compile(script).expect_err("should have failed");

    assert_eq!(format!("{error:?}"), expected_error_message.trim());
}

/// # Panics
/// Intentionally panics if output is not the same as the `expected_output`
pub fn check(script: &str, expected_output: &str) {
    let (symbol_table, monomorphization_cache, expression, source_map) =
        internal_compile(script).expect("should work");

    let mut formatted_output = String::new();

    let source_map_display = SourceMapDisplay {
        source_map: &SourceMapWrapper { source_map },
    };

    if !symbol_table.is_empty() {
        let mut format_symbol_table = SymbolTableDisplay {
            symbol_table: &symbol_table,
            source_map_display: &source_map_display,
        };
        SourceMapDisplay::<'_>::set_color(false);
        formatted_output += &*format_symbol_table.to_string();
    }

    if !monomorphization_cache.is_empty() {
        if !formatted_output.is_empty() {
            formatted_output += "---\n";
        }
        for (name, ty) in monomorphization_cache.cache {
            formatted_output += &*format!("{name} ==> {ty:?}\n");
        }
    }

    if let Some(expr) = expression {
        if !formatted_output.is_empty() {
            formatted_output += "---\n";
        }

        let format_expr = ExpressionDisplay {
            expression: &expr,
            source_map_display: &source_map_display,
        };
        formatted_output += &*format_expr.to_string();
        formatted_output += "\n";
    }

    let actual = formatted_output
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n");

    let expected = expected_output
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n");

    assert_eq!(
        actual, expected,
        "\nExpected output:\n\n{expected}\n\nActual output:\n\n{actual}\n"
    );

    let mut differences = Vec::new();

    for (i, (exp, act)) in expected.lines().zip(actual.lines()).enumerate() {
        if exp != act {
            differences.push((i + 1, exp, act));
        }
    }

    if !differences.is_empty() {
        for (line_num, exp, act) in &differences {
            warn!(
                "\nDifference at line {}: expected \n{}\n, got \n{}",
                line_num, exp, act
            );
        }
        panic!("Differences found between expected and actual output");
    }
}
