/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use seq_map::SeqMap;
use std::path::Path;
use swamp_script_analyzer::Program;
use swamp_script_compile::bootstrap_and_compile;
use swamp_script_error_report::ScriptResolveError;
use swamp_script_pretty_print::{SourceMapDisplay, SymbolTableDisplay};
use swamp_script_source_map::SourceMap;
use swamp_script_source_map_lookup::SourceMapWrapper;
use tracing::{info, warn};

fn internal_compile(script: &str) -> Result<Program, ScriptResolveError> {
    let mut source_map = SourceMap::new(&SeqMap::default()).unwrap();
    let file_id = 0xffff;

    source_map.add_mount("crate", Path::new("/tmp/")).unwrap();
    source_map.add_to_cache("crate", Path::new("test.swamp"), script, file_id);

    let resolved_path_str = vec!["crate".to_string(), "test".to_string()];

    let program = bootstrap_and_compile(&mut source_map, &resolved_path_str)?;

    let source_map_lookup = SourceMapWrapper {
        source_map: &source_map,
    };
    let pretty_printer = SourceMapDisplay {
        source_map: &source_map_lookup,
    };

    let test_module = program.modules.get(&resolved_path_str).unwrap();

    let symbol_table_display = SymbolTableDisplay {
        symbol_table: &test_module.symbol_table,
        source_map_display: &pretty_printer,
    };

    info!(%symbol_table_display, "symbol table");

    Ok(program)
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
    let resolved_module = internal_compile(script).expect("should work to analyze");

    let formatted_output = format!("{resolved_module:?}");

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
