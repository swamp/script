/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use seq_map::SeqMap;
use std::path::Path;
use swamp_script_analyzer::{Analyzer, TypeContext};
use swamp_script_compile::bootstrap_modules;
use swamp_script_dep_loader::swamp_registry_path;
use swamp_script_error_report::{show_script_resolve_error, ScriptResolveError};
use swamp_script_modules::prelude::{Module, Modules, Namespace};
use swamp_script_parser::AstParser;
use swamp_script_source_map::SourceMap;
use tracing::{error, info, warn};

fn internal_compile(script: &str) -> Result<Module, ScriptResolveError> {
    let parser = AstParser {};

    let program = parser.parse_module(script).expect("Failed to parse script");

    let mut source_map = SourceMap::new(&SeqMap::default()).unwrap();
    let file_id = 0xffff;

    source_map.add_manual(file_id, "crate", Path::new("some_path/main"), script);
    let resolved_path_str = vec!["test".to_string()];

    let registry_path = swamp_registry_path().unwrap();
    source_map.add_mount("registry", &registry_path).unwrap();

    let mut bootstrap_result = bootstrap_modules(&mut source_map).inspect_err(|err| {
        show_script_resolve_error(err, &source_map, Path::new(""));
    })?;
    let core_symbol_table = bootstrap_result.core_symbol_table;

    let mut analyzer = Analyzer::new(
        &mut bootstrap_result.state,
        &bootstrap_result.modules,
        core_symbol_table.into(),
        &source_map,
        file_id,
    );

    info!(def_tbl=?bootstrap_result.default_symbol_table, "default");
    analyzer.shared.lookup_table = bootstrap_result.default_symbol_table;

    for definition in &program.definitions {
        analyzer.analyze_definition(definition)?;
    }

    let expression = &program.expression;
    let any_context = TypeContext::new_anything_argument();
    let maybe_resolved_expression = match expression {
        Some(unwrapped_expression) => {
            let result = analyzer.analyze_expression(unwrapped_expression, &any_context);
            if let Ok(expression) = result {
                Some(expression)
            } else {
                let err = result.err().unwrap();
                error!(?err, "found error");
                return Err(err)?;
            }
        }
        None => None,
    };

    let ns_ref = Namespace::new(resolved_path_str, analyzer.shared.definition_table);

    let resolved_module = Module {
        expression: maybe_resolved_expression,
        namespace: ns_ref,
    };

    Ok(resolved_module)
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
