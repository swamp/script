/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;
use swamp_script_analyzer::Analyzer;
use swamp_script_analyzer::prelude::Error;
use swamp_script_error_report::show_analyzer_error;
use swamp_script_parser::AstParser;
use swamp_script_semantic::ProgramState;
use swamp_script_semantic::modules::{Module, Modules};
use swamp_script_semantic::ns::Namespace;
use swamp_script_source_map::SourceMap;
use tracing::warn;

fn internal_compile(script: &str) -> Result<Module, Error> {
    /*
    let parser = AstParser {};

    let program = parser.parse_module(script).expect("Failed to parse script");

    let mut state = ProgramState::new();
    let mut modules = Modules::new();

    let mut source_map = SourceMap::new(Path::new("tests/fixtures/"));
    let file_id = 0xffff;

    source_map.add_manual(file_id, Path::new("some_path/main"), script);
    let resolved_path_str = vec!["test".to_string()];
    let _own_module = modules.add_empty_module(&resolved_path_str);

    let mut name_lookup = NameLookup::new(resolved_path_str, &mut modules);

    let mut resolver = Analyzer::new(&mut state, &mut name_lookup, &source_map, file_id);

    let mut resolved_definitions = Vec::new();
    for definition in &program.definitions {
        let resolved_definition = resolver.analyze_definition(definition)?;
        resolved_definitions.push(resolved_definition);
    }

    let expression = &program.expression;
    let maybe_resolved_expression = match expression {
        Some(unwrapped_expression) => {
            let result = resolver.analyze_expression(unwrapped_expression, None);
            if let Ok(expression) = result {
                Some(expression)
            } else {
                let err = result.err().unwrap();
                show_error(&err, &source_map);
                return Err(err)?;
            }
        }
        None => None,
    };

    let ns_ref = Rc::new(RefCell::new(ModuleNamespace::new(&[])));

    let resolved_module = Module {
        definitions: resolved_definitions,
        expression: maybe_resolved_expression,
        namespace: ns_ref,
    };

    Ok(resolved_module)

     */
    Ok(Module {
        namespace: Namespace {
            symbol_table: Rc::new(Default::default()),
            path: vec![],
        },
        expression: None,
    })
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
