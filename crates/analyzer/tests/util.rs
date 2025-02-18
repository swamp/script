/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use seq_map::SeqMap;
use std::fmt::{Debug, Formatter};
use std::path::Path;
use swamp_script_analyzer::prelude::Error;
use swamp_script_analyzer::Analyzer;
use swamp_script_error_report::show_error;
use swamp_script_modules::modules::{pretty_print, Module, Modules};
use swamp_script_modules::symtbl::SymbolTable;
use swamp_script_parser::AstParser;
use swamp_script_semantic::{Expression, ExpressionKind, ProgramState};
use swamp_script_source_map::SourceMap;
use tracing::warn;

fn internal_compile(script: &str) -> Result<(SymbolTable, Option<Expression>), Error> {
    let parser = AstParser {};

    let program = parser.parse_module(script).expect("Failed to parse script");

    let mut state = ProgramState::new();
    let modules = Modules::new();

    let mut mount_maps = SeqMap::new();
    mount_maps
        .insert("crate".to_string(), Path::new(".").to_path_buf())
        .unwrap();

    let mut source_map = SourceMap::new(&mount_maps);
    let file_id = 0xffff;

    source_map.add_manual(file_id, "crate", Path::new("some_path/main"), script);

    let mut analyzer = Analyzer::new(&mut state, &modules, &source_map, file_id);

    let mut resolved_definitions = Vec::new();
    for definition in &program.definitions {
        let result = analyzer.analyze_definition(definition);
        match result {
            Ok(analyzed_definition) => {
                resolved_definitions.push(analyzed_definition);
            }
            Err(err) => {
                show_error(&err, &source_map);
                return Err(err)?;
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
                show_error(&err, &source_map);
                return Err(err)?;
            }
        }
        None => None,
    };

    Ok((analyzer.shared.definition_table, maybe_resolved_expression))
}

/// # Panics
/// Intentionally panics if error message is not equal to the error encountered.
pub fn check_fail(script: &str, expected_error_message: &str) {
    let error = internal_compile(script).expect_err("should have failed");

    assert_eq!(format!("{error:?}"), expected_error_message.trim());
}

pub struct FormatExpression {
    pub expression: Expression,
}

impl Debug for FormatExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        pretty_print(f, &self.expression, 0)
    }
}

/// # Panics
/// Intentionally panics if output is not the same as the `expected_output`
pub fn check(script: &str, expected_output: &str) {
    let (symbol_table, expression) = internal_compile(script).expect("should work");

    let mut formatted_output = String::new();

    if !symbol_table.is_empty() {
        formatted_output += &*format!("{symbol_table:?}");
    }
    if let Some(expr) = expression {
        let format_expr = FormatExpression { expression: expr };
        formatted_output += &*format!("{format_expr:?}");
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
