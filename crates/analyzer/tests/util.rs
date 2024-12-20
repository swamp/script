/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::path::Path;
use swamp_script_analyzer::modules::ResolvedModules;
use swamp_script_analyzer::{ResolveError, Resolver};
use swamp_script_parser::AstParser;
use swamp_script_semantic::{
    ResolvedDefinition, ResolvedProgramState, ResolvedProgramTypes, ResolvedStatement,
};
use swamp_script_source_map::SourceMap;
use tracing::warn;

fn internal_compile(
    script: &str,
) -> Result<(Vec<ResolvedDefinition>, Vec<ResolvedStatement>), ResolveError> {
    let parser = AstParser {};

    let program = parser.parse_module(script).expect("Failed to parse script");

    let types = ResolvedProgramTypes::new();
    let mut state = ResolvedProgramState::new();
    let mut modules = ResolvedModules::new();
    let mut source_map = SourceMap::new(Path::new("tests/fixtures/"));
    let file_id = 0xffff;

    source_map.add_manual(file_id, Path::new("some_path/main"), script);
    let resolved_path_str = vec!["test".to_string()];

    let mut resolver = Resolver::new(
        &types,
        &mut state,
        &mut modules,
        &source_map,
        resolved_path_str,
        file_id,
    );

    let mut resolved_definitions = Vec::new();
    for definition in &program.definitions {
        let resolved_definition = resolver.resolve_definition(definition)?;

        resolved_definitions.push(resolved_definition);
    }

    let mut resolved_statements = Vec::new();
    for statement in &program.statements {
        let resolved_statement = resolver.resolve_statement(statement)?;

        resolved_statements.push(resolved_statement);
    }

    Ok((resolved_definitions, resolved_statements))
}

pub fn check_fail(script: &str, expected_error_message: &str) {
    let error = internal_compile(script).expect_err("should have failed");

    assert_eq!(format!("{error:?}"), expected_error_message.trim());
}

pub fn check(script: &str, expected_output: &str) {
    let (resolved_definitions, resolved_statements) =
        internal_compile(script).expect("should work to analyze");

    let mut formatted_output = String::new();
    for resolved_def in &resolved_definitions {
        formatted_output += &*format!("{resolved_def:?}\n");
    }
    if !resolved_definitions.is_empty() && !resolved_statements.is_empty() {
        formatted_output += "---\n";
    }
    for resolved_statement in &resolved_statements {
        formatted_output += &*format!("{resolved_statement:?}\n");
    }

    let actual = formatted_output
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n");

    let expected = expected_output
        .lines()
        .map(|line| line.trim())
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n");

    if actual != expected {
        panic!(
            "\nExpected output:\n\n{}\n\nActual output:\n\n{}\n",
            expected, actual
        );
    }

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
