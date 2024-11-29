/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::env;
use std::path::PathBuf;
use swamp_script_analyzer::dep::DependencyParser;
use swamp_script_analyzer::{parse_dependant_modules_and_resolve, ParseModule, ResolveError};
use swamp_script_ast::{ModulePath, Node, Position, Span};
use swamp_script_parser::AstParser;
use swamp_script_semantic::ResolvedProgram;
use tracing::{debug, warn};
use tracing::{error, trace};

fn get_test_fixtures_directory(suffix: &str) -> PathBuf {
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join(suffix);

    let current_dir = env::current_dir().unwrap();
    path.strip_prefix(current_dir)
        .unwrap_or(&*path)
        .to_path_buf()
}

pub fn create_program(script: &str) -> Result<ResolvedProgram, ResolveError> {
    let parser = AstParser::new();
    let ast_program = parser.parse_script(script)?;
    trace!("ast_program:\n{:#?}", ast_program);

    let parse_module = ParseModule { ast_program };

    let mut graph = DependencyParser::new();

    let root = ModulePath(vec!["test".to_string()]);
    graph.add_ast_module(root.clone(), parse_module);

    let root_path = get_test_fixtures_directory("first");
    debug!("root path is {root_path:?}");
    let mut resolved_program = ResolvedProgram::new();

    parse_dependant_modules_and_resolve(root_path, root, &mut graph, &mut resolved_program)?;

    Ok(resolved_program)
}

// Parse should work, but resolve should fail
pub fn check_fail(script: &str, expected_error: &str) {
    let resolved_program_err = create_program(script).err().expect("Expected error");
    let output = format!("{resolved_program_err:?}");
    warn!("semantic output: '{}'", output);
    assert_eq!(output, expected_error.trim());
}

pub fn check(script: &str, expected_output: &str) {
    let resolved_program_result = create_program(script);
    if resolved_program_result.is_err() {
        error!("{:?}", resolved_program_result.as_ref().unwrap_err());
        panic!(
            "Failed to create program {:?}",
            resolved_program_result.unwrap_err()
        );
    }
    let resolved_program = resolved_program_result.unwrap();
    let formatted_output = resolved_program.to_string();

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
