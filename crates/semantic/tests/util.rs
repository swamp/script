/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::env;
use std::path::PathBuf;
use swamp_script_ast::{LocalIdentifier, ModulePath, Node, Position, Span};
use swamp_script_parser::AstParser;
use swamp_script_semantic::dep::DependencyGraph;
use swamp_script_semantic::{resolve, resolve_with_graph, ParseModule, ResolveError, ResolvedProgram};
use tracing::{info, warn};

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

fn create_program(script: &str) -> Result<ResolvedProgram, ResolveError> {
    let parser = AstParser::new();
    info!("before parsing");
    let ast_program = parser.parse_script(script)?;
    info!("Parsed the following AST program:\n{:#?}", ast_program);

    let parse_module = ParseModule { ast_program };

    let mut graph = DependencyGraph::new();
    let mut vec = Vec::new();
    vec.push(LocalIdentifier::new(
        Node {
            span: Span {
                start: Position {
                    offset: 0,
                    line: 0,
                    column: 0,
                },
                end: Position {
                    offset: 0,
                    line: 0,
                    column: 0,
                },
            },
        },
        "test",
    ));
    let root = ModulePath(vec);
    graph.add_ast_module(root.clone(), parse_module);

    let root_path = get_test_fixtures_directory("first");
    info!("root path is {root_path:?}");
    let mut resolved_program = ResolvedProgram::new();

    resolve_with_graph(root_path, root, &mut graph, &mut resolved_program)?;

    Ok(resolved_program)
}

// Parse should work, but resolve should fail
pub fn check_fail(script: &str, expected_error: &str) {
    let resolved_program_err = create_program(script).err().expect("Expected error");

    assert_eq!(format!("{resolved_program_err:?}"), expected_error.trim());
}

pub fn check(script: &str, expected_output: &str) {
    let resolved_program = create_program(script).expect("Failed to create program");

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
