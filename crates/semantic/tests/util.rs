/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::env;
use std::path::PathBuf;
use swamp_script_parser::AstParser;
use swamp_script_semantic::resolve;
use tracing::{info, warn};

fn get_test_fixtures(suffix: &str) -> PathBuf {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join(suffix);

    let current_dir = env::current_dir().unwrap();
    path.strip_prefix(current_dir)
        .unwrap_or(&*path)
        .to_path_buf()
}

pub fn check(script: &str, expected_output: &str) {
    let parser = AstParser::new();

    let root_path = get_test_fixtures("first");
    info!("root path is {root_path:?}");
    let resolved = resolve(root_path).expect("Failed to resolve program");

    let formatted_output = resolved.to_string();

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
