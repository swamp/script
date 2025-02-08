/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::cell::RefCell;
use std::path::Path;
use std::rc::Rc;
use swamp_script_analyzer::lookup::NameLookup;
use swamp_script_analyzer::prelude::ResolveError;
use swamp_script_analyzer::Resolver;
use swamp_script_doc::generate_html_doc;
use swamp_script_parser::AstParser;
use swamp_script_semantic::modules::{ResolvedModule, ResolvedModules};
use swamp_script_semantic::ns::ResolvedModuleNamespace;
use swamp_script_semantic::{ResolvedDefinition, ResolvedProgramState, ResolvedType};
use swamp_script_source_map::SourceMap;
use tracing::warn;

fn internal_compile(script: &str) -> Result<(SourceMap, ResolvedModule), ResolveError> {
    let parser = AstParser {};

    let program = parser.parse_module(script).expect("Failed to parse script");

    let mut state = ResolvedProgramState::new();
    let mut modules = ResolvedModules::new();

    let mut source_map = SourceMap::new(Path::new("tests/fixtures/"));
    let file_id = 0xffff;

    source_map.add_manual(file_id, Path::new("some_path/main"), script);
    let resolved_path_str = vec!["test".to_string()];
    let _own_module = modules.add_empty_module(&resolved_path_str);

    let mut name_lookup = NameLookup::new(resolved_path_str, &mut modules);

    let mut resolver = Resolver::new(&mut state, &mut name_lookup, &source_map, file_id);

    let mut resolved_definitions = Vec::new();
    for definition in &program.definitions {
        let resolved_definition = resolver.resolve_definition(definition)?;
        resolved_definitions.push(resolved_definition);
    }

    let expression = &program.expression;
    let maybe_resolved_expression = match expression {
        Some(unwrapped_expression) => {
            let result = resolver.resolve_expression(unwrapped_expression, None);
            if let Ok(expression) = result {
                Some(expression)
            } else {
                let err = result.err().unwrap();
                return Err(err)?;
            }
        }
        None => None,
    };

    let ns_ref = Rc::new(RefCell::new(ResolvedModuleNamespace::new(&[])));

    let resolved_module = ResolvedModule {
        definitions: resolved_definitions,
        expression: maybe_resolved_expression,
        namespace: ns_ref,
    };

    Ok((source_map, resolved_module))
}

/// # Panics
/// Intentionally panics if output is not the same as the `expected_output`
pub fn check_doc(script: &str, expected_output: &str) {
    let (source_map, resolved_module) = internal_compile(script).expect("should work to analyze");
    let mut formatted_output = "".to_string();

    for def in resolved_module.definitions {
        match def {
            ResolvedDefinition::StructType(_) => {}
            ResolvedDefinition::EnumType(_) => {}
            ResolvedDefinition::ImplType(x) => match &x {
                ResolvedType::Int => {}
                ResolvedType::Float => {}
                ResolvedType::String => {}
                ResolvedType::Bool => {}
                ResolvedType::Unit => {}
                ResolvedType::Array(_) => {}
                ResolvedType::Tuple(_) => {}
                ResolvedType::Struct(struct_type) => {
                    formatted_output += &*generate_html_doc(struct_type, &source_map);
                }
                ResolvedType::Map(_) => {}
                ResolvedType::Enum(_) => {}
                ResolvedType::Generic(_, _) => {}
                ResolvedType::Function(_) => {}
                ResolvedType::Iterable(_) => {}
                ResolvedType::Optional(_) => {}
                ResolvedType::RustType(_) => {}
            },
            ResolvedDefinition::FunctionDef(_) => {}
            ResolvedDefinition::Alias(_) => {}
            ResolvedDefinition::Comment(_) => {}
            ResolvedDefinition::Use(_) => {}
            ResolvedDefinition::Constant(_, _) => {}
        }
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
