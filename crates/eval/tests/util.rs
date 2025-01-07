/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */

use std::path::Path;
use swamp_script_analyzer::lookup::NameLookup;
use swamp_script_analyzer::{ResolveError, Resolver};
use swamp_script_core::prelude::Value;
use swamp_script_eval::prelude::{ExecuteError, VariableValue};
use swamp_script_eval::{eval_constants, eval_module, Constants, ExternalFunctions};
use swamp_script_parser::AstParser;
use swamp_script_semantic::modules::ResolvedModules;
use swamp_script_semantic::prelude::ResolvedModuleNamespaceRef;
use swamp_script_semantic::{
    ExternalFunctionId, FunctionTypeSignature, ResolvedExpression,
    ResolvedExternalFunctionDefinition, ResolvedNode, ResolvedParameterNode, ResolvedProgramState,
    ResolvedProgramTypes, ResolvedType, ResolvedTypeForParameter, SemanticError, Span,
};
use swamp_script_source_map::SourceMap;

#[derive(Debug)]
#[allow(dead_code)]
pub enum EvalTestError {
    ExecuteError(ExecuteError),
    ResolveError(ResolveError),
    String(String),
}

impl From<ResolveError> for EvalTestError {
    fn from(e: ResolveError) -> Self {
        Self::ResolveError(e)
    }
}

impl From<SemanticError> for EvalTestError {
    fn from(e: SemanticError) -> Self {
        Self::ResolveError(e.into())
    }
}

impl From<ExecuteError> for EvalTestError {
    fn from(e: ExecuteError) -> Self {
        Self::ExecuteError(e)
    }
}

fn internal_compile(
    script: &str,
    target_namespace: &ResolvedModuleNamespaceRef,
    modules: &mut ResolvedModules,
) -> Result<(Option<ResolvedExpression>, SourceMap), ResolveError> {
    let parser = AstParser {};

    let program = parser.parse_module(script).expect("Failed to parse script");

    let types = ResolvedProgramTypes::new();
    let mut state = ResolvedProgramState::new();
    // let modules = ResolvedModules::new();

    let mut source_map = SourceMap::new(Path::new("tests/fixtures/"));
    let file_id = 0xffff;

    source_map.add_manual(file_id, Path::new("some_path/main"), script);
    // let resolved_path_str = vec!["test".to_string()];
    // let own_module = modules.add_empty_module(&resolved_path_str);

    let mut name_lookup = NameLookup::new(target_namespace.clone(), modules);

    let mut resolver = Resolver::new(&types, &mut state, &mut name_lookup, &source_map, file_id);

    //let mut resolved_definitions = Vec::new();
    for definition in &program.definitions {
        resolver.resolve_definition(definition)?;
        //  resolved_definitions.push(resolved_definition);
    }

    let maybe_resolved_expression = program
        .expression
        .map(|expr| resolver.resolve_expression(&expr))
        .transpose()?;

    Ok((maybe_resolved_expression, source_map))
}

fn compile_and_eval(script: &str) -> Result<(Value, Vec<String>), EvalTestError> {
    let mut modules = ResolvedModules::new();
    let resolved_path_str = vec!["test".to_string()];
    let main_module = modules.add_empty_module(&resolved_path_str);

    let external_print = ResolvedExternalFunctionDefinition {
        name: ResolvedNode {
            span: Span::default(),
        },

        signature: FunctionTypeSignature {
            first_parameter_is_self: false,
            parameters: vec![ResolvedTypeForParameter {
                name: String::new(),
                resolved_type: ResolvedType::Any,
                is_mutable: false,
                node: None,
            }],
            return_type: Box::from(ResolvedType::Any),
        },
        id: 1,
    };

    main_module
        .borrow_mut()
        .namespace
        .borrow_mut()
        .add_external_function_declaration("print", external_print)
        .expect("TODO: panic message");

    let (maybe_expression, source_map) =
        internal_compile(script, &main_module.borrow_mut().namespace, &mut modules)?;
    main_module.borrow_mut().expression = maybe_expression;

    // Run
    let mut externals = ExternalFunctions::new();
    register_print(1, &mut externals);

    let mut context = TestContext {
        secret: 42,
        output: vec![],
    };

    modules.finalize()?;

    let mut constants = Constants::new();

    eval_constants(&externals, &mut constants, &modules, &mut context)?;

    let value = eval_module(
        &externals,
        &constants,
        main_module.borrow().expression.as_ref().unwrap(),
        &mut context,
    )?;

    Ok((value, context.output))
}

pub struct TestContext {
    pub secret: i32,
    pub output: Vec<String>,
}

fn register_print(
    external_id: ExternalFunctionId,
    external_functions: &mut ExternalFunctions<TestContext>,
) {
    external_functions
        .register_external_function(
            external_id,
            move |args: &[VariableValue], context: &mut TestContext| {
                if let Some(value) = args.first() {
                    let display_value = value.convert_to_string_if_needed();
                    assert_eq!(context.secret, 42);
                    context.output.push(display_value);
                    Ok(Value::Unit)
                } else {
                    Err("print requires at least one argument".to_string())?
                }
            },
        )
        .expect("should work to register");
}

#[allow(dead_code)] // TODO: this should not be needed since it is under tests/
pub fn check(script: &str, expected_result: &str) {
    let (_v, output) = compile_and_eval(script).expect("eval script failed");

    let actual_lines: Vec<&str> = output.iter().map(|s| s.trim()).collect();
    let expected_lines: Vec<&str> = expected_result
        .lines()
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .collect();

    assert_eq!(
        actual_lines, expected_lines,
        "\nExpected:\n{expected_lines:#?}\n\nGot:\n{actual_lines:#?}\n",
    );
}

#[allow(dead_code)] // TODO: this should not be needed since it is under tests/
pub fn check_fail(script: &str, expected_err: &str) {
    let err = compile_and_eval(script).err().expect("should fail");

    assert_eq!(
        format!("{:?}", err),
        expected_err.to_string(),
        "\nExpected:\n{:#?}",
        expected_err
    );
}

#[allow(dead_code)] // TODO: This should not be needed since it is under tests/
pub fn check_value(script: &str, expected_value: Value) {
    let (value, _output) = compile_and_eval(script).expect("eval script failed");

    assert_eq!(value, expected_value);
}

#[allow(dead_code)] // TODO: This should not be needed since it is under tests/
pub fn eval(script: &str) -> Value {
    let (value, _output) = compile_and_eval(script).expect("eval script failed");

    value
}

#[allow(dead_code)] // TODO: This should not be needed since it is under tests/
pub fn eval_string(script: &str, expected_string: &str) {
    let (value, _output) = compile_and_eval(script).expect("eval script failed");

    let value_string = value.to_string();

    assert_eq!(value_string, expected_string);
}
