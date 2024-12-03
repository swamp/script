/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::path::PathBuf;

use swamp_script_analyzer::ResolveError;
use swamp_script_ast::{Parameter, Type, Variable};
use swamp_script_dep_loader::{
    create_parsed_modules, parse_dependant_modules_and_resolve, DepLoaderError,
};
use swamp_script_eval::value::Value;
use swamp_script_eval::{eval_module, ExecuteError, ExternalFunctions};
use swamp_script_eval_loader::resolve_program;
use swamp_script_parser::Rule;
use swamp_script_semantic::{ExternalFunctionId, ModulePath, ResolvedProgram};

#[derive(Debug)]
#[allow(dead_code)]
pub enum EvalTestError {
    ExecuteError(ExecuteError),
    ResolveError(ResolveError),
    DepLoaderError(DepLoaderError),
    String(String),
}

impl From<ResolveError> for EvalTestError {
    fn from(e: ResolveError) -> Self {
        Self::ResolveError(e)
    }
}

impl From<ExecuteError> for EvalTestError {
    fn from(e: ExecuteError) -> Self {
        Self::ExecuteError(e)
    }
}

impl From<pest::error::Error<Rule>> for EvalTestError {
    fn from(e: pest::error::Error<Rule>) -> Self {
        Self::String(e.to_string())
    }
}

impl From<DepLoaderError> for EvalTestError {
    fn from(e: DepLoaderError) -> Self {
        Self::DepLoaderError(e)
    }
}

fn compile_and_eval(script: &str) -> Result<(Value, Vec<String>), EvalTestError> {
    //    let parser = AstParser::new();
    //    let program = parser.parse_script(script).unwrap();

    // Analyze
    let mut dependency_parser = create_parsed_modules(script, PathBuf::new())?;
    let root_path = &ModulePath(vec!["test".to_string()]);

    let main_module = dependency_parser
        .get_parsed_module_mut(root_path)
        .expect("should exist");

    main_module.declare_external_function(
        "print".to_string(),
        vec![Parameter {
            variable: Variable {
                name: "data".to_string(),
                is_mutable: false,
            },
            param_type: Type::Any,
            is_mutable: false,
            is_self: false,
        }],
        Type::Unit,
    );

    let module_paths_in_order = parse_dependant_modules_and_resolve(
        PathBuf::new(),
        root_path.clone(),
        &mut dependency_parser,
    )?;

    let mut resolved_program = ResolvedProgram::new();
    resolve_program(
        &resolved_program.types,
        &mut resolved_program.state,
        &mut resolved_program.modules,
        &module_paths_in_order,
        &dependency_parser,
    )?;

    let resolved_main_module = resolved_program
        .modules
        .get(&ModulePath(vec!["test".to_string()]))
        .expect("can not find module");

    // Run
    let mut externals = ExternalFunctions::new();
    register_print(1, &mut externals);

    let mut context = TestContext {
        secret: 42,
        output: vec![],
    };

    let value = eval_module(&externals, resolved_main_module, &mut context)?;

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
        .register_external_function("print", external_id, move |args: &[Value], context| {
            if let Some(value) = args.first() {
                let display_value = value.to_string();
                assert_eq!(context.secret, 42);
                context.output.push(display_value.clone());
                println!("{}", display_value);
                Ok(Value::Unit)
            } else {
                Err("print requires at least one argument".to_string())?
            }
        })
        .expect("should work to register");
}

#[allow(dead_code)] // TODO: this should not be needed since it is under tests/
pub fn check(script: &str, expected_result: &str) {
    let (_v, output) = compile_and_eval(script).expect("eval script failed");

    let actual_lines: Vec<&str> = output.iter().map(|s| s.trim()).collect();
    let expected_lines: Vec<&str> = expected_result
        .lines()
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .collect();

    assert_eq!(
        actual_lines, expected_lines,
        "\nExpected:\n{:#?}\n\nGot:\n{:#?}\n",
        expected_lines, actual_lines
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
    let (value_with_signal, _output) = compile_and_eval(script).expect("eval script failed");
    let value: Value = value_with_signal.try_into().unwrap();

    assert_eq!(value, expected_value);
}

#[allow(dead_code)] // TODO: This should not be needed since it is under tests/
pub fn eval(script: &str) -> Value {
    let (value_with_signal, _output) = compile_and_eval(script).expect("eval script failed");
    let value: Value = value_with_signal.try_into().unwrap();

    value
}
