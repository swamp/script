/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::cell::RefCell;
use std::path::PathBuf;
use std::rc::Rc;
use swamp_script_analyzer::dep::DependencyParser;
use swamp_script_analyzer::{parse_dependant_modules_and_resolve, ParseModule, ResolveError};
use swamp_script_ast::{Parameter, Type, Variable};
use swamp_script_eval::value::Value;
use swamp_script_eval::{ExecuteError, Interpreter};
use swamp_script_parser::AstParser;
use swamp_script_semantic::{ModulePath, ResolvedProgram};
use tracing::{debug, trace};

#[derive(Debug)]
#[allow(dead_code)]
pub enum EvalTestError {
    ExecuteError(ExecuteError),
    ResolveError(ResolveError),
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

pub fn create_parsed_modules(
    script: &str,
    root_path: PathBuf,
) -> Result<DependencyParser, ResolveError> {
    let parser = AstParser::new();
    let ast_program = parser.parse_script(script)?;
    trace!("ast_program:\n{:#?}", ast_program);

    let parse_module = ParseModule { ast_program };

    let mut graph = DependencyParser::new();
    let root = ModulePath(vec!["test".to_string()]);
    graph.add_ast_module(root.clone(), parse_module);

    debug!("root path is {root_path:?}");

    Ok(graph)
}

fn compile_and_eval(script: &str) -> Result<(Value, Vec<String>), EvalTestError> {
    //    let parser = AstParser::new();
    //    let program = parser.parse_script(script).unwrap();

    // Analyze
    let mut parsed_modules = create_parsed_modules(script, PathBuf::new())?;
    let root_path = &ModulePath(vec!["test".to_string()]);

    let main_module = parsed_modules
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
        }],
        Type::Unit,
    );

    let mut resolved_program = ResolvedProgram::new();

    parse_dependant_modules_and_resolve(
        PathBuf::new(),
        root_path.clone(),
        &mut parsed_modules,
        &mut resolved_program,
    )?;

    let resolved_main_module = resolved_program
        .modules
        .get(&ModulePath(vec!["test".to_string()]))
        .expect("can not find module");

    // Run
    let mut interpreter = Interpreter::new();
    let output = Rc::new(RefCell::new(Vec::new()));
    register_print(&mut interpreter, output.clone());
    let value = interpreter.eval_module(resolved_main_module)?;

    let strings = output.borrow().to_vec();

    Ok((value, strings))
}

fn register_print(interpreter: &mut Interpreter, output: Rc<RefCell<Vec<String>>>) {
    interpreter
        .register_external_function(
            "print".to_string(),
            1, /* TODO: HARD CODED */
            move |args: &[Value]| {
                if let Some(value) = args.first() {
                    let display_value = value.to_string();
                    output.borrow_mut().push(display_value.clone());
                    println!("{}", display_value);
                    Ok(Value::Unit)
                } else {
                    Err("print requires at least one argument".to_string())?
                }
            },
        )
        .expect("should work to register");
}

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

pub fn check_fail(script: &str, expected_err: &str) {
    let err = compile_and_eval(script).err().expect("should fail");

    assert_eq!(
        format!("{:?}", err),
        expected_err.to_string(),
        "\nExpected:\n{:#?}",
        expected_err
    );
}

pub fn check_value(script: &str, expected_value: Value) {
    let (value_with_signal, _output) = compile_and_eval(script).expect("eval script failed");
    let value: Value = value_with_signal.try_into().unwrap();

    assert_eq!(value, expected_value);
}

pub fn eval(script: &str) -> Value {
    let (value_with_signal, _output) = compile_and_eval(script).expect("eval script failed");
    let value: Value = value_with_signal.try_into().unwrap();

    value
}
