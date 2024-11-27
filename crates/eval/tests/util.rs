/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_script_eval::{ExecuteError, Interpreter, ValueWithSignal};
use swamp_script_parser::AstParser;
use swamp_script_semantic::ns::ResolvedModuleNamespace;

fn compile(script: &str) -> Result<(ValueWithSignal, Vec<String>), ExecuteError> {
    let parser = AstParser::new();
    let mut interpreter = Interpreter::new();

    let program = parser.parse_script(script).unwrap();

    let resolved_program = swamp_script_analyzer::create_program("");

    let value = interpreter.eval_program(program)?;

    let output = interpreter.get_output();

    Ok((value, output))
}

/*
fn register_builtins(namespace: &mut ResolvedModuleNamespace) {
    let output = self.output.clone();
    let print_fn = FunctionRef::External(
        LocalTypeIdentifier::new("print"),
        (
            vec![ResolvedParameter {
                variable: ResolvedVariable::new("value", false),
                param_type: ResolvedType::Any,
                is_mutable: false,
            }],
            ResolvedType::Unit,
        ),
        Rc::new(Box::new(move |args: &[Value]| {
            if let Some(value) = args.first() {
                let display_value = value.to_string();
                output.borrow_mut().push(display_value.clone());
                println!("{}", display_value);
                Ok(Value::Unit)
            } else {
                Err("print requires at least one argument".to_string())?
            }
        })),
    );

    // Add directly to the current module's namespace values
   namespace.add_external_function
        .values
        .insert("print".to_string(), Value::InternalFunction(print_fn));
}*/

pub fn check(script: &str, expected_result: &str) {
    let (_v, output) = compile(script).expect("eval script failed");

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
    let err = compile(script).err().unwrap();

    assert_eq!(
        err,
        ExecuteError::Error(expected_err.parse().unwrap()),
        "\nExpected:\n{:#?}",
        expected_err
    );
}
