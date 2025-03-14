use swamp_script_code_gen::CodeGen;
use swamp_script_compile::compile_string;
use swamp_script_vm::Vm;

pub fn exec(code: &str, expected_hex: &str) {
    let (program, main_module, source_map) = compile_string(code).unwrap();

    let mut code_gen = CodeGen::new();

    let main_expression = main_module.main_expression.as_ref().unwrap();
    let mut function_ctx = code_gen.layout_variables(&main_expression.function_scope_state);

    code_gen.gen_expression(&main_expression.expression, &mut function_ctx);

    code_gen.finalize();

    let mut vm = Vm::new(code_gen.take_instructions(), 32000);

    vm.execute();

    let output = hexify::format_hex(&vm.stack_memory()[..16]);
    let compare_output = expected_hex.trim();

    eprintln!("{output}");
    assert_eq!(compare_output, output);
}
