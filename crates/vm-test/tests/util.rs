use swamp_script_code_gen::CodeGen;
use swamp_script_compile::compile_string;
use swamp_script_types::Type;
use swamp_script_vm::Vm;

pub fn exec_internal(code: &str) -> Vm {
    let (program, main_module, source_map) = compile_string(code).unwrap();

    let mut code_gen = CodeGen::new();

    let main_expression = main_module.main_expression.as_ref().unwrap();
    let mut function_ctx = code_gen.layout_variables(
        &main_expression.function_scope_state,
        &main_expression.expression.ty,
    );

    let mut test_ctx = function_ctx.temp_space_for_type(&Type::Unit);

    code_gen.gen_expression(&main_expression.expression, &mut test_ctx);

    code_gen.finalize();

    let mut vm = Vm::new(code_gen.take_instructions(), 32000);

    vm.execute();

    vm
}

fn compare_outputs(memory: &[u8], expected_hex: &str) {
    let output = hexify::format_hex(memory);
    let compare_output = expected_hex.trim();

    eprintln!("{output}");
    assert_eq!(compare_output, output);
}

pub fn exec(code: &str, expected_hex: &str) {
    let vm = exec_internal(code);

    compare_outputs(&vm.stack_memory()[..16], expected_hex);
}
pub fn exec_vars(code: &str, expected_hex: &str) {
    let vm = exec_internal(code);

    compare_outputs(&vm.frame_memory()[..16], expected_hex);
}
