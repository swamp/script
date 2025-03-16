use swamp_script_code_gen::{CodeGenState, FunctionCodeGen};
use swamp_script_compile::compile_string;
use swamp_script_types::Type;
use swamp_vm::Vm;
use swamp_vm_disasm::disasm_instructions_color;

pub fn exec_internal(code: &str) -> Vm {
    let (program, main_module, source_map) = compile_string(code).unwrap();

    let mut code_gen = CodeGenState::new();

    let main_expression = main_module.main_expression.as_ref().unwrap();

    code_gen.gen_main_function(&main_expression);

    for internal_function_def in main_module.symbol_table.internal_functions() {
        code_gen.gen_function_def(internal_function_def);
    }

    code_gen.finalize();

    let instructions = code_gen.take_instructions();

    let mut vm = Vm::new(instructions, 32000);

    vm.execute();

    vm
}

pub fn exec_internal_debug(code: &str) -> Vm {
    let vm = exec_internal(code);
    let disasm_output = disasm_instructions_color(&vm.instructions());

    eprintln!("{disasm_output}");

    vm
}

fn compare_outputs(memory: &[u8], expected_hex: &str) {
    let output = hexify::format_hex(memory);
    let compare_output = expected_hex.trim();

    eprintln!("{output}");
    assert_eq!(compare_output, output);
}

pub fn exec(code: &str, expected_hex: &str) {
    let vm = exec_internal_debug(code);

    compare_outputs(&vm.stack_memory()[..16], expected_hex);
}
pub fn exec_vars(code: &str, expected_hex: &str) {
    let vm = exec_internal_debug(code);

    compare_outputs(&vm.frame_memory()[..16], expected_hex);
}
