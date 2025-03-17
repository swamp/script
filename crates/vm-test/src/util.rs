use swamp_script_code_gen::{CodeGenState, GenOptions};
use swamp_script_compile::compile_string;
use swamp_script_semantic::Function;
use swamp_script_types::Type;
use swamp_vm::Vm;
use swamp_vm_disasm::{disasm_instructions_color, disasm_instructions_no_color};

pub fn gen_internal(code: &str) -> CodeGenState {
    let (program, main_module, source_map) = compile_string(code).unwrap();

    let mut code_gen = CodeGenState::new();

    let main_expression = main_module.main_expression.as_ref().unwrap();
    let halt_function = GenOptions {
        is_halt_function: true,
    };

    code_gen.gen_main_function(main_expression, &halt_function);

    let normal_function = GenOptions {
        is_halt_function: false,
    };

    for internal_function_def in &main_module.symbol_table.internal_functions() {
        code_gen.gen_function_def(internal_function_def, &normal_function);
    }

    for (associated_on_type, impl_functions) in program.state.associated_impls.functions {
        if !associated_on_type.is_concrete() {
            continue;
        }
        if associated_on_type == Type::Int
            || associated_on_type == Type::Float
            || associated_on_type == Type::Bool
            || associated_on_type == Type::String
        {
            continue;
        }

        for (_name, func) in impl_functions.functions {
            match &*func {
                Function::Internal(int_fn) => {
                    code_gen.gen_function_def(int_fn, &normal_function);
                }

                Function::External(_ext_fn) => {}
            }
        }
    }

    code_gen.finalize();

    code_gen
}

pub fn gen_internal_debug(code: &str) -> CodeGenState {
    let code_gen = gen_internal(code);
    let disassembler_output = disasm_instructions_color(
        code_gen.instructions(),
        code_gen.comments(),
        &code_gen.create_function_sections(),
    );

    eprintln!("{disassembler_output}");

    code_gen
}

pub fn exec_internal(code: &str) -> Vm {
    let code_gen = gen_internal_debug(code);

    let instructions = code_gen.take_instructions();

    let mut vm = Vm::new(instructions, 32000);

    vm.execute();

    vm
}

pub fn exec_internal_debug(code: &str) -> Vm {
    let vm = exec_internal(code);

    vm
}

fn trim_lines(text: &str) -> String {
    text.lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}

fn compare_line_outputs(encountered: &str, expected: &str) {
    let encountered_trimmed = trim_lines(encountered);
    let expected_trimmed = trim_lines(expected);

    eprintln!("{encountered}");
    assert_eq!(encountered_trimmed, expected_trimmed);
}

fn compare_hex_outputs(memory: &[u8], expected_hex: &str) {
    let encountered_hexed = hexify::format_hex(memory);
    let expected_hex_trimmed = expected_hex.trim();

    compare_line_outputs(&encountered_hexed, expected_hex_trimmed);
}

pub fn exec(code: &str, expected_hex: &str) {
    let vm = exec_internal_debug(code);

    compare_hex_outputs(&vm.stack_base_memory()[..16], expected_hex);
}
pub fn exec_vars(code: &str, expected_hex: &str) {
    let vm = exec_internal_debug(code);

    compare_hex_outputs(&vm.frame_memory()[..16], expected_hex);
}

pub fn gen_code(code: &str, expected_output: &str) {
    let generator = gen_internal_debug(code);

    let disassembler_output = disasm_instructions_no_color(
        generator.instructions(),
        generator.comments(),
        &generator.create_function_sections(),
    );

    compare_line_outputs(&disassembler_output, expected_output);
}
