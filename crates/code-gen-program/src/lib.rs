use swamp_script_code_gen::{CodeGenState, Error, GenOptions};
use swamp_script_compile::Program;
use swamp_script_modules::prelude::ModuleRef;
use swamp_script_semantic::Function;
use swamp_script_source_map_lookup::{SourceMapLookup, SourceMapWrapper};
use swamp_script_types::Type;

pub fn code_gen_program<'a>(
    program: &Program,
    main_module: &ModuleRef,
    source_map_lookup: &'a SourceMapWrapper,
) -> Result<CodeGenState<'a>, Error> {
    let mut code_gen = CodeGenState::new(source_map_lookup);

    code_gen.reserve_space_for_constants(&program.state.constants_in_dependency_order)?;

    if let Some(found_main_expression) = &main_module.main_expression {
        let halt_function = GenOptions {
            is_halt_function: true,
        };
        code_gen.gen_main_function(found_main_expression, &halt_function, source_map_lookup)?;
    }

    let normal_function = GenOptions {
        is_halt_function: false,
    };

    for internal_function_def in &main_module.symbol_table.internal_functions() {
        code_gen.gen_function_def(internal_function_def, &normal_function)?;
    }

    for (associated_on_type, impl_functions) in
        &program.state.instantiator.associated_impls.functions
    {
        if !associated_on_type.is_concrete() {
            continue;
        }
        if associated_on_type == &Type::Int
            || associated_on_type == &Type::Float
            || associated_on_type == &Type::Bool
            || associated_on_type == &Type::String
        {
            continue;
        }

        for (_name, func) in &impl_functions.functions {
            if func.name().clone().starts_with("instantiated ") {
                continue;
            }
            match &**func {
                Function::Internal(int_fn) => {
                    code_gen.gen_function_def(int_fn, &normal_function)?;
                }

                Function::External(_ext_fn) => {}
            }
        }
    }

    code_gen.gen_constants_expression_functions_in_order(
        &program.state.constants_in_dependency_order,
    )?;

    code_gen.finalize();

    Ok(code_gen)
}
