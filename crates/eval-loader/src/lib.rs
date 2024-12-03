/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_script_analyzer::prelude::*;
use swamp_script_ast::prelude::*;
use swamp_script_dep_loader::prelude::*;
use swamp_script_semantic::prelude::*;
use swamp_script_semantic::ResolvedModules;
use swamp_script_semantic::ResolvedProgramState;
use swamp_script_semantic::ResolvedProgramTypes;

pub fn resolve_to_new_module(
    types: &ResolvedProgramTypes,
    state: &mut ResolvedProgramState,
    modules: &mut ResolvedModules,

    module_path: &ModulePath,
    ast_module: &ParseModule,
) -> Result<(), ResolveError> {
    let mut resolved_module = ResolvedModule::new(module_path.clone());

    resolve_to_existing_module(types, state, modules, &mut resolved_module, ast_module)?;

    modules.add_module(resolved_module)?;
    Ok(())
}

pub fn resolve_to_existing_module(
    types: &ResolvedProgramTypes,
    state: &mut ResolvedProgramState,
    modules: &mut ResolvedModules,
    resolved_module: &mut ResolvedModule,
    ast_module: &ParseModule,
) -> Result<(), ResolveError> {
    for ast_def in ast_module.ast_module.definitions() {
        let mut resolver = Resolver::new(&types, state, &modules, resolved_module);
        let resolved_def = resolver.resolve_definition(ast_def)?;
        resolver.insert_definition(resolved_def)?;
    }

    {
        let mut resolver = Resolver::new(&types, state, &modules, resolved_module);
        resolved_module.statements =
            resolver.resolve_statements(ast_module.ast_module.statements())?;
    }
    Ok(())
}

pub fn resolve_program(
    types: &ResolvedProgramTypes,
    state: &mut ResolvedProgramState,
    modules: &mut ResolvedModules,

    module_paths_in_order: &[ModulePath],
    parsed_modules: &DependencyParser,
) -> Result<(), ResolveError> {
    for module_path in module_paths_in_order {
        if let Some(parse_module) = parsed_modules.get_parsed_module(module_path) {
            if modules.contains_key(module_path.clone()) {
                let mut existing_resolve_module = modules.modules.remove(module_path).unwrap();
                resolve_to_existing_module(
                    types,
                    state,
                    modules,
                    &mut existing_resolve_module,
                    parse_module,
                )?;
                modules
                    .modules
                    .insert(module_path.clone(), existing_resolve_module);
            } else {
                resolve_to_new_module(types, state, modules, module_path, parse_module)?;
            }
        } else {
            return Err(ResolveError::CanNotFindModule(module_path.clone()));
        }
    }
    Ok(())
}
