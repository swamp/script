/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_analyzer::lookup::NameLookup;
use swamp_script_analyzer::prelude::*;
use swamp_script_dep_loader::prelude::*;
use swamp_script_semantic::modules::ResolvedModules;
use swamp_script_semantic::prelude::*;
use swamp_script_source_map::SourceMap;
use tracing::info;

pub fn resolve_to_new_module(
    types: &ResolvedProgramTypes,
    state: &mut ResolvedProgramState,
    modules: &mut ResolvedModules,
    module_path: &[String],
    source_map: &SourceMap,
    ast_module: &ParseModule,
) -> Result<(), ResolveError> {
    let resolved_module = ResolvedModule::new(module_path);
    let resolved_module_ref = Rc::new(RefCell::new(resolved_module));
    modules.add(resolved_module_ref);

    resolve_to_existing_module(
        types,
        state,
        modules,
        module_path.to_vec(),
        source_map,
        ast_module,
    )?;

    Ok(())
}

pub fn resolve_to_existing_module(
    types: &ResolvedProgramTypes,
    state: &mut ResolvedProgramState,
    mut modules: &mut ResolvedModules,
    path: Vec<String>,
    source_map: &SourceMap,
    ast_module: &ParseModule,
) -> Result<Option<ResolvedExpression>, ResolveError> {
    let statements = {
        let mut name_lookup = NameLookup::new(path.clone(), &mut modules);
        let mut resolver = Resolver::new(
            types,
            state,
            &mut name_lookup,
            source_map,
            ast_module.file_id,
        );

        info!(?path, "analyzing");

        for ast_def in ast_module.ast_module.definitions() {
            let _resolved_def = resolver.resolve_definition(ast_def)?;
        }

        let maybe_resolved_expression = if let Some(expr) = ast_module.ast_module.expression() {
            Some(resolver.resolve_expression(expr)?)
        } else {
            None
        };
        maybe_resolved_expression
    };

    Ok(statements)
}

pub fn resolve_program(
    types: &ResolvedProgramTypes,
    state: &mut ResolvedProgramState,
    modules: &mut ResolvedModules,
    source_map: &SourceMap,
    module_paths_in_order: &[Vec<String>],
    parsed_modules: &DependencyParser,
) -> Result<(), ResolveError> {
    for module_path in module_paths_in_order {
        if let Some(parse_module) = parsed_modules.get_parsed_module(module_path) {
            if let Some(found_module) = modules.get(&*module_path.clone()) {
                info!(?module_path, "this is an existing module");

                let _maybe_expression = resolve_to_existing_module(
                    types,
                    state,
                    modules,
                    module_path.clone(),
                    source_map,
                    parse_module,
                )?;
            } else {
                info!(?module_path, "this is a new module");
                resolve_to_new_module(
                    types,
                    state,
                    modules,
                    module_path,
                    source_map,
                    parse_module,
                )?;
            }
        } else {
            return Err(ResolveError::CanNotFindModule(module_path.clone()));
        }
    }
    Ok(())
}
