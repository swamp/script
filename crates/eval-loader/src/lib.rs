/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_analyzer::err::ResolveErrorKind;
use swamp_script_analyzer::lookup::NameLookup;
use swamp_script_analyzer::prelude::*;
use swamp_script_dep_loader::prelude::*;
use swamp_script_semantic::modules::ResolvedModules;
use swamp_script_semantic::prelude::*;
use swamp_script_source_map::SourceMap;

pub fn analyze_to_new_module(
    state: &mut ResolvedProgramState,
    modules: &mut ResolvedModules,
    module_path: &[String],
    source_map: &SourceMap,
    ast_module: &ParseModule,
) -> Result<(), ResolveError> {
    let resolved_module = ResolvedModule::new(module_path);
    let resolved_module_ref = Rc::new(RefCell::new(resolved_module));
    modules.add(resolved_module_ref);

    analyze_to_existing_module(state, modules, module_path.to_vec(), source_map, ast_module)?;

    Ok(())
}

pub fn analyze_to_existing_module(
    state: &mut ResolvedProgramState,
    mut modules: &mut ResolvedModules,
    path: Vec<String>,
    source_map: &SourceMap,
    ast_module: &ParseModule,
) -> Result<Option<ResolvedExpression>, ResolveError> {
    let statements = {
        let mut name_lookup = NameLookup::new(path.clone(), &mut modules);
        let mut resolver = Analyzer::new(state, &mut name_lookup, source_map, ast_module.file_id);

        for ast_def in ast_module.ast_module.definitions() {
            let _resolved_def = resolver.analyze_definition(ast_def)?;
        }

        let maybe_resolved_expression = if let Some(expr) = ast_module.ast_module.expression() {
            Some(resolver.analyze_expression(expr, None)?)
        } else {
            None
        };
        maybe_resolved_expression
    };

    Ok(statements)
}

pub fn analyze_program(
    state: &mut ResolvedProgramState,
    modules: &mut ResolvedModules,
    source_map: &SourceMap,
    module_paths_in_order: &[Vec<String>],
    parsed_modules: &DependencyParser,
) -> Result<(), ResolveError> {
    for module_path in module_paths_in_order {
        if let Some(parse_module) = parsed_modules.get_parsed_module(module_path) {
            if let Some(_found_module) = modules.get(&*module_path.clone()) {
                let _maybe_expression = analyze_to_existing_module(
                    state,
                    modules,
                    module_path.clone(),
                    source_map,
                    parse_module,
                )?;
            } else {
                analyze_to_new_module(state, modules, module_path, source_map, parse_module)?;
            }
        } else {
            return Err(ResolveError {
                kind: ResolveErrorKind::CanNotFindModule(module_path.clone()),
                node: ResolvedNode::default(),
            });
        }
    }
    Ok(())
}
