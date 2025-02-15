/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use std::cell::RefCell;
use std::rc::Rc;
use swamp_script_analyzer::err::ErrorKind;
use swamp_script_analyzer::lookup::NameLookup;
use swamp_script_analyzer::prelude::*;
use swamp_script_analyzer::AutoUseModules;
use swamp_script_dep_loader::prelude::*;
use swamp_script_modules::modules::{Module, Modules};
use swamp_script_semantic::prelude::*;
use swamp_script_source_map::SourceMap;

pub fn resolve_to_new_module(
    state: &mut ProgramState,
    auto_use_modules: &AutoUseModules,
    modules: &mut Modules,
    module_path: &[String],
    source_map: &SourceMap,
    ast_module: &ParseModule,
) -> Result<(), Error> {
    let resolved_module = Module::new(module_path);
    let resolved_module_ref = Rc::new(RefCell::new(resolved_module));
    modules.add(resolved_module_ref);

    resolve_to_existing_module(
        state,
        auto_use_modules,
        modules,
        module_path.to_vec(),
        source_map,
        ast_module,
    )?;

    Ok(())
}

pub fn resolve_to_existing_module(
    state: &mut ProgramState,
    auto_use_modules: &AutoUseModules,
    modules: &mut Modules,
    path: Vec<String>,
    source_map: &SourceMap,
    ast_module: &ParseModule,
) -> Result<Option<Expression>, Error> {
    let statements = {
        let mut name_lookup = NameLookup::new(path.clone(), modules);
        let mut resolver = Resolver::new(state, &mut name_lookup, source_map, ast_module.file_id);

        if !auto_use_modules.modules.is_empty() {
            let target_module = resolver.shared.lookup.own_namespace();
            for namespace in &auto_use_modules.modules {
                for (_name, struct_type) in namespace.borrow().structs() {
                    target_module
                        .borrow_mut()
                        .add_struct_ref(struct_type.clone())?;
                }
            }
        }

        for ast_def in ast_module.ast_module.definitions() {
            resolver.analyze_definition(ast_def)?;
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

pub fn resolve_program(
    state: &mut ProgramState,
    auto_use: &AutoUseModules,
    modules: &mut Modules,
    source_map: &SourceMap,
    module_paths_in_order: &[Vec<String>],
    parsed_modules: &DependencyParser,
) -> Result<(), Error> {
    for module_path in module_paths_in_order {
        if let Some(parse_module) = parsed_modules.get_parsed_module(module_path) {
            if let Some(_found_module) = modules.get(&module_path.clone()) {
                let _maybe_expression = resolve_to_existing_module(
                    state,
                    auto_use,
                    modules,
                    module_path.clone(),
                    source_map,
                    parse_module,
                )?;
            } else {
                resolve_to_new_module(
                    state,
                    auto_use,
                    modules,
                    module_path,
                    source_map,
                    parse_module,
                )?;
            }
        } else {
            return Err(Error {
                kind: ErrorKind::CanNotFindModule(module_path.clone()),
                node: Node::default(),
            });
        }
    }
    Ok(())
}
