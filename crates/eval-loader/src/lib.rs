/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use std::rc::Rc;
use swamp_script_analyzer::prelude::*;
use swamp_script_ast::prelude::*;
use swamp_script_dep_loader::prelude::*;
use swamp_script_semantic::prelude::*;

pub fn resolve_module(
    resolved_program: &mut ResolvedProgram,
    module_path: &ModulePath,
    module: &ParseModule,
) -> Result<ResolvedModuleRef, ResolveError> {
    let mut resolve_module = ResolvedModule::new(module_path.clone());

    for ast_def in module.ast_module.definitions() {
        let mut resolver = Resolver::new(resolved_program, &mut resolve_module);
        let resolved_def = resolver.resolve_definition(ast_def)?;
        resolver.insert_definition(resolved_def)?;
    }

    {
        let mut resolver = Resolver::new(resolved_program, &mut resolve_module);
        resolve_module.statements = resolver.resolve_statements(module.ast_module.statements())?;
    }

    let module_ref = Rc::new(resolve_module);
    resolved_program
        .modules
        .add_module(module_path.clone(), module_ref.clone())?;

    Ok(module_ref)
}

pub fn resolve_program(
    resolved_program: &mut ResolvedProgram,
    module_paths_in_order: &[ModulePath],
    parsed_modules: &DependencyParser,
) -> Result<(), ResolveError> {
    for module_path in module_paths_in_order {
        if let Some(parse_module) = parsed_modules.get_parsed_module(module_path) {
            let _resolved_module = resolve_module(resolved_program, module_path, parse_module)?;
        } else {
            panic!("can not find parsed module");
        }
    }

    Ok(())
}
