/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
pub mod prelude;

use std::rc::Rc;
use swamp_script_analyzer::AutoUseModules;
use swamp_script_analyzer::err::ErrorKind;
use swamp_script_analyzer::prelude::*;
use swamp_script_dep_loader::prelude::*;
use swamp_script_modules::modules::ModuleRef;
use swamp_script_modules::modules::{Module, Modules};
use swamp_script_modules::symtbl::SymbolTable;
use swamp_script_semantic::prelude::*;
use swamp_script_source_map::SourceMap;

#[derive(Debug)]
pub enum LoaderErr {
    CouldNotLoad,
    SemanticError(SemanticError),
    AnalyzerError(Error),
}

impl From<Error> for LoaderErr {
    fn from(value: Error) -> Self {
        LoaderErr::AnalyzerError(value)
    }
}

pub fn resolve_to_new_module(
    state: &mut ProgramState,
    auto_use_modules: &AutoUseModules,
    modules: &mut Modules,
    module_path: &[String],
    source_map: &SourceMap,
    ast_module: &ParsedAstModule,
) -> Result<ModuleRef, LoaderErr> {
    let (analyzed_symbol_table, expression) = analyze_module(
        state,
        auto_use_modules,
        modules,
        source_map,
        module_path,
        ast_module,
    )?;

    let resolved_module = Module::new(module_path, analyzed_symbol_table, expression);
    let resolved_module_ref = Rc::new(resolved_module);

    modules.add(resolved_module_ref.clone());

    Ok(resolved_module_ref)
}

pub fn analyze_module(
    state: &mut ProgramState,
    auto_use_modules: &AutoUseModules,
    modules: &mut Modules,
    source_map: &SourceMap,
    canonical_path: &[String],
    ast_module: &ParsedAstModule,
) -> Result<(SymbolTable, Option<Expression>), LoaderErr> {
    let mut resolver = Analyzer::new(
        state,
        modules,
        source_map,
        canonical_path,
        ast_module.file_id,
    );
    if !auto_use_modules.modules.is_empty() {
        let target = &mut resolver.shared.lookup_table;
        for symbol_table in &auto_use_modules.modules {
            for (name, symbol) in symbol_table.symbols() {
                target
                    .add_symbol(name, symbol.clone())
                    .map_err(|err| LoaderErr::SemanticError(err))?;
            }
        }
    }

    let statements = {
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

    Ok((resolver.shared.definition_table, statements))
}

pub fn analyze_modules_in_order(
    state: &mut ProgramState,
    auto_use: &AutoUseModules,
    modules: &mut Modules,
    source_map: &SourceMap,
    module_paths_in_order: &[Vec<String>],
    parsed_modules: &DependencyParser,
) -> Result<(), LoaderErr> {
    for module_path in module_paths_in_order {
        if let Some(parse_module) = parsed_modules.get_parsed_module(module_path) {
            analyze_module(
                state,
                auto_use,
                modules,
                source_map,
                module_path,
                parse_module,
            )?;
        } else {
            return Err(LoaderErr::CouldNotLoad);
        }
    }
    Ok(())
}
