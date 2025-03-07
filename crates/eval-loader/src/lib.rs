/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_script_analyzer::prelude::{Error, Program};
use swamp_script_analyzer::{Analyzer, AutoUseModules, TypeContext, TypeContextScope};
use swamp_script_dep_loader::{
    DependencyParser, ParsedAstModule, parse_local_modules_and_get_order,
};
use swamp_script_modules::prelude::*;
use swamp_script_semantic::{Expression, ProgramState, SemanticError};
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

pub fn analyze_module(
    state: &mut ProgramState,
    auto_use_modules: &AutoUseModules,
    modules: &mut Modules,
    core_symbol_table: &SymbolTableRef,
    source_map: &SourceMap,
    ast_module: &ParsedAstModule,
) -> Result<(SymbolTable, Option<Expression>), LoaderErr> {
    let mut resolver = Analyzer::new(
        state,
        modules,
        core_symbol_table.clone(),
        source_map,
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

    let outside_context = TypeContext::new(None, None, TypeContextScope::ArgumentOrOutsideFunction);
    let statements = {
        for ast_def in ast_module.ast_module.definitions() {
            resolver.analyze_definition(ast_def)?;
        }

        let maybe_resolved_expression = if let Some(expr) = ast_module.ast_module.expression() {
            Some(resolver.analyze_expression(expr, &outside_context)?)
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
    core_symbol_table: SymbolTableRef,
    source_map: &SourceMap,
    module_paths_in_order: &[Vec<String>],
    parsed_modules: &DependencyParser,
) -> Result<(), LoaderErr> {
    for module_path in module_paths_in_order {
        if let Some(parse_module) = parsed_modules.get_parsed_module(module_path) {
            let (analyzed_symbol_table, maybe_expression) = analyze_module(
                state,
                auto_use,
                modules,
                &core_symbol_table,
                source_map,
                parse_module,
            )?;
            let analyzed_module = Module::new(module_path, analyzed_symbol_table, maybe_expression);
            modules.add(analyzed_module.into());
        } else {
            panic!("could not load")
        }
    }
    Ok(())
}

pub fn compile_and_analyze_all_modules(
    module_path: &[String],
    resolved_program: &mut Program,
    source_map: &mut SourceMap,
    core_symbol_table: SymbolTableRef,
) -> Result<(), LoaderErr> {
    let mut dependency_parser = DependencyParser::new();

    let module_paths_in_order =
        parse_local_modules_and_get_order(module_path.to_vec(), &mut dependency_parser, source_map)
            .unwrap(); // TODO: FIX THIS

    analyze_modules_in_order(
        &mut resolved_program.state,
        &resolved_program.auto_use_modules,
        &mut resolved_program.modules,
        core_symbol_table,
        source_map,
        &module_paths_in_order,
        &dependency_parser,
    )?;

    Ok(())
}
