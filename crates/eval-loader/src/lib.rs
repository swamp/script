/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_script_analyzer::prelude::{Error, Program};
use swamp_script_analyzer::{Analyzer, AutoUseModules, TypeContext, TypeContextScope};
use swamp_script_dep_loader::{
    parse_local_modules_and_get_order, DepLoaderError, DependencyParser, ParsedAstModule,
};
use swamp_script_semantic::modules::Module;
use swamp_script_semantic::prelude::Modules;
use swamp_script_semantic::symtbl::SymbolTable;
use swamp_script_semantic::{Expression, ProgramState};
use swamp_script_source_map::SourceMap;

#[derive(Debug)]
pub enum EvalLoaderError {
    DepLoaderError(DepLoaderError),
    AnalyzerError(Error),
}

impl From<DepLoaderError> for EvalLoaderError {
    fn from(err: DepLoaderError) -> Self {
        Self::DepLoaderError(err)
    }
}

impl From<Error> for EvalLoaderError {
    fn from(err: Error) -> Self {
        Self::AnalyzerError(err)
    }
}

pub fn analyze_module(
    state: &mut ProgramState,
    auto_use_modules: &AutoUseModules,
    modules: &mut Modules,
    source_map: &SourceMap,
    ast_module: &ParsedAstModule,
) -> Result<(SymbolTable, Option<Expression>), Error> {
    let mut resolver = Analyzer::new(state, modules, source_map, ast_module.file_id);
    if !auto_use_modules.modules.is_empty() {
        let target = &mut resolver.shared.lookup_table;
        for symbol_table in &auto_use_modules.modules {
            for (name, symbol) in symbol_table.symbols() {
                target.add_symbol(name, symbol.clone())?;
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
    source_map: &SourceMap,
    module_paths_in_order: &[Vec<String>],
    parsed_modules: &DependencyParser,
) -> Result<(), Error> {
    for module_path in module_paths_in_order {
        if let Some(parse_module) = parsed_modules.get_parsed_module(module_path) {
            let (analyzed_symbol_table, maybe_expression) =
                analyze_module(state, auto_use, modules, source_map, parse_module)?;
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
) -> Result<(), EvalLoaderError> {
    let mut dependency_parser = DependencyParser::new();

    let module_paths_in_order = parse_local_modules_and_get_order(
        module_path.to_vec(),
        &mut dependency_parser,
        source_map,
    )?;

    analyze_modules_in_order(
        &mut resolved_program.state,
        &resolved_program.auto_use_modules,
        &mut resolved_program.modules,
        source_map,
        &module_paths_in_order,
        &dependency_parser,
    )?;

    Ok(())
}
