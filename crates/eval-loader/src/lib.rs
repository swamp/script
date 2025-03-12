/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use swamp_script_analyzer::prelude::{Error, Program};
use swamp_script_analyzer::{Analyzer, TypeContext, TypeContextScope};
use swamp_script_dep_loader::{
    DependencyParser, ParsedAstModule, parse_local_modules_and_get_order,
};
use swamp_script_modules::prelude::*;
use swamp_script_modules::symtbl::SymbolTableRef;
use swamp_script_semantic::{Expression, ProgramState, SemanticError};
use swamp_script_source_map::SourceMap;
use tracing::{Level, debug, span};

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
    default_lookup_symbol_table: &SymbolTable,
    modules: &mut Modules,
    core_symbol_table: &SymbolTableRef,
    source_map: &SourceMap,
    module_path: &[String],
    ast_module: &ParsedAstModule,
) -> Result<(SymbolTable, Option<Expression>), LoaderErr> {
    let mut resolver = Analyzer::new(
        state,
        modules,
        core_symbol_table.clone(),
        source_map,
        module_path,
        ast_module.file_id,
    );

    resolver.shared.lookup_table = default_lookup_symbol_table.clone();

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
    default_lookup_symbol_table: &SymbolTable,
    modules: &mut Modules,
    core_symbol_table: SymbolTableRef,
    source_map: &SourceMap,
    module_paths_in_order: &[Vec<String>],
    parsed_modules: &DependencyParser,
) -> Result<(), LoaderErr> {
    debug!(?module_paths_in_order, "analyzing modules in order");
    for module_path in module_paths_in_order {
        if *module_path == ["core"] {
            continue;
        }
        if let Some(parse_module) = parsed_modules.get_parsed_module(module_path) {
            let process_span = span!(Level::TRACE, "analyze mod", path = ?module_path);
            let enter_ = process_span.enter();

            let (analyzed_symbol_table, maybe_expression) = analyze_module(
                state,
                default_lookup_symbol_table,
                modules,
                &core_symbol_table,
                source_map,
                module_path,
                parse_module,
            )?;
            let analyzed_module = Module::new(analyzed_symbol_table, maybe_expression);
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
        &resolved_program.default_symbol_table,
        &mut resolved_program.modules,
        core_symbol_table,
        source_map,
        &module_paths_in_order,
        &dependency_parser,
    )?;

    Ok(())
}
