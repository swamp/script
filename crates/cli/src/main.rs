/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use clap::{Parser, Subcommand};

use std::fmt::{Display, Formatter};
use std::io;
use std::path::{Path, PathBuf};
use swamp_script_analyzer::prelude::Error;
use swamp_script_compile::{bootstrap_modules, create_registry_source_map};
use swamp_script_dep_loader::{swamp_registry_path, DepLoaderError};
use swamp_script_error_report::{show_script_resolve_error, ScriptResolveError};
use swamp_script_eval::err::ExecuteError;
use swamp_script_parser::prelude::*;
use swamp_script_pretty_print::{ImplsDisplay, SourceMapDisplay, SymbolTableDisplay};
use swamp_script_source_map_lookup::SourceMapWrapper;
use tracing::info;
use tracing_subscriber::EnvFilter;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compiles and build swamp script
    #[command(alias = "b")]
    Build {
        #[arg(default_value = ".")]
        path: PathBuf,

        #[arg(default_value = "main")]
        module: String,
    },
    /// Run the swamp script project
    #[command(alias = "r")]
    Run {
        #[arg(default_value = ".")]
        path: PathBuf,
    },
}

fn init_logging() {
    tracing_subscriber::fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_writer(io::stderr)
        .init();
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    init_logging();

    let cli = Cli::parse();

    command(&cli.command)?;

    Ok(())
}

fn command(command: &Commands) -> Result<(), CliError> {
    match command {
        Commands::Build { path, module } => build(path, module),
        Commands::Run { path } => run(path),
    }
}

#[derive(Debug)]
pub enum CliError {
    IoError(io::Error),
    ParseError(ParseError),
    ResolveError(Error),
    ScriptResolveError(ScriptResolveError),
    ExecuteError(ExecuteError),
    DepLoaderError(DepLoaderError),
    Other(String),
}

impl Display for CliError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for CliError {}

impl From<io::Error> for CliError {
    fn from(value: io::Error) -> Self {
        Self::IoError(value)
    }
}

impl From<ExecuteError> for CliError {
    fn from(value: ExecuteError) -> Self {
        Self::ExecuteError(value)
    }
}

impl From<Error> for CliError {
    fn from(value: Error) -> Self {
        Self::ResolveError(value)
    }
}

impl From<DepLoaderError> for CliError {
    fn from(value: DepLoaderError) -> Self {
        Self::DepLoaderError(value)
    }
}

impl From<ParseError> for CliError {
    fn from(value: ParseError) -> Self {
        Self::ParseError(value)
    }
}

impl From<String> for CliError {
    fn from(value: String) -> Self {
        Self::Other(value)
    }
}

impl From<ScriptResolveError> for CliError {
    fn from(value: ScriptResolveError) -> Self {
        Self::ScriptResolveError(value)
    }
}

pub struct CliContext;

#[allow(clippy::too_many_lines)]
fn build(_local_path: &Path, _local_module_name: &str) -> Result<(), CliError> {
    let registry_path = swamp_registry_path()?;

    let mut source_map = create_registry_source_map(&registry_path)?;
    let bootstrap_result = bootstrap_modules(&mut source_map);

    match bootstrap_result {
        Err(err) => {
            show_script_resolve_error(&err, &source_map);
            Err(err)?;
        }
        Ok(bootstrap) => {
            info!(?bootstrap, "bootstrap worked");
            let source_map_lookup = SourceMapWrapper { source_map };
            let pretty_printer = SourceMapDisplay {
                source_map: &source_map_lookup,
            };

            let symbol_table_display = SymbolTableDisplay {
                symbol_table: &bootstrap.default_symbol_table,
                source_map: &pretty_printer,
            };

            info!(%symbol_table_display, "default symbol table");

            let core_module_symbol_table = &bootstrap
                .modules
                .get(&bootstrap.core_module_path)
                .unwrap()
                .namespace
                .symbol_table;

            let core_module_symbol_table_display = SymbolTableDisplay {
                symbol_table: core_module_symbol_table,
                source_map: &pretty_printer,
            };

            info!(%core_module_symbol_table_display, "core symbol table");

            let all_impls_display = ImplsDisplay {
                all_impls: &bootstrap.state.associated_impls,
                source_map: &pretty_printer,
            };

            info!(%all_impls_display, "all impls");
        }
    }

    Ok(())

    /*
       // mangrove::render
       let mangrove_render_module_path = &["mangrove-0.0.0".to_string(), "render".to_string()];
       compile_analyze_and_link_without_version(
           mangrove_render_module_path,
           &mut resolved_program,
           &mut source_map,
       )?;

       // mangrove::collection
       let mangrove_collection_module_path = &["mangrove-0.0.0".to_string(), "collection".to_string()];
       compile_analyze_and_link_without_version(
           mangrove_collection_module_path,
           &mut resolved_program,
           &mut source_map,
       )?;


       {
           //let mangrove_collection_module_path_without_version =
           //  &["mangrove".to_string(), "collection".to_string()];
           //let mangrove_collection_module = resolved_program
           //  .modules
           //.get(mangrove_collection_module_path_without_version)
           //.unwrap();

           //let md = mangrove_collection_module.borrow_mut();
           //let mut ns = md.namespace.borrow_mut();
       }

       let result = compile_and_analyze(
           &["crate".to_string(), root_module.to_string()],
           &mut resolved_program,
           &mut source_map,
       );

       let lookup = SourceMapWrapper { source_map };
       match result {
           Ok(()) => {
               eprintln!(
                   "{}",
                   ModulesDisplay {
                       resolved_modules: &resolved_program.modules,
                       source_map: &lookup
                   }
               );
           }
           Err(err) => {
               show_script_resolve_error(&err, &lookup.source_map);
           }
       }
    */
}

fn run(_path: &Path) -> Result<(), CliError> {
    Ok(())
}
