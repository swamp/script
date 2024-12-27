/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use clap::{Parser, Subcommand};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};
use std::{fs, io};
use swamp_script_analyzer::{ResolveError, ResolvedProgram};
use swamp_script_ast::{Node, Parameter, Type, Variable};
use swamp_script_core::prelude::Value;
use swamp_script_dep_loader::{
    parse_dependant_modules_and_resolve, DepLoaderError, DependencyParser, ParseModule,
};
use swamp_script_eval::{err::ExecuteError, eval_module, ExternalFunctions, SourceMapWrapper};
use swamp_script_eval_loader::resolve_program;
use swamp_script_parser::prelude::*;
use swamp_script_parser::AstParser;
use swamp_script_semantic::modules::ResolvedModuleRef;
use swamp_script_source_map::SourceMap;
use tracing::{debug, info, trace};
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
        .with_writer(std::io::stderr)
        .init();
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    init_logging();

    let cli = Cli::parse();

    Ok(command(&cli.command)?)
}

fn command(command: &Commands) -> Result<(), CliError> {
    match command {
        Commands::Build { path } => build(path),
        Commands::Run { path } => run(path),
    }
}

#[derive(Debug)]
pub enum CliError {
    IoError(std::io::Error),
    ParseError(ParseError),
    ResolveError(ResolveError),
    ExecuteError(ExecuteError),
    DepLoaderError(DepLoaderError),
    Other(String),
}

impl Display for CliError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Error for CliError {}

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

impl From<ResolveError> for CliError {
    fn from(value: ResolveError) -> Self {
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

pub struct CliContext;

// In the future we want to support directories with a project swamp.toml, but for now
// just resolve to single .swamp file
fn resolve_swamp_file(path: &Path) -> Result<PathBuf, String> {
    if !path.exists() {
        return Err(format!("Path does not exist: {}", path.display()));
    }

    if path.is_dir() {
        let main_file = path.join("main.swamp");
        if !main_file.exists() {
            return Err(format!(
                "No main.swamp found in directory: {}",
                path.display()
            ));
        }
        Ok(main_file)
    } else if path.extension().and_then(|ext| ext.to_str()) == Some("swamp") {
        Ok(path.to_path_buf())
    } else {
        Err(format!("Not a .swamp file: {}", path.display()))
    }
}

fn register_print(interpreter: &mut ExternalFunctions<CliContext>) {
    interpreter
        .register_external_function(
            "print",
            1, /* TODO: HARD CODED */
            move |args: &[Value], _context| {
                if let Some(value) = args.first() {
                    let display_value = value.convert_to_string_if_needed();
                    println!("{display_value}");
                    Ok(Value::Unit)
                } else {
                    Err("print requires at least one argument".to_string())?
                }
            },
        )
        .expect("should work to register");
}

pub fn eval(
    resolved_main_module: &ResolvedModuleRef,
    source_map_wrapper: SourceMapWrapper,
) -> Result<Value, CliError> {
    let mut external_functions = ExternalFunctions::new();
    register_print(&mut external_functions);
    let mut context = CliContext;
    let value = eval_module(
        &external_functions,
        &resolved_main_module.borrow().statements,
        &source_map_wrapper,
        &mut context,
    )?;
    Ok(value)
}

pub fn create_parsed_modules(
    script: &str,
    root_path: PathBuf,
) -> Result<DependencyParser, CliError> {
    let parser = AstParser;
    let ast_program = parser.parse_module(script)?;
    trace!("ast_program:\n{:#?}", ast_program);

    let parse_module = ParseModule {
        ast_module: ast_program,
        file_id: 0,
    };

    let mut graph = DependencyParser::new();
    let root = module_path();
    graph.add_ast_module(root, parse_module);

    debug!("root path is {root_path:?}");

    Ok(graph)
}

fn compile_to_resolved_program(script: &str) -> Result<(ResolvedProgram, SourceMap), CliError> {
    // Analyze
    let mut parsed_modules = create_parsed_modules(script, PathBuf::new())?;
    let root_path = &vec!["main".to_string()];

    let main_module = parsed_modules
        .get_parsed_module_mut(root_path)
        .unwrap_or_else(|| panic!("should exist {root_path:?}"));

    let mut source_map = SourceMap::new("".as_ref());

    main_module.declare_external_function(
        vec![Parameter {
            variable: Variable {
                name: Default::default(),
                is_mutable: None,
            },
            param_type: Type::Any(Node::default()),
        }],
        None,
    );

    let module_paths_in_order = parse_dependant_modules_and_resolve(
        PathBuf::new(),
        root_path.clone(),
        &mut parsed_modules,
        &mut source_map,
    )?;

    let mut resolved_program = ResolvedProgram::new();
    resolve_program(
        &resolved_program.types,
        &mut resolved_program.state,
        &mut resolved_program.modules,
        &source_map,
        &module_paths_in_order,
        &parsed_modules,
    )?;

    Ok((resolved_program, source_map))
}

fn compile_and_eval(script: &str) -> Result<Value, CliError> {
    let (resolved_program, source_map) = compile_to_resolved_program(script)?;

    let resolved_main_module = resolved_program
        .modules
        .get(&vec!["main".to_string()])
        .expect("can not find main module");

    let source_map_wrap = SourceMapWrapper { source_map };
    eval(&resolved_main_module, source_map_wrap)
}

fn read_root_source_file(path: &Path) -> Result<String, CliError> {
    debug!("compile {path:?}");
    let swamp_file = resolve_swamp_file(path)?;
    let absolute_path = fs::canonicalize(&swamp_file)?;
    debug!("found_file {absolute_path:?}");

    debug!("reading {}", swamp_file.display());
    let file = fs::read_to_string(&swamp_file)?;
    Ok(file)
}

fn build(path: &PathBuf) -> Result<(), CliError> {
    let source_file_contents = read_root_source_file(path)?;

    let program = compile_to_resolved_program(&source_file_contents)?;
    eprintln!("{program:?}");
    Ok(())
}

fn module_path() -> Vec<String> {
    vec!["main".to_string()]
}

fn run(path: &PathBuf) -> Result<(), CliError> {
    let source_file_contents = read_root_source_file(path)?;

    let value = compile_and_eval(&source_file_contents)?;

    info!("returned: {:?}", value);
    eprintln!("{}", value);

    Ok(())
}
