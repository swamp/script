/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use clap::{Parser, Subcommand};
use std::cell::RefCell;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::io;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use swamp_script_analyzer::prelude::ResolveError;
use swamp_script_analyzer::Program;
use swamp_script_compile::{compile_analyze_and_link_without_version, compile_and_analyze};
use swamp_script_core::prelude::SeqMap;
use swamp_script_dep_loader::{create_source_map, DepLoaderError};
use swamp_script_error_report::{show_script_resolve_error, ScriptResolveError};
use swamp_script_eval::err::ExecuteError;
use swamp_script_parser::prelude::*;
use swamp_script_pretty_print::ModulesDisplay;
use swamp_script_semantic::ns::{ClosureTypeGenerator, ModuleNamespace};
use swamp_script_semantic::prelude::Modules;
use swamp_script_semantic::{
    AnonymousStructType, ExternalFunctionDefinition, ExternalFunctionDefinitionRef, Function,
    FunctionTypeSignature, IteratorTypeDetails, IteratorYieldType, Node, StructType, Type,
    TypeForParameter,
};
use swamp_script_source_map_lookup::SourceMapWrapper;
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

fn main() -> Result<(), Box<dyn Error>> {
    init_logging();
    let cli = Cli::parse();

    /*
    match &cli.command {
        Commands::Build { path, module } => {
            println!("Building swamp script at path: {}", path.display());
            // Call your build function here, passing the path
            if let Err(e) = build(path, module) {
                eprintln!("Error during build: {}", e);
            }
        }
        Commands::Run { path } => {
            println!("Running swamp script at path: {}", path.display());
            if let Err(e) = run(path) {
                eprintln!("Error during run: {}", e);
            }
        }
    }

     */

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
    ResolveError(ResolveError),
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

impl From<ScriptResolveError> for CliError {
    fn from(value: ScriptResolveError) -> Self {
        Self::ScriptResolveError(value)
    }
}

pub struct CliContext;

#[allow(clippy::too_many_lines)]
fn build(root_path: &Path, root_module: &str) -> Result<(), CliError> {
    let mut source_map = create_source_map(root_path)?;
    let mut resolved_program = Program::new();

    // std::
    let mangrove_std_module_path = &["mangrove-0.0.0".to_string(), "std".to_string()];
    compile_analyze_and_link_without_version(
        mangrove_std_module_path,
        &mut resolved_program,
        &mut source_map,
    )?;

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
        let mangrove_collection_module_path_without_version =
            &["mangrove".to_string(), "collection".to_string()];
        let mangrove_collection_module = resolved_program
            .modules
            .get(mangrove_collection_module_path_without_version)
            .unwrap();

        let md = mangrove_collection_module.borrow_mut();
        let mut ns = md.namespace.borrow_mut();

        let closure_gen = ClosureTypeGenerator::new(
            |ns: &mut ModuleNamespace, modules: &Modules, params: &[Type]| {
                let concretized_struct_name_in_namespace = format!("Sparse<{}>", params[0]);
                if let Some(found_concrete_struct_type) =
                    ns.get_struct(&concretized_struct_name_in_namespace)
                {
                    return Ok(Type::Struct(found_concrete_struct_type));
                }

                let value_type = &params[0];
                let optional_value_type = Type::Optional(Box::from(value_type.clone()));

                let create_struct = StructType {
                    name: Node::default(),
                    assigned_name: concretized_struct_name_in_namespace,
                    anon_struct_type: AnonymousStructType {
                        defined_fields: SeqMap::default(),
                    },
                    functions: SeqMap::default(),
                };

                let create_struct_ref = Rc::new(RefCell::new(create_struct));

                let mut functions = SeqMap::new();

                // ::new()
                let external_def_new_fn = ExternalFunctionDefinition {
                    name: None,
                    assigned_name: "new".to_string(),
                    signature: FunctionTypeSignature {
                        parameters: vec![],
                        return_type: Box::new(Type::Struct(create_struct_ref.clone())),
                    },
                    id: 0,
                };
                let external_func = Rc::new(Function::External(
                    ExternalFunctionDefinitionRef::from(external_def_new_fn),
                ));
                functions.insert("new".to_string(), external_func).unwrap();

                let collection_module = modules
                    .get(&["mangrove".into(), "collection".into()])
                    .unwrap();
                let sparse_id_struct_type = collection_module
                    .borrow()
                    .namespace
                    .borrow()
                    .get_struct("SparseId")
                    .unwrap();
                let sparse_id_type = Type::Struct(sparse_id_struct_type);

                // ::iter()
                let external_iter_fn = ExternalFunctionDefinition {
                    name: None,
                    assigned_name: "iter".to_string(),
                    signature: FunctionTypeSignature {
                        parameters: vec![],
                        return_type: Box::new(Type::Iterator(Box::from(IteratorTypeDetails {
                            yield_type: IteratorYieldType::KeyValue(
                                sparse_id_type.clone(),
                                value_type.clone(),
                            ),
                        }))),
                    },
                    id: 0,
                };
                let external_iter_func = Rc::new(Function::External(
                    ExternalFunctionDefinitionRef::from(external_iter_fn),
                ));
                functions
                    .insert("iter".to_string(), external_iter_func)
                    .unwrap();

                // ::subscript()
                let external_subscript_mut_fn = ExternalFunctionDefinition {
                    name: None,
                    assigned_name: "subscript_mut".to_string(),
                    signature: FunctionTypeSignature {
                        parameters: vec![
                            TypeForParameter {
                                name: "self".to_string(),
                                resolved_type: Type::Struct(create_struct_ref.clone()),
                                is_mutable: true,
                                node: None,
                            },
                            TypeForParameter {
                                name: "index".to_string(),
                                resolved_type: Type::Int,
                                is_mutable: false,
                                node: None,
                            },
                            TypeForParameter {
                                name: "out".to_string(),
                                resolved_type: optional_value_type.clone(),
                                is_mutable: true,
                                node: None,
                            },
                        ],
                        return_type: Box::from(Type::Unit),
                    },
                    id: 0,
                };
                let external_subscript_mut_func = Rc::new(Function::External(
                    ExternalFunctionDefinitionRef::from(external_subscript_mut_fn),
                ));
                functions
                    .insert("subscript_mut".to_string(), external_subscript_mut_func)
                    .unwrap();

                // ::subscript()
                let external_subscript_fn = ExternalFunctionDefinition {
                    name: None,
                    assigned_name: "subscript".to_string(),
                    signature: FunctionTypeSignature {
                        parameters: vec![
                            TypeForParameter {
                                name: "self".to_string(),
                                resolved_type: Type::Struct(create_struct_ref.clone()),
                                is_mutable: true,
                                node: None,
                            },
                            TypeForParameter {
                                name: "index".to_string(),
                                resolved_type: Type::Int,
                                is_mutable: false,
                                node: None,
                            },
                        ],
                        return_type: Box::from(optional_value_type.clone()),
                    },
                    id: 0,
                };
                let external_subscript_func = Rc::new(Function::External(
                    ExternalFunctionDefinitionRef::from(external_subscript_fn),
                ));
                functions
                    .insert("subscript".to_string(), external_subscript_func)
                    .unwrap();

                // ::remove()
                let remove_fn_def = ExternalFunctionDefinition {
                    name: None,
                    assigned_name: "remove".to_string(),
                    signature: FunctionTypeSignature {
                        parameters: vec![
                            TypeForParameter {
                                name: "self".to_string(),
                                resolved_type: Type::Struct(create_struct_ref.clone()),
                                is_mutable: true,
                                node: None,
                            },
                            TypeForParameter {
                                name: "id".to_string(),
                                resolved_type: sparse_id_type.clone(),
                                is_mutable: false,
                                node: None,
                            },
                        ],
                        return_type: Box::from(optional_value_type),
                    },
                    id: 0,
                };
                let remove_fn_def_ref = Rc::new(Function::External(
                    ExternalFunctionDefinitionRef::from(remove_fn_def),
                ));
                functions
                    .insert("remove".to_string(), remove_fn_def_ref)
                    .unwrap();

                // ::add()
                let add_fn_def = ExternalFunctionDefinition {
                    name: None,
                    assigned_name: "add".to_string(),
                    signature: FunctionTypeSignature {
                        parameters: vec![
                            TypeForParameter {
                                name: "self".to_string(),
                                resolved_type: Type::Struct(create_struct_ref.clone()),
                                is_mutable: true,
                                node: None,
                            },
                            TypeForParameter {
                                name: "value".to_string(),
                                resolved_type: value_type.clone(),
                                is_mutable: false,
                                node: None,
                            },
                        ],
                        return_type: Box::from(sparse_id_type),
                    },
                    id: 0,
                };
                let add_fn_def_ref = Rc::new(Function::External(
                    ExternalFunctionDefinitionRef::from(add_fn_def),
                ));
                functions.insert("add".to_string(), add_fn_def_ref).unwrap();

                // ------------------

                create_struct_ref.borrow_mut().functions = functions;

                Ok(Type::Struct(create_struct_ref))
            },
        );

        ns.add_generator("Sparse", Rc::new(closure_gen))
            .expect("TODO: panic message");
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

    Ok(())
}

fn run(_path: &Path) -> Result<(), CliError> {
    Ok(())
}
