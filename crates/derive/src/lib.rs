/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput};

fn type_to_swamp(syn_type: &syn::Type) -> String {
    match syn_type {
        syn::Type::Path(type_path) => {
            let type_str = type_path
                .path
                .segments
                .last()
                .map(|s| s.ident.to_string())
                .unwrap_or_default();

            match type_str.as_str() {
                "f32" => "Float".to_string(),
                "i32" => "Int".to_string(),
                "String" => "String".to_string(),
                "bool" => "Bool".to_string(),
                other => other.to_string(),
            }
        }
        _ => "Any".to_string(),
    }
}
#[proc_macro_derive(SwampExport)]
pub fn derive_swamp_export(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    // Extract fields from struct
    let fields = match input.data {
        syn::Data::Struct(ref data) => &data.fields,
        _ => panic!("SwampExport can only be derived for structs"),
    };

    // Generate field conversions for to_swamp_value
    let to_field_conversions = fields.iter().map(|f| {
        let field_name = &f.ident.as_ref().unwrap();
        quote! {
            fields.insert(stringify!(#field_name).to_string(), self.#field_name.to_swamp_value());
        }
    });

    // Generate field extractions for from_swamp_value
    let from_field_extractions = fields.iter().map(|f| {
        let field_name = &f.ident.as_ref().unwrap();
        let field_type = &f.ty;
        quote! {
            let #field_name = fields.get(stringify!(#field_name))
                .ok_or_else(|| format!("Missing {} field", stringify!(#field_name)))?;
            let #field_name = <#field_type>::from_swamp_value(#field_name)?;
        }
    });

    // Collect field names for struct construction
    let field_names = fields
        .iter()
        .map(|f| f.ident.as_ref().unwrap())
        .collect::<Vec<_>>();

    // Generate field definitions for swamp
    let field_defs = fields
        .iter()
        .map(|f| {
            let field_name = &f.ident.as_ref().unwrap();
            let swamp_type = type_to_swamp(&f.ty);
            format!(
                "{}: {}",
                quote!(#field_name).to_string().replace('"', ""),
                swamp_type
            )
        })
        .collect::<Vec<_>>()
        .join(",\n    ");

    let expanded = quote! {
        impl SwampExport for #name {
            fn generate_swamp_definition() -> String {
                format!("struct {} {{\n    {}\n}}\n",
                    stringify!(#name),
                    #field_defs
                )
            }

            fn to_swamp_value(&self) -> Value {
                let mut fields = std::collections::HashMap::new();
                #(#to_field_conversions)*
                Value::Struct(
                    stringify!(#name).to_string(),
                    fields
                )
            }

            fn from_swamp_value(value: &Value) -> Result<Self, String> {
                match value {
                    Value::Struct(type_name, fields) => {
                        if type_name != stringify!(#name) {
                            return Err(format!("Expected {} struct, got {}", stringify!(#name), type_name));
                        }
                        #(#from_field_extractions)*
                        Ok(Self {
                            #(#field_names),*
                        })
                    }
                    _ => Err(format!("Expected {} struct", stringify!(#name)))
                }
            }
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn swamp_fn(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input_fn = parse_macro_input!(item as syn::ItemFn);
    let fn_name = &input_fn.sig.ident;
    // module must have lowercase snake case name
    let module_name = format_ident!("swamp_{}", fn_name.to_string().to_lowercase());

    // Extract return type and convert to swamp type
    let return_type = match &input_fn.sig.output {
        syn::ReturnType::Default => "Unit".to_string(),
        syn::ReturnType::Type(_, ty) => type_to_swamp(ty),
    };

    let args = input_fn
        .sig
        .inputs
        .iter()
        .map(|arg| {
            if let syn::FnArg::Typed(pat_type) = arg {
                let pat = &pat_type.pat;
                let ty = &pat_type.ty;
                (pat, ty)
            } else {
                panic!("self parameters not supported yet") // TODO: support this with swamp script impl
            }
        })
        .collect::<Vec<_>>();

    let arg_count = args.len();
    let arg_indices = 0..arg_count;
    let (patterns, types): (Vec<_>, Vec<_>) = args.iter().cloned().unzip();

    // Generate argument definitions for Swamp
    let arg_defs = args
        .iter()
        .map(|(pat, ty)| {
            format!(
                "{}: {}",
                quote!(#pat).to_string().replace('"', ""),
                type_to_swamp(ty)
            )
        })
        .collect::<Vec<_>>()
        .join(", ");

    let expanded = quote! {
        #input_fn  // Keep the original function

        mod #module_name {
            use super::*;
            use std::sync::LazyLock;

            // TODO: Is there a cleaner way than using LazyLock?
            pub static FUNCTION: LazyLock<SwampFunction> = LazyLock::new(|| SwampFunction {
                name: stringify!(#fn_name),
                handler: Box::new(|args: &[Value]| {
                    if args.len() != #arg_count {
                        return Err(format!(
                            "{} expects {} arguments, got {}",
                            stringify!(#fn_name),
                            #arg_count,
                            args.len()
                        ));
                    }

                    // Convert arguments
                    #(
                        let #patterns = <#types>::from_swamp_value(&args[#arg_indices])?;
                    )*

                    // Call the function
                    let result = super::#fn_name(#(#patterns),*);

                    // Convert result back to Value and wrap in Ok
                    Ok(result.to_swamp_value())
                }),
            });

            pub fn generate_swamp_definition() -> String {
                format!("external fn {}({}) -> {}\n",
                    stringify!(#fn_name),
                    #arg_defs,
                    #return_type
                )
            }
        }
    };

    TokenStream::from(expanded)
}
