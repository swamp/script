/*
 * Copyright (c) Peter Bjorklund. All rights reserved. https://github.com/swamp/script
 * Licensed under the MIT License. See LICENSE in the project root for license information.
 */
use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{DeriveInput, parse_macro_input};

#[proc_macro_derive(SwampExport, attributes(swamp))]
pub fn derive_swamp_export(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    // Extract fields from struct
    let fields = match input.data {
        syn::Data::Struct(ref data) => &data.fields,
        _ => panic!("SwampExport can only be derived for structs"),
    };

    // Generate field extractions for from_swamp_value
    let from_field_extractions = fields.iter().enumerate().map(|(index, f)| {
        let field_name = &f.ident.as_ref().unwrap();
        let field_type = &f.ty;
        quote! {
            let #field_name = <#field_type>::from_swamp_value(&values[#index])?;
        }
    });

    // Collect field names and types for struct construction
    let field_names: Vec<_> = fields.iter().map(|f| f.ident.as_ref().unwrap()).collect();
    let field_types: Vec<_> = fields.iter().map(|f| &f.ty).collect();

    let expanded = quote! {
        impl SwampExport for #name {

            fn get_resolved_type(registry: &TypeRegistry) -> ResolvedType {
                let fields = vec![
                    #((stringify!(#field_names), <#field_types>::get_resolved_type(registry))),*
                ];
                registry.register_derived_struct(stringify!(#name), fields)
            }

            fn to_swamp_value(&self, registry: &TypeRegistry) -> Value {
                let mut values = Vec::new();
                #(values.push(self.#field_names.to_swamp_value(registry));)*

                let resolved_type = Self::get_resolved_type(registry);
                match &resolved_type {
                    ResolvedType::Struct(struct_type) => {
                        Value::Struct(struct_type.clone(), values, resolved_type)
                    },
                    _ => unreachable!("get_resolved_type returned non-struct type")
                }
            }

            fn from_swamp_value(value: &Value) -> Result<Self, String> {
                match value {
                    Value::Struct(struct_type_ref, values, _) => {
                        if struct_type_ref.borrow().name.text != stringify!(#name) {
                            return Err(format!(
                                "Expected {} struct, got {}",
                                stringify!(#name),
                                struct_type_ref.borrow().name.text
                            ));
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
    let module_name = format_ident!("swamp_{}", fn_name.to_string().to_lowercase());

    // Get the context type from the first parameter
    let context_type = match &input_fn.sig.inputs[0] {
        syn::FnArg::Typed(pat_type) => &*pat_type.ty,
        _ => panic!("First parameter must be the context type"),
    };

    // Extract the inner type from &mut MyContext
    let context_inner_type = match context_type {
        syn::Type::Reference(type_ref) => &*type_ref.elem,
        _ => panic!("Context parameter must be a mutable reference"),
    };

    // Extract return type
    let return_type = match &input_fn.sig.output {
        syn::ReturnType::Default => quote!(<()>::get_resolved_type(registry)),
        syn::ReturnType::Type(_, ty) => quote!(<#ty>::get_resolved_type(registry)),
    };

    // Skip the context parameter
    let args = input_fn
        .sig
        .inputs
        .iter()
        .skip(1)
        .map(|arg| {
            if let syn::FnArg::Typed(pat_type) = arg {
                let pat = &pat_type.pat;
                let ty = &pat_type.ty;
                (pat, ty)
            } else {
                panic!("self parameters not supported yet")
            }
        })
        .collect::<Vec<_>>();

    let arg_count = args.len();
    let arg_indices = 0..arg_count;
    let (patterns, types): (Vec<_>, Vec<_>) = args.iter().copied().unzip();

    let expanded = quote! {
        #input_fn  // Keep the original function

        mod #module_name {
            use super::*;
            use swamp_script_core_extra::prelude::*;

            pub struct Function {
                pub name: &'static str,
                pub function_id: ExternalFunctionId,
            }

            impl Function {
                pub fn new(function_id: ExternalFunctionId) -> Self {
                    Self {
                        name: stringify!(#fn_name),
                        function_id,
                    }
                }

                pub fn handler<'a>(
                    &'a self,
                    registry: &'a TypeRegistry,
                ) -> Box<dyn FnMut(&[Value], &mut #context_inner_type) -> Result<Value, ValueError> + 'a> {
                    Box::new(move |args: &[Value], ctx: &mut #context_inner_type| {
                        if args.len() != #arg_count {
                            return Err(ValueError::WrongNumberOfArguments {
                                expected: #arg_count,
                                got: args.len(),
                            });
                        }

                        // Convert arguments
                        #(
                            let #patterns = <#types>::from_swamp_value(&args[#arg_indices])
                                .map_err(|e| ValueError::TypeError(e))?;
                        )*

                        // Call the function with context
                        let result = super::#fn_name(ctx, #(#patterns),*);

                        // Convert result back to Value
                        Ok(result.to_swamp_value(registry))
                    })
                }

                pub fn get_definition(&self, registry: &TypeRegistry) -> ResolvedExternalFunctionDefinition {
                    ResolvedExternalFunctionDefinition {
                        name: LocalIdentifier::from_str(self.name),
                        signature: ResolvedFunctionSignature {
                            parameters: vec![
                                #(ResolvedParameter {
                                    name: stringify!(#patterns).to_string(),
                                    resolved_type: <#types>::get_resolved_type(registry),
                                    ast_parameter: Parameter::default(),
                                    is_mutable: false,
                                },)*
                            ],
                            return_type: #return_type,
                        },
                        id: self.function_id,
                    }
                }
            }
        }
    };

    TokenStream::from(expanded)
}

#[proc_macro_derive(SwampExportEnum, attributes(swamp))]
pub fn derive_swamp_export_enum(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let expanded = match input.data {
        syn::Data::Enum(ref data) => {
            let variant_matches = data.variants.iter().enumerate().map(|(variant_index, variant)| {
                let variant_name = &variant.ident;

                match &variant.fields {
                    syn::Fields::Unit => {
                        quote! {
                            #name::#variant_name => {
                                let variant_type = ResolvedEnumVariantType {
                                    owner: enum_type.clone(),
                                    data: ResolvedEnumVariantContainerType::Nothing,
                                    name: LocalTypeIdentifier::from_str(stringify!(#variant_name)),
                                    number: #variant_index as TypeNumber,
                                };
                                Value::EnumVariantSimple(Rc::new(variant_type))
                            }
                        }
                    }
                    syn::Fields::Named(fields) => {
                        let field_names: Vec<_> = fields.named.iter().map(|f| &f.ident).collect();
                        let field_types: Vec<_> = fields.named.iter().map(|f| &f.ty).collect();

                        let field_type_conversions = field_types.iter().map(|ty| {
                            match quote!(#ty).to_string().as_str() {
                                "f32" => quote! { registry.get_float_type() },
                                "i32" => quote! { registry.get_int_type() },
                                "bool" => quote! { registry.get_bool_type() },
                                "String" => quote! { registry.get_string_type() },
                                ty => quote! { panic!("Unsupported type: {}", #ty) },
                            }
                        });

                        let field_value_conversions = field_names.iter().zip(field_types.iter()).map(|(name, ty)| {
                            match quote!(#ty).to_string().as_str() {
                                "f32" => quote! { Value::Float(Fp::from(*#name)) },
                                "i32" => quote! { Value::Int(*#name) },
                                "bool" => quote! { Value::Bool(*#name) },
                                "String" => quote! { Value::String(#name.clone()) },
                                ty => quote! { panic!("Unsupported type: {}", #ty) },
                            }
                        });

                        quote! {
                            #name::#variant_name { #(ref #field_names),* } => {
                                let mut fields = SeqMap::new();
                                #(
                                    fields.insert(
                                        IdentifierName(stringify!(#field_names).to_string()),
                                        #field_type_conversions
                                    );
                                )*

                                let common = CommonEnumVariantType {
                                    number: #variant_index as TypeNumber,
                                    module_path: ModulePath::new(),
                                    variant_name: LocalTypeIdentifier::from_str(stringify!(#variant_name)),
                                    enum_ref: enum_type.clone(),
                                };

                                let variant_struct = Rc::new(ResolvedEnumVariantStructType {
                                    common,
                                    fields,
                                    ast_struct: AnonymousStruct::default(),
                                });

                                let values = vec![
                                    #(#field_value_conversions),*
                                ];

                                Value::EnumVariantStruct(variant_struct, values)
                            }
                        }
                    }


                    syn::Fields::Unnamed(fields) => {
                        let field_types: Vec<_> = fields.unnamed.iter().map(|f| &f.ty).collect();
                        let field_names: Vec<_> = (0..field_types.len())
                            .map(|i| format_ident!("field_{}", i))
                            .collect::<Vec<_>>();

                        let field_type_conversions = field_types.iter().map(|ty| {
                            match quote!(#ty).to_string().as_str() {
                                "f32" => quote! { registry.get_float_type() },
                                "i32" => quote! { registry.get_int_type() },
                                "bool" => quote! { registry.get_bool_type() },
                                "String" => quote! { registry.get_string_type() },
                                ty => quote! { panic!("Unsupported type: {}", #ty) },
                            }
                        });

                        let field_value_conversions = field_names.iter().zip(field_types.iter()).map(|(name, ty)| {
                            match quote!(#ty).to_string().as_str() {
                                "f32" => quote! { Value::Float(Fp::from(*#name)) },
                                "i32" => quote! { Value::Int(*#name) },
                                "bool" => quote! { Value::Bool(*#name) },
                                "String" => quote! { Value::String(#name.clone()) },
                                ty => quote! { panic!("Unsupported type: {}", #ty) },
                            }
                        });

                        quote! {
                            #name::#variant_name(#(ref #field_names),*) => {
                                let fields_in_order = vec![
                                    #(#field_type_conversions),*
                                ];

                                let common = CommonEnumVariantType {
                                    number: #variant_index as TypeNumber,
                                    module_path: ModulePath::new(),
                                    variant_name: LocalTypeIdentifier::from_str(stringify!(#variant_name)),
                                    enum_ref: enum_type.clone(),
                                };

                                let variant_tuple = Rc::new(ResolvedEnumVariantTupleType {
                                    common,
                                    fields_in_order,
                                });

                                let values = vec![
                                    #(#field_value_conversions),*
                                ];

                                Value::EnumVariantTuple(variant_tuple, values)
                            }
                        }
                    }
                }
            });

            quote! {
                impl SwampExport for #name {
                    fn get_resolved_type(registry: &TypeRegistry) -> ResolvedType {
                        let enum_type = Rc::new(ResolvedEnumType {
                            name: LocalTypeIdentifier::from_str(stringify!(#name)),
                            number: registry.allocate_type_number(),
                            module_path: ModulePath(vec![]),
                        });
                        ResolvedType::Enum(enum_type)
                    }

                    fn to_swamp_value(&self, registry: &TypeRegistry) -> Value {
                        let enum_type = match Self::get_resolved_type(registry) {
                            ResolvedType::Enum(t) => t,
                            _ => unreachable!(),
                        };

                        match self {
                            #(#variant_matches),*
                        }
                    }

                    fn from_swamp_value(value: &Value) -> Result<Self, String> {
                        match value {
                            Value::EnumVariantSimple(_) |
                            Value::EnumVariantTuple(_, _) |
                            Value::EnumVariantStruct(_, _) => {
                                todo!("Implement from_swamp_value for enums") // TODO: PBJ: Fix this when needed
                            }
                            _ => Err(format!("Expected enum variant, got {:?}", value))
                        }
                    }
                }
            }
        }
        _ => panic!("SwampExportEnum can only be derived for enums"),
    };

    TokenStream::from(expanded)
}
