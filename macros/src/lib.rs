use proc_macro as pc;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::{collections::HashMap, fmt};
use syn::{parenthesized, parse::Parse, spanned::Spanned};

fn s_err(span: proc_macro2::Span, msg: impl fmt::Display) -> syn::Error {
    syn::Error::new(span, msg)
}

/// Used to extract the metadata of functions and structs. Use inside the `Api::route` function.
#[proc_macro]
pub fn extract(input: pc::TokenStream) -> pc::TokenStream {
    match extract_inner(input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn extract_inner(input: TokenStream) -> syn::Result<TokenStream> {
    let ExtractArgs { routes } = syn::parse2::<ExtractArgs>(input.clone())?;
    let original_input = input;

    let routes = routes.iter().map(|Route { method, handler }| {
        let method_name = method.to_string();
        let handler_name = handler.to_string();

        let fn_info = syn::Ident::new(
            &format!("FN_{}", handler_name.to_uppercase()),
            proc_macro2::Span::call_site(),
        );

        quote! {
            gluer::Route {
                url: "",
                method: #method_name,
                fn_name: #handler_name,
                fn_info: #fn_info,
            }
        }
    });

    Ok(quote! { ( #original_input, &[#(#routes,)*] )})
}

struct ExtractArgs {
    routes: Vec<Route>,
}

impl Parse for ExtractArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut routes = vec![];

        while !input.is_empty() {
            let route = input.parse()?;
            routes.push(route);

            if !input.is_empty() {
                input.parse::<syn::Token!(.)>()?;
            }
        }

        Ok(ExtractArgs { routes })
    }
}

struct Route {
    method: syn::Ident,
    handler: syn::Ident,
}

impl Parse for Route {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let method = input.parse()?;
        let content;
        parenthesized!(content in input);
        let handler = content.parse()?;

        Ok(Route { method, handler })
    }
}

/// Put before structs or functions to be usable by the `glue` crate.
#[proc_macro_attribute]
pub fn metadata(args: pc::TokenStream, input: pc::TokenStream) -> pc::TokenStream {
    match metadata_inner(args.into(), input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn metadata_inner(args: TokenStream, input: TokenStream) -> syn::Result<TokenStream> {
    let span = input.span();
    let item = syn::parse2::<syn::Item>(input.clone())?;
    let _args = syn::parse2::<NoArgs>(args)?;

    let generated_const = match item.clone() {
        syn::Item::Struct(item_struct) => generate_struct_const(item_struct)?,
        syn::Item::Fn(item_fn) => generate_fn_const(item_fn)?,
        _ => return Err(s_err(span, "Expected struct or function")),
    };

    Ok(quote! {
        #generated_const
        #item
    })
}

fn generate_struct_const(item_struct: syn::ItemStruct) -> syn::Result<TokenStream> {
    let struct_name = item_struct.ident.to_string();
    let vis = &item_struct.vis;
    let fields = item_struct
        .fields
        .into_iter()
        .map(|field| {
            let ident = field
                .ident
                .clone()
                .ok_or_else(|| s_err(field.span(), "Unnamed field not supported"))?
                .to_string();
            let ty = field.ty.into_token_stream().to_string();
            Ok((ident, ty))
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let const_ident = syn::Ident::new(
        &format!("STRUCT_{}", struct_name.to_uppercase()),
        proc_macro2::Span::call_site(),
    );

    let const_value = fields.iter().map(|(ident, ty)| {
        quote! { gluer::Field { name: #ident, ty: #ty } }
    });

    Ok(quote! {
        #vis const #const_ident: gluer::StructInfo = gluer::StructInfo { name: #struct_name, fields: &[#(#const_value),*] };
    })
}

fn generate_fn_const(item_fn: syn::ItemFn) -> syn::Result<TokenStream> {
    let fn_name = item_fn.sig.ident.to_string();
    let vis = &item_fn.vis;
    let mut structs = HashMap::new();

    let params = item_fn
        .sig
        .inputs
        .iter()
        .filter_map(|param| match param {
            syn::FnArg::Typed(syn::PatType { pat, ty, .. }) => {
                let pat = pat.to_token_stream().to_string();
                if let Some((is_basic, outer_ty, inner_ty)) = basic_rust_type(ty).ok()? {
                    if !is_basic {
                        let struct_const = format!("STRUCT_{}", inner_ty.to_uppercase());
                        structs.insert(inner_ty.clone(), struct_const);
                    }
                    Some(Ok((pat, outer_ty)))
                } else {
                    None
                }
            }
            syn::FnArg::Receiver(_) => {
                Some(Err(s_err(param.span(), "Receiver parameter not allowed")))
            }
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let response = match &item_fn.sig.output {
        syn::ReturnType::Type(_, ty) => {
            if let Some((is_basic, outer_ty, inner_ty)) = basic_rust_type(ty)? {
                if !is_basic {
                    let struct_const = format!("STRUCT_{}", inner_ty.to_uppercase());
                    structs.insert(inner_ty.clone(), struct_const);
                }
                outer_ty
            } else {
                return Err(s_err(ty.span(), "Unsupported return type"));
            }
        }
        syn::ReturnType::Default => "()".to_string(),
    };

    let const_ident = syn::Ident::new(
        &format!("FN_{}", fn_name.to_uppercase()),
        proc_macro2::Span::call_site(),
    );

    let const_value = params.iter().map(|(pat, ty)| {
        quote! { gluer::Field { name: #pat, ty: #ty } }
    });

    let structs_quote = structs.iter().map(|(_ty_str, struct_name)| {
        let struct_ident = syn::Ident::new(struct_name, proc_macro2::Span::call_site());
        quote! { #struct_ident }
    });

    Ok(quote! {
        #vis const #const_ident: gluer::FnInfo = gluer::FnInfo {
            params: &[#(#const_value),*],
            response: #response,
            structs: &[#(#structs_quote),*]
        };
    })
}

struct NoArgs {}

impl syn::parse::Parse for NoArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if !input.is_empty() {
            return Err(input.error("No arguments expected"));
        }
        Ok(NoArgs {})
    }
}

/// Returns a tuple (bool, outermost_type, innermost_type)
fn basic_rust_type(ty: &syn::Type) -> syn::Result<Option<(bool, String, String)>> {
    let ty_str = ty.to_token_stream().to_string();
    match ty {
        syn::Type::Path(syn::TypePath { path, .. }) => {
            if let Some(segment) = path.segments.last() {
                let ty_name = segment.ident.to_string();

                // Skip types like State<...> and more, see the `extract` section in axum's docs
                if matches!(
                    ty_name.as_ref(),
                    "State" | "Headers" | "Bytes" | "Request" | "Extension"
                ) {
                    return Ok(None);
                }

                match &segment.arguments {
                    syn::PathArguments::None => {
                        let is_basic = matches!(
                            ty_name.as_str(),
                            "bool"
                                | "char"
                                | "str"
                                | "u8"
                                | "u16"
                                | "u32"
                                | "u64"
                                | "u128"
                                | "i8"
                                | "i16"
                                | "i32"
                                | "i64"
                                | "i128"
                                | "usize"
                                | "isize"
                                | "f32"
                                | "f64"
                                | "String"
                                | "Json"
                        );
                        return Ok(Some((is_basic, ty_name.clone(), ty_name)));
                    }
                    syn::PathArguments::AngleBracketed(ref args) => {
                        let mut outer_type = ty_name.clone();
                        let mut innermost_type = String::new();
                        let mut is_basic = false;

                        for arg in &args.args {
                            if let syn::GenericArgument::Type(ref inner_ty) = arg {
                                if let Ok(Some((inner_is_basic, outer_most, inner_innermost))) =
                                    basic_rust_type(inner_ty)
                                {
                                    outer_type = format!("{}<{}>", ty_name, outer_most);
                                    innermost_type = inner_innermost;
                                    is_basic = inner_is_basic;
                                } else {
                                    return Ok(None);
                                }
                            }
                        }

                        return Ok(Some((is_basic, outer_type, innermost_type)));
                    }
                    _ => {}
                }
            }
        }
        syn::Type::Reference(syn::TypeReference { elem, .. }) => return basic_rust_type(&elem),
        _ => {}
    }
    Err(s_err(
        proc_macro2::Span::call_site(),
        format!("Failed to parse type {}", ty_str),
    ))
}
