#![doc = include_str!("../README.md")]

mod extractors;

use crate::extractors::{extract_function, extract_struct, resolve_path};
use lazy_static::lazy_static;
use proc_macro as pc;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::{fmt, io::Write, sync::Mutex};
use syn::{parenthesized, parse::Parse, spanned::Spanned};

fn s_err(span: proc_macro2::Span, msg: impl fmt::Display) -> syn::Error {
    syn::Error::new(span, msg)
}

lazy_static! {
    static ref ROUTES: Mutex<Vec<Route>> = Mutex::new(Vec::new());
}

/// Adds a route to the router. Use for each api endpoint you want to expose to the frontend.
/// `Inline Functions` are not supported because of rust limitations of inferring types in macros.
#[proc_macro]
pub fn add_route(input: pc::TokenStream) -> pc::TokenStream {
    match add_route_inner(input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn add_route_inner(input: TokenStream) -> syn::Result<TokenStream> {
    let span = input.span();
    let args = syn::parse2::<RouterArgs>(input)?;

    let ident = args.ident;
    let route = args.route;
    let handler = args.handler;

    for MethodCall {
        method,
        r#fn,
        params,
        response,
    } in &handler
    {
        let fn_name = r#fn.segments.last().unwrap().ident.to_string();
        let params: Vec<String> = params
            .iter()
            .map(|param| match param {
                syn::FnArg::Typed(syn::PatType { ty, .. }) => Ok(ty.to_token_stream().to_string()),
                syn::FnArg::Receiver(_) => Err(s_err(span, "Receiver not allowed")),
            })
            .collect::<Result<_, _>>()?;

        ROUTES.lock().unwrap().push(Route {
            route: route.clone(),
            method: method.to_string(),
            fn_name,
            params,
            response: response.clone(),
        });
    }

    Ok(quote! {
        #ident = #ident.route(#route, #(#handler).*);
    })
}

struct RouterArgs {
    ident: syn::Ident,
    route: String,
    handler: Vec<MethodCall>,
}

impl Parse for RouterArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        input.parse::<syn::Token![,]>()?;
        let route = input.parse::<syn::LitStr>()?.value();
        input.parse::<syn::Token![,]>()?;
        let handler = input.parse_terminated(MethodCall::parse, syn::Token![.])?;
        let handler: Vec<MethodCall> = handler.into_iter().collect();

        Ok(RouterArgs {
            ident,
            route,
            handler,
        })
    }
}

struct MethodCall {
    method: syn::Ident,
    r#fn: syn::Path,
    params: Vec<syn::FnArg>,
    response: String,
}

impl Parse for MethodCall {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let method: syn::Ident = input.parse()?;
        let content;
        parenthesized!(content in input);
        let r#fn: syn::Path = content.parse()?;

        let fn_name = r#fn.segments.last().unwrap().ident.to_string();
        let segments = r#fn.segments.iter().map(|s| s.ident.to_string()).collect();

        let file_paths = resolve_path(input.span(), segments)?;

        let (params, response) = extract_function(input.span(), &fn_name, file_paths)?;

        Ok(MethodCall {
            method,
            r#fn,
            params,
            response,
        })
    }
}

impl ToTokens for MethodCall {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let method = &self.method;
        let r#fn = &self.r#fn;

        tokens.extend(quote! {
            #method(#r#fn)
        });
    }
}
struct Route {
    route: String,
    method: String,
    fn_name: String,
    // Should be syn::Type but that's not thread safe
    params: Vec<String>,
    response: String,
}

/// Generates an api ts file from the routes added with `add_route!`. Specify the path to save the api to.
#[proc_macro]
pub fn gen_spec(input: pc::TokenStream) -> pc::TokenStream {
    match gen_spec_inner(input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn gen_spec_inner(input: TokenStream) -> syn::Result<TokenStream> {
    let span = input.span();
    let args = syn::parse2::<GenArgs>(input)?;

    let path = args.path.value();

    let routes = ROUTES.lock().unwrap();

    let mut ts_functions = String::new();
    let mut ts_interfaces = String::new();

    for route in routes.iter() {
        let fn_name = route.method.clone() + "_" + &route.fn_name;
        let method = &route.method;
        let url = &route.route;

        let mut param_names = vec![];
        let mut param_types = vec![];

        for param in &route.params {
            if param.contains("Json") {
                let struct_path = param
                    .split('<')
                    .nth(1)
                    .unwrap()
                    .split('>')
                    .next()
                    .unwrap()
                    .to_string();

                let struct_name = struct_path.split("::").last().unwrap().trim().to_string();

                param_names.push("data".to_string());
                param_types.push(struct_name.to_string());

                let file_paths = resolve_path(
                    span,
                    struct_path.split("::").map(|f| f.to_string()).collect(),
                )?;

                let interface = generate_ts_interface(
                    &struct_name,
                    extract_struct(span, &struct_name, file_paths)?,
                );
                ts_interfaces.push_str(&interface);
            } else {
                let param_name = param.split(':').next().unwrap().trim().to_string();
                let param_type = convert_rust_type_to_ts(param.split(':').nth(1).unwrap().trim());
                param_names.push(param_name.clone());
                param_types.push(param_type);
            }
        }

        let params_str = param_names
            .iter()
            .zip(param_types.iter())
            .map(|(name, ty)| format!("{}: {}", name, ty))
            .collect::<Vec<_>>()
            .join(", ");
        let response_type = convert_rust_type_to_ts(&route.response.clone());

        let body_assignment = if param_names.contains(&"data".to_string()) {
            "JSON.stringify(data)"
        } else {
            "undefined"
        };

        let function = format!(
            r#"export async function {fn_name}({params_str}): Promise<{response_type} | any> {{
    const response = await fetch("{url}", {{
        method: "{method}",
        headers: {{
            "Content-Type": "application/json"
        }},
        body: {body_assignment}
    }});
    return response.json();
}}

"#,
            fn_name = fn_name,
            params_str = params_str,
            response_type = response_type,
            url = url,
            method = method.to_uppercase(),
            body_assignment = body_assignment
        );

        ts_functions.push_str(&function);
    }

    let mut file = std::fs::File::create(path)
        .map_err(|e| s_err(span, format!("Failed to create file: {}", e)))?;
    file.write_all(ts_interfaces.as_bytes())
        .map_err(|e| s_err(span, format!("Failed to write to file: {}", e)))?;
    file.write_all(ts_functions.as_bytes())
        .map_err(|e| s_err(span, format!("Failed to write to file: {}", e)))?;

    Ok(quote! {})
}

fn convert_rust_type_to_ts(rust_type: &str) -> String {
    let rust_type = rust_type.trim();
    match rust_type {
        "str" | "String" => "string".to_string(),
        "usize" | "isize" | "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64" | "f32"
        | "f64" => "number".to_string(),
        "bool" => "boolean".to_string(),
        "()" => "void".to_string(),
        t if t.starts_with("Vec <") => format!("{}[]", convert_rust_type_to_ts(&t[5..t.len() - 1])),
        t if t.starts_with("Option <") => convert_rust_type_to_ts(&t[8..t.len() - 1]),
        t if t.starts_with("Result <") => convert_rust_type_to_ts(&t[8..t.len() - 1]),
        t if t.starts_with("Json <") => convert_rust_type_to_ts(&t[6..t.len() - 1]),
        t if t.starts_with('&') => convert_rust_type_to_ts(&t[1..]),
        t if t.starts_with("'static") => convert_rust_type_to_ts(&t[8..]),
        t => t.to_string(),
    }
}

fn generate_ts_interface(struct_name: &str, fields: Vec<(String, String)>) -> String {
    let mut interface = format!("export interface {} {{\n", struct_name);

    for (field_name, field_type) in fields {
        let field_type = convert_rust_type_to_ts(&field_type);
        interface.push_str(&format!("    {}: {};\n", field_name, field_type));
    }

    interface.push_str("}\n\n");
    interface
}

struct GenArgs {
    path: syn::LitStr,
}

impl Parse for GenArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let path = input.parse()?;
        Ok(GenArgs { path })
    }
}
