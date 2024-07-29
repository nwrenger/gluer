#![doc = include_str!("../README.md")]

use once_cell::sync::Lazy;
use proc_macro as pc;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::{collections::HashMap, fmt, io::Write, sync::RwLock};
use syn::{parenthesized, parse::Parse, spanned::Spanned};

fn s_err(span: proc_macro2::Span, msg: impl fmt::Display) -> syn::Error {
    syn::Error::new(span, msg)
}

struct Route {
    route: String,
    method: String,
    fn_name: String,
}

struct Function {
    params: HashMap<String, String>,
    response: String,
}

#[derive(Clone)]
struct StructField {
    ident: String,
    ty: String,
}

static ROUTES: Lazy<RwLock<Vec<Route>>> = Lazy::new(|| RwLock::new(Vec::new()));
static STRUCTS: Lazy<RwLock<HashMap<String, Vec<StructField>>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));
static FUNCTIONS: Lazy<RwLock<HashMap<String, Function>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

/// Adds a route to the router. Use for each api endpoint you want to expose to the frontend.
/// `Inline Functions` are currently not supported.
#[proc_macro]
pub fn add_route(input: pc::TokenStream) -> pc::TokenStream {
    match add_route_inner(input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn add_route_inner(input: TokenStream) -> syn::Result<TokenStream> {
    let args = syn::parse2::<RouterArgs>(input)?;

    let ident = args.ident;
    let route = args.route;
    let handler = args.handler;

    for MethodCall { method, r#fn } in &handler {
        let fn_name = r#fn.segments.last().unwrap().ident.to_string();

        ROUTES.write().unwrap().push(Route {
            route: route.clone(),
            method: method.to_string(),
            fn_name,
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
}

impl Parse for MethodCall {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let method: syn::Ident = input.parse()?;
        let content;
        parenthesized!(content in input);
        let r#fn: syn::Path = content.parse()?;

        Ok(MethodCall { method, r#fn })
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

/// Generates an api ts file from the routes added with `add_route!`. Specify the path to save the api to.
#[proc_macro]
pub fn gen_ts(input: pc::TokenStream) -> pc::TokenStream {
    match gen_ts_inner(input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn gen_ts_inner(input: TokenStream) -> syn::Result<TokenStream> {
    let span = input.span();
    let args = syn::parse2::<GenArgs>(input)?;
    let path = args.path.value();

    let routes = ROUTES.read().unwrap();
    let functions = FUNCTIONS.read().unwrap();
    let structs = STRUCTS.read().unwrap();

    let mut ts_functions = HashMap::new();
    let mut ts_interfaces = HashMap::new();

    for route in routes.iter() {
        let fn_name = &route.fn_name;
        let method = &route.method;
        let url = &route.route;

        let function = functions.get(fn_name).ok_or_else(|| {
            s_err(
                span,
                format!(
                    "Function '{}' not found in the cache, mind adding it with fun! {{}}",
                    fn_name
                ),
            )
        })?;

        let ty = collect_params(function, &structs, span, &mut ts_interfaces)?;

        let response_type =
            collect_response_type(&function.response, &structs, span, &mut ts_interfaces)?;

        let params_str = if !ty.is_empty() {
            format!("params: {}", ty)
        } else {
            String::new()
        };

        let body_assignment = if !ty.is_empty() {
            "JSON.stringify(params)"
        } else {
            "undefined"
        };

        let function_str = format!(
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

        ts_functions.insert(fn_name.to_owned(), function_str);
    }

    write_to_file(path, ts_interfaces, ts_functions, span)?;

    Ok(quote! {})
}

fn collect_params(
    function: &Function,
    structs: &HashMap<String, Vec<StructField>>,
    span: proc_macro2::Span,
    ts_interfaces: &mut HashMap<String, String>,
) -> syn::Result<String> {
    for param in &function.params {
        if param.1.contains("Json") {
            let struct_name = extract_struct_name(param.1)?;
            if let Some(fields) = structs.get(&struct_name).cloned() {
                ts_interfaces
                    .entry(struct_name.clone())
                    .or_insert_with(|| generate_ts_interface(&struct_name.clone(), fields));
                return Ok(struct_name);
            } else {
                let interface = convert_rust_type_to_ts(&struct_name);
                if let Some(_interface) = interface {
                    return Ok(struct_name);
                } else {
                    return Err(s_err(
                        span,
                        format!(
                            "Struct '{}' not found in the cache, mind adding it with #[param]",
                            struct_name
                        ),
                    ));
                }
            }
        }
    }
    Ok(String::new())
}

fn collect_response_type(
    response: &str,
    structs: &HashMap<String, Vec<StructField>>,
    span: proc_macro2::Span,
    ts_interfaces: &mut HashMap<String, String>,
) -> syn::Result<String> {
    let response = response.replace(" ", "");
    if let Some(response_type) = convert_rust_type_to_ts(&response) {
        return Ok(response_type);
    }

    if response.contains("Json") {
        let struct_name = extract_struct_name(&response)?;
        if let Some(fields) = structs.get(&struct_name).cloned() {
            ts_interfaces
                .entry(struct_name.clone())
                .or_insert_with(|| generate_ts_interface(&struct_name, fields));
            return Ok(struct_name);
        }
    }

    Err(s_err(
        span,
        format!(
            "Struct '{}' not found in the cache, mind adding it with #[param]",
            response
        ),
    ))
}

fn extract_struct_name(type_str: &str) -> syn::Result<String> {
    type_str
        .split('<')
        .nth(1)
        .and_then(|s| s.split('>').next())
        .map(|s| s.split("::").last().unwrap().trim().to_string())
        .ok_or_else(|| {
            s_err(
                proc_macro2::Span::call_site(),
                format!("Failed to extract struct name from '{}'", type_str),
            )
        })
}

fn write_to_file(
    path: String,
    ts_interfaces: HashMap<String, String>,
    ts_functions: HashMap<String, String>,
    span: proc_macro2::Span,
) -> syn::Result<()> {
    let mut file = std::fs::File::create(path)
        .map_err(|e| s_err(span, format!("Failed to create file: {}", e)))?;

    for interface in ts_interfaces.values() {
        file.write_all(interface.as_bytes())
            .map_err(|e| s_err(span, format!("Failed to write to file: {}", e)))?;
    }

    for function in ts_functions.values() {
        file.write_all(function.as_bytes())
            .map_err(|e| s_err(span, format!("Failed to write to file: {}", e)))?;
    }

    Ok(())
}

fn convert_rust_type_to_ts(rust_type: &str) -> Option<String> {
    let rust_type = rust_type.trim();
    Some(match rust_type {
        "str" | "String" => "string".to_string(),
        "usize" | "isize" | "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64" | "f32"
        | "f64" => "number".to_string(),
        "bool" => "boolean".to_string(),
        "()" => "void".to_string(),
        t if t.starts_with("Vec<") => format!(
            "{}[]",
            convert_rust_type_to_ts(&t[4..t.len() - 1]).unwrap_or_default()
        ),
        t if t.starts_with("Option<") => return convert_rust_type_to_ts(&t[7..t.len() - 1]),
        t if t.starts_with("Result<") => return convert_rust_type_to_ts(&t[7..t.len() - 1]),
        t if t.starts_with("Json<") => return convert_rust_type_to_ts(&t[5..t.len() - 1]),
        t if t.starts_with('&') => return convert_rust_type_to_ts(&t[1..]),
        t if t.starts_with("'static") => return convert_rust_type_to_ts(&t[8..]),
        _ => return None,
    })
}

fn generate_ts_interface(struct_name: &str, fields: Vec<StructField>) -> String {
    let mut interface = format!("export interface {} {{\n", struct_name);
    for StructField { ident, ty } in fields {
        let ty = convert_rust_type_to_ts(&ty).unwrap_or_default();
        interface.push_str(&format!("    {}: {};\n", ident, ty));
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

/// Put here inside the functions which should be used by the api.
#[proc_macro]
pub fn fun(input: pc::TokenStream) -> pc::TokenStream {
    match fun_inner(input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn fun_inner(input: TokenStream) -> syn::Result<TokenStream> {
    let span = input.span();
    let items = syn::parse2::<syn::File>(input.clone())?;

    for item in items.items {
        if let syn::Item::Fn(item_fn) = item {
            let fn_name = item_fn.sig.ident.to_string();
            let params = item_fn.sig.inputs.clone();
            let response = match item_fn.sig.output.clone() {
                syn::ReturnType::Type(_, ty) => ty.into_token_stream().to_string(),
                _ => "()".to_string(),
            };

            FUNCTIONS.write().unwrap().insert(
                fn_name.clone(),
                Function {
                    params: {
                        let mut map = HashMap::new();
                        for param in params {
                            match param {
                                syn::FnArg::Typed(syn::PatType { ty, pat, .. }) => {
                                    if pat.to_token_stream().to_string() == "Json" {
                                        let struct_path = ty.to_token_stream().to_string();
                                        let struct_name = struct_path.split("::").last().unwrap().trim();
                                        let fields = STRUCTS
                                            .read().unwrap()
                                            .get(&struct_name.to_string())
                                            .ok_or_else(|| {
                                                s_err(
                                                    span,
                                                    format!(
                                                        "Struct '{}' not found in the cache, mind adding it with #[param]",
                                                        struct_name
                                                    ),
                                                )
                                            })?
                                            .clone();

                                        for StructField { ident, ty } in fields {
                                            map.insert(ident, ty);
                                        }
                                    } else {
                                        let ty = ty.to_token_stream().to_string();
                                        map.insert(pat.to_token_stream().to_string(), ty);
                                    }
                                }
                                syn::FnArg::Receiver(_) => {
                                    return Err(s_err(span, "Receiver not allowed"));
                                }
                            }
                        }
                        map
                    },
                    response,
                },
            );
        } else {
            return Err(s_err(span, "Expected function item"));
        }
    }

    Ok(quote! {#input})
}

/// Put before the structs which should be used by the api.
#[proc_macro_attribute]
pub fn param(args: pc::TokenStream, input: pc::TokenStream) -> pc::TokenStream {
    match param_inner(args.into(), input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn param_inner(args: TokenStream, input: TokenStream) -> syn::Result<TokenStream> {
    let input = syn::parse2::<syn::ItemStruct>(input)?;
    let _args = syn::parse2::<NoArgs>(args)?;

    STRUCTS.write().unwrap().insert(input.ident.to_string(), {
        let mut field_vec = Vec::new();

        if let syn::Fields::Named(fields) = input.fields.clone() {
            for field in fields.named {
                let ident = field.ident.unwrap().to_string();
                let ty = field.ty.into_token_stream().to_string();
                field_vec.push(StructField { ident, ty });
            }
        }

        field_vec
    });

    Ok(quote! {#input})
}

struct NoArgs {}

impl Parse for NoArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if !input.is_empty() {
            return Err(input.error("No arguments expected"));
        }
        Ok(NoArgs {})
    }
}
