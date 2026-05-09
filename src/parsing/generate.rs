use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::{
    collections::HashMap,
    fmt::{self, Debug},
    fs::{self},
    vec,
};
use syn::{
    braced, bracketed, parenthesized,
    parse::Parse,
    punctuated::Punctuated,
    token::{Brace, Comma},
    Item, LitStr, Token,
};

use crate::{
    codegen,
    framework::Framework,
    parsing::{
        metadata::MetadataAttr,
        rust::{FnInfo, TypeCategory, TypeInfo},
        s_err,
    },
};

pub fn inner(input: TokenStream) -> syn::Result<TokenStream> {
    let GenerateArgs {
        prefix,
        routes,
        files,
        output,
    } = syn::parse2(input.clone())?;

    let files = files
        .iter()
        .map(|s: &String| std::path::PathBuf::from(s))
        .collect::<Vec<_>>();

    let parsed_routes: Vec<Route> = routes.clone().iter().flat_map(|f| f.to_routes()).collect();

    let mut fn_infos = HashMap::new();
    let mut type_infos = HashMap::new();

    fn process_paths(
        files: &[std::path::PathBuf],
        fn_infos: &mut HashMap<String, FnInfo>,
        type_infos: &mut HashMap<String, TypeCategory>,
    ) -> syn::Result<()> {
        for path in files {
            if path.is_dir() {
                process_entries(path, fn_infos, type_infos)?;
            } else if path.extension().and_then(|s: &std::ffi::OsStr| s.to_str()) == Some("rs") {
                process_file(path, fn_infos, type_infos)?;
            } else {
                return Err(s_err(
                    proc_macro2::Span::call_site(),
                    format!(
                        "Path '{}' is not a directory or a Rust file",
                        path.display()
                    ),
                ));
            }
        }
        Ok(())
    }

    fn process_entries(
        dir: &std::path::Path,
        fn_infos: &mut HashMap<String, FnInfo>,
        type_infos: &mut HashMap<String, TypeCategory>,
    ) -> syn::Result<()> {
        for entry in fs::read_dir(dir).map_err(|e| {
            s_err(
                proc_macro2::Span::call_site(),
                format!("Couldn't read entire dir: {}", e),
            )
        })? {
            let entry = entry.map_err(|e| {
                s_err(
                    proc_macro2::Span::call_site(),
                    format!("Couldn't read dir entry: {}", e),
                )
            })?;
            let path = entry.path();
            if path.is_dir() {
                process_entries(&path, fn_infos, type_infos)?;
            } else if path.extension().and_then(|s| s.to_str()) == Some("rs") {
                process_file(&path, fn_infos, type_infos)?;
            }
        }
        Ok(())
    }

    fn process_file(
        path: &std::path::Path,
        fn_infos: &mut HashMap<String, FnInfo>,
        type_infos: &mut HashMap<String, TypeCategory>,
    ) -> syn::Result<()> {
        let content = fs::read_to_string(path).map_err(|e| {
            s_err(
                proc_macro2::Span::call_site(),
                format!("Couldn't read file to string: {}", e),
            )
        })?;
        let syntax = syn::parse_file(&content)?;
        process_syntax(&syntax.items, fn_infos, type_infos)?;
        Ok(())
    }

    fn process_syntax(
        syntax: &Vec<Item>,
        fn_infos: &mut HashMap<String, FnInfo>,
        type_infos: &mut HashMap<String, TypeCategory>,
    ) -> syn::Result<()> {
        for item in syntax {
            match item {
                Item::Enum(item_enum) => {
                    for attr in &item_enum.attrs {
                        if attr.path().is_ident("metadata") {
                            let metadata_attr = attr
                                .parse_args::<MetadataAttr>()
                                .unwrap_or(MetadataAttr { custom: vec![] });
                            let enum_type =
                                TypeInfo::from_enum_tokens(item_enum.clone(), metadata_attr)?;
                            if !type_infos.contains_key(&enum_type.name) {
                                type_infos
                                    .insert(enum_type.name.clone(), TypeCategory::Enum(enum_type));
                            }
                        }
                    }
                }
                Item::Struct(item_struct) => {
                    for attr in &item_struct.attrs {
                        if attr.path().is_ident("metadata") {
                            let metadata_attr = attr
                                .parse_args::<MetadataAttr>()
                                .unwrap_or(MetadataAttr { custom: vec![] });
                            let struct_type =
                                TypeInfo::from_struct_tokens(item_struct.clone(), metadata_attr)?;
                            if !type_infos.contains_key(&struct_type.name) {
                                type_infos.insert(
                                    struct_type.name.clone(),
                                    TypeCategory::Struct(struct_type),
                                );
                            }
                        }
                    }
                }
                Item::Type(item_type) => {
                    for attr in &item_type.attrs {
                        if attr.path().is_ident("metadata") {
                            let metadata_attr = attr
                                .parse_args::<MetadataAttr>()
                                .unwrap_or(MetadataAttr { custom: vec![] });
                            let type_type =
                                TypeInfo::from_type_tokens(item_type.clone(), metadata_attr)?;
                            if !type_infos.contains_key(&type_type.name) {
                                type_infos
                                    .insert(type_type.name.clone(), TypeCategory::Type(type_type));
                            }
                        }
                    }
                }
                Item::Fn(item_fn) => {
                    for attr in &item_fn.attrs {
                        if attr.path().is_ident("metadata") {
                            let metadata_attr = attr
                                .parse_args::<MetadataAttr>()
                                .unwrap_or(MetadataAttr { custom: vec![] });
                            let fn_info = FnInfo::from_tokens(item_fn.clone(), metadata_attr)?;
                            if !fn_infos.contains_key(&fn_info.name) {
                                fn_infos.insert(fn_info.name.clone(), fn_info);
                            }
                        }
                    }
                }
                Item::Mod(item_mod) => {
                    process_syntax(
                        &item_mod
                            .content
                            .as_ref()
                            .unwrap_or(&(Brace::default(), vec![]))
                            .1,
                        fn_infos,
                        type_infos,
                    )?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    process_paths(&files, &mut fn_infos, &mut type_infos)?;

    if parsed_routes.is_empty() {
        return Err(s_err(
            proc_macro2::Span::call_site(),
            "The routes are empty, please add them in the `routes` field of the `generate!` macro",
        ));
    }

    for route in &parsed_routes {
        // Check if function exists, if not throw an error.
        let fn_info = fn_infos.get(&route.handler).ok_or(s_err(
            proc_macro2::Span::call_site(),
            format!(
                "Function '{}' not found, add the `#[metadata]` attribute to the definition and make sure it's included in the `files` of the `generate!` macro",
                route.handler
            ),
        ))?;

        // Check if all dependencies of the function are present, if not throw an error.
        for ty in &fn_info.types {
            check_deps(ty, &type_infos)?;
        }
    }

    // After all checks are done, generate the client and write it to the output file.
    codegen::write_client(output, prefix, parsed_routes, fn_infos, type_infos)
        .map_err(|e| s_err(proc_macro2::Span::call_site(), e))?;

    let routes_quote = MethodRoutes(routes);

    Ok(quote! { #routes_quote })
}

/// Recursively checks if all dependencies of a type are present in the type_infos, if not throws an error.
fn check_deps(name: &str, type_infos: &HashMap<String, TypeCategory>) -> syn::Result<()> {
    let deps = match type_infos.get(name) {
        Some(TypeCategory::Struct(i)) | Some(TypeCategory::Type(i)) => &i.dependencies,
        Some(TypeCategory::Enum(_)) => return Ok(()),
        None => return Err(s_err(
            proc_macro2::Span::call_site(),
            format!(
                "Dependency '{}' not found, add the `#[metadata]` attribute to the definition and make sure it's included in the `files` of the `generate!` macro",
                name
            ),
        )),
    };
    for dep in deps {
        check_deps(dep, type_infos)?;
    }
    Ok(())
}

struct GenerateArgs {
    prefix: String,
    routes: Vec<MethodRouter>,
    files: Vec<String>,
    output: String,
}

impl GenerateArgs {
    fn new() -> Self {
        Self {
            prefix: String::new(),
            routes: vec![],
            files: vec![String::from("src")],
            output: String::new(),
        }
    }
}

impl Parse for GenerateArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut ret = GenerateArgs::new();

        while !input.is_empty() {
            let ident = syn::Ident::parse(input)?;
            <Token![=]>::parse(input)?;
            match ident.to_string().as_str() {
                "prefix" => {
                    ret.prefix = input.parse::<LitStr>()?.value();
                }
                "routes" => {
                    let content;
                    braced!(content in input);
                    let parsed_content: Punctuated<MethodRouter, Comma> =
                        Punctuated::parse_terminated(&content)?;
                    ret.routes = parsed_content.iter().map(|lit| lit.to_owned()).collect();
                }
                "files" => {
                    ret.files = if input.peek(syn::token::Bracket) {
                        let content;
                        bracketed!(content in input);
                        let parsed_content: Punctuated<LitStr, Comma> =
                            Punctuated::parse_terminated(&content)?;
                        parsed_content.iter().map(|lit| lit.value()).collect()
                    } else {
                        vec![input.parse::<syn::LitStr>()?.value()]
                    };
                }
                "output" => {
                    ret.output = input.parse::<LitStr>()?.value();
                }
                _ => return Err(s_err(ident.span(), "unknown argument")),
            };
            if !input.is_empty() {
                <Token![,]>::parse(input)?;
            }
        }

        if ret.routes.is_empty() || ret.output.is_empty() {
            return Err(s_err(
                proc_macro2::Span::call_site(),
                "to generate the api both `routes` and `output` fields are required",
            ));
        }

        Ok(ret)
    }
}

/// Syntax in `routers` field.
#[derive(Clone)]
pub struct MethodRouter {
    pub url: LitStr,
    pub methods: Vec<Method>,
}

impl fmt::Debug for MethodRouter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("MethodRouter")
            .field("url", &self.url.value())
            .field("methods", &self.methods)
            .finish()
    }
}

impl MethodRouter {
    fn to_routes(&self) -> Vec<Route> {
        self.methods
            .iter()
            .map(|method| Route {
                url: self.url.value(),
                method: method.method.to_string(),
                handler: method.handler.to_string(),
            })
            .collect()
    }
}

impl Parse for MethodRouter {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let url = input.parse()?;
        <Token![=]>::parse(input)?;
        let mut methods = vec![];
        while !input.is_empty() {
            methods.push(input.parse()?);
            if input.peek(Token![.]) {
                <Token![.]>::parse(input)?;
            } else {
                break;
            }
        }
        Ok(MethodRouter { url, methods })
    }
}

/// Wrapper over MethodRouter for ToTokens conversion
struct MethodRoutes(Vec<MethodRouter>);

impl ToTokens for MethodRoutes {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let expanded = Framework::router_tokens(&self.0);
        tokens.extend(expanded);
    }
}

#[derive(Debug, Clone)]
pub struct Method {
    pub method: syn::Ident,
    pub handler: syn::Ident,
}

impl Parse for Method {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let method = input.parse()?;
        let content;
        parenthesized!(content in input);
        let handler = content.parse()?;
        Ok(Method { method, handler })
    }
}

/// Route information.
#[derive(Clone, Debug, PartialEq)]
pub struct Route {
    pub url: String,
    pub method: String,
    pub handler: String,
}
