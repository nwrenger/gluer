#![doc = include_str!("../README.md")]

use proc_macro as pc;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::{
    collections::{BTreeMap, HashMap},
    fmt, fs,
    io::Write,
    vec,
};
use syn::{
    bracketed, parenthesized,
    parse::Parse,
    punctuated::Punctuated,
    spanned::Spanned,
    token::{Brace, Comma},
    Item, LitStr, Stmt, StmtMacro, Type,
};

fn s_err(span: proc_macro2::Span, msg: impl fmt::Display) -> syn::Error {
    syn::Error::new(span, msg)
}

/// Use this for defining the routes of the router, this is kind of a wrapper, needed for the `generate!` macro to find this.
///
/// # Parameters
/// - `router_ident`: The ident of the router variable.
/// - `url`: The URL of the route.
/// - `base`: The base URL for the API.
///
/// # Note
/// When using state, make sure to return the router with the state, like this:
/// ```rust
/// use axum::{Router, routing::get, extract::State};
/// use gluer::route;
///
/// async fn fetch_root(State(_): State<()>) -> String { String::new() }
///
/// let mut router = Router::new();
///
/// route!(router, "/api", get(fetch_root));
///
/// router.with_state::<()>(()); // <- here and remove the semicolon for returning it
#[proc_macro]
pub fn route(input: pc::TokenStream) -> pc::TokenStream {
    match route_inner(input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn route_inner(input: TokenStream) -> syn::Result<TokenStream> {
    let RouteArgs {
        router_ident,
        url,
        routes,
    } = syn::parse2::<RouteArgs>(input)?;
    Ok(quote! {
        #router_ident = #router_ident.route(#url, #(#routes).*);
    })
}

struct RouteArgs {
    router_ident: syn::Ident,
    url: syn::LitStr,
    routes: Vec<MethodRouter>,
}

impl Parse for RouteArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut routes = vec![];

        let router_ident = input.parse::<syn::Ident>()?;
        input.parse::<syn::Token!(,)>()?;
        let url = input.parse::<syn::LitStr>()?;
        input.parse::<syn::Token!(,)>()?;

        while !input.is_empty() {
            let route = input.parse()?;
            routes.push(route);

            if !input.is_empty() {
                input.parse::<syn::Token!(.)>()?;
            }
        }

        Ok(RouteArgs {
            router_ident,
            url,
            routes,
        })
    }
}

struct MethodRouter {
    method: syn::Ident,
    handler: syn::Ident,
}

impl Parse for MethodRouter {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let method = input.parse()?;
        let content;
        parenthesized!(content in input);
        let handler = content.parse()?;

        Ok(MethodRouter { method, handler })
    }
}

impl ToTokens for MethodRouter {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let method = &self.method;
        let handler = &self.handler;
        tokens.extend(quote! { #method(#handler) });
    }
}

/// Use before structs, functions, enums or types to be findable by the `generate!` macro.
///
/// # Attributes
/// - `custom = [Type, *]`: Specify here types which are named equally to std types but are custom.
///
/// # Struct Attributes
///
/// - `#[meta(into = Type)]`: Specify a type to convert the field into.
/// - `#[meta(skip)]`: Skip the field.
#[proc_macro_attribute]
pub fn metadata(args: pc::TokenStream, input: pc::TokenStream) -> pc::TokenStream {
    match metadata_inner(args.into(), input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn metadata_inner(args: TokenStream, input: TokenStream) -> syn::Result<TokenStream> {
    let span = input.span();
    let item = syn::parse2::<syn::Item>(input)?;
    let _ = syn::parse2::<MetadataAttr>(args)?;

    let out = match item {
        syn::Item::Struct(mut struct_item) => {
            // Clean off all "meta" attributes
            for field in struct_item.fields.iter_mut() {
                field.attrs.retain(|attr| !attr.path().is_ident("meta"));
            }
            quote! { #struct_item }
        }
        syn::Item::Enum(enum_item) => quote! { #enum_item },
        syn::Item::Type(type_item) => quote! { #type_item},
        syn::Item::Fn(fn_item) => quote! { #fn_item },
        _ => return Err(s_err(span, "Expected struct, function, enum or type")),
    };

    Ok(out)
}

struct MetadataAttr {
    custom: Vec<String>,
}

impl syn::parse::Parse for MetadataAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut ret = MetadataAttr { custom: vec![] };

        if !input.is_empty() {
            let ident = syn::Ident::parse(input)?;
            <syn::Token![=]>::parse(input)?;
            match ident.to_string().as_str() {
                "custom" => {
                    let content;
                    bracketed!(content in input);
                    let parsed_content: Punctuated<Type, Comma> =
                        Punctuated::parse_terminated(&content)?;

                    for ty in parsed_content {
                        match &ty {
                            Type::Path(path) => {
                                let segments = &path.path.segments.last().unwrap();
                                let ident = &segments.ident;
                                ret.custom.push(ident.to_token_stream().to_string());
                            }
                            _ => return Err(s_err(ty.span(), "Expected the type")),
                        }
                    }
                }
                _ => return Err(s_err(ident.span(), "Unknown argument")),
            };
            if !input.is_empty() {
                <syn::Token![,]>::parse(input)?;
            }
        }

        Ok(ret)
    }
}

/// Generates a TypeScript API client for the frontend from the API routes.
///
/// ## Parameters
/// - `paths`: An array of directories and/or files to include for retrieving project data.
///   - Supports a root directory via `""`.
///   - Supports multiple directories or files via `["dir1", "dir2/some.rs"]`.
/// - `path`: The directory and filename where the generated TypeScript file will be saved.
/// - `base`: The base URL for the API. This URL should not end with a slash (`/`). Examples:
///   - Use `""` if you are utilizing `axum`'s static file serving and need no base URL.
///   - Use `"http://localhost:8080"` for a local server.
#[proc_macro]
pub fn generate(input: pc::TokenStream) -> pc::TokenStream {
    match generate_inner(input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn generate_inner(input: TokenStream) -> syn::Result<TokenStream> {
    let GenerateArgs {
        project_paths,
        path,
        base,
    } = syn::parse2::<GenerateArgs>(input.clone())?;

    let project_paths = project_paths
        .iter()
        .map(|s: &String| std::path::PathBuf::from(s))
        .collect::<Vec<_>>();

    let mut routes = Vec::new();
    let mut fn_infos = HashMap::new();
    let mut type_infos = HashMap::new();

    let mut parsed_ts = ParsedTypeScript::filled(&base);

    fn process_paths(
        project_paths: &[std::path::PathBuf],
        routes: &mut Vec<Route>,
        fn_infos: &mut HashMap<String, FnInfo>,
        type_infos: &mut HashMap<String, TypeCategory>,
    ) -> syn::Result<()> {
        for path in project_paths {
            if path.is_dir() {
                process_single_dir(path, routes, fn_infos, type_infos)?;
            } else if path.extension().and_then(|s| s.to_str()) == Some("rs") {
                process_single_file(path, routes, fn_infos, type_infos)?;
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

    fn process_single_dir(
        dir: &std::path::Path,
        routes: &mut Vec<Route>,
        fn_infos: &mut HashMap<String, FnInfo>,
        type_infos: &mut HashMap<String, TypeCategory>,
    ) -> syn::Result<()> {
        for entry in fs::read_dir(dir).map_err(|e| {
            s_err(
                proc_macro2::Span::call_site(),
                format!("Couldn't read dir entry: {}", e),
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
                process_single_dir(&path, routes, fn_infos, type_infos)?;
            } else if path.extension().and_then(|s| s.to_str()) == Some("rs") {
                process_single_file(&path, routes, fn_infos, type_infos)?;
            }
        }
        Ok(())
    }

    fn process_single_file(
        path: &std::path::Path,
        routes: &mut Vec<Route>,
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
        process_syntax(&syntax.items, routes, fn_infos, type_infos)?;
        Ok(())
    }

    fn process_syntax(
        syntax: &Vec<Item>,
        routes: &mut Vec<Route>,
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
                            let enum_type = generate_enum(item_enum.clone(), metadata_attr)?;
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
                            let struct_type = generate_struct(item_struct.clone(), metadata_attr)?;
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
                            let type_type = generate_type(item_type.clone(), metadata_attr)?;
                            if !type_infos.contains_key(&type_type.name) {
                                type_infos
                                    .insert(type_type.name.clone(), TypeCategory::Type(type_type));
                            }
                        }
                    }
                }
                Item::Fn(item_fn) => {
                    for stmt in &item_fn.block.stmts {
                        if let Stmt::Macro(StmtMacro { mac, .. }) = stmt {
                            if mac.path.is_ident("route") {
                                let RouteArgs {
                                    url,
                                    routes: method_routes,
                                    ..
                                } = syn::parse2::<RouteArgs>(mac.tokens.clone())?;

                                for route in method_routes {
                                    let method = route.method.to_string().to_uppercase();
                                    let handler = route.handler.to_string();
                                    let route = Route {
                                        url: url.value(),
                                        method: method.clone(),
                                        handler,
                                    };
                                    if !routes.contains(&route) {
                                        routes.push(route);
                                    }
                                }
                            }
                        }
                    }
                    for attr in &item_fn.attrs {
                        if attr.path().is_ident("metadata") {
                            let metadata_attr = attr
                                .parse_args::<MetadataAttr>()
                                .unwrap_or(MetadataAttr { custom: vec![] });
                            let fn_info = generate_function(item_fn.clone(), metadata_attr)?;
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
                        routes,
                        fn_infos,
                        type_infos,
                    )?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    process_paths(&project_paths, &mut routes, &mut fn_infos, &mut type_infos)?;

    for route in routes {
        let fn_info = fn_infos.get(&route.handler).ok_or(s_err(
            proc_macro2::Span::call_site(),
            format!(
                "Function '{}' not found, add the `#[metadata] attribute to the definition",
                route.handler
            ),
        ))?;
        for ty in &fn_info.types {
            let ty = type_infos.get(ty).ok_or(s_err(
                proc_macro2::Span::call_site(),
                format!(
                    "Dependency '{}' not found, add the `#[metadata] attribute to the definition",
                    ty
                ),
            ))?;
            ty.resolving_dependencies(&type_infos, &mut parsed_ts)?;
        }

        if parsed_ts.functions.contains_key(&fn_info.name) {
            return Err(s_err(
                proc_macro2::Span::call_site(),
                format!("Function with name '{}' already exists", fn_info.name,),
            ));
        } else {
            parsed_ts.functions.insert(
                fn_info.name.to_string(),
                fn_info.generate_ts_function(&route, &parsed_ts)?,
            );
        }
    }

    parsed_ts
        .write_to_file(path)
        .map_err(|e| s_err(proc_macro2::Span::call_site(), e))?;

    Ok(quote! {})
}

struct GenerateArgs {
    project_paths: Vec<String>,
    path: String,
    base: String,
}

impl Parse for GenerateArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let project_paths = if input.peek(syn::token::Bracket) {
            let content;
            bracketed!(content in input);
            let parsed_content: Punctuated<LitStr, Comma> = Punctuated::parse_terminated(&content)?;
            parsed_content.iter().map(|lit| lit.value()).collect()
        } else {
            vec![input.parse::<syn::LitStr>()?.value()]
        };
        input.parse::<syn::Token![,]>()?;
        let path = input.parse::<syn::LitStr>()?.value();
        input.parse::<syn::Token![,]>()?;
        let base = input.parse::<syn::LitStr>()?.value();

        if !input.is_empty() {
            input.parse::<syn::Token![,]>()?;
        }

        Ok(GenerateArgs {
            project_paths,
            path,
            base,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
/// Route information.
struct Route {
    url: String,
    method: String,
    handler: String,
}

/// Function information.
#[derive(Clone, Debug)]
struct FnInfo {
    name: String,
    params: Vec<Field>,
    response: RustType,
    types: Vec<String>,
    docs: Vec<String>,
}

impl FnInfo {
    fn generate_ts_function(
        &self,
        route: &Route,
        parsed_ts: &ParsedTypeScript,
    ) -> syn::Result<String> {
        let mut url = route.url.to_string();

        let params_type = self
            .params
            .iter()
            .map(|Field { name: _, ty }| {
                ty.to_api_type(
                    &[],
                    &parsed_ts.interfaces,
                    &parsed_ts.enum_types,
                    &parsed_ts.type_types,
                )
            })
            .collect::<syn::Result<Vec<_>>>()?;

        let response_type = self.response.to_api_type(
            &[],
            &parsed_ts.interfaces,
            &parsed_ts.enum_types,
            &parsed_ts.type_types,
        )?;

        let params_str = params_type
            .iter()
            .filter_map(|ty| match ty {
                ApiType::Json(ty) => Some(format!("data: {}", ty)),
                ApiType::Path(ty) => Some(format!("path: {}", ty)),
                ApiType::PathTuple(ty) => Some(format!("pathTuple: {}", ty)),
                ApiType::Query(ty) => Some(format!("query: {}", ty)),
                ApiType::QueryMap(ty) => Some(format!("queryMap: {}", ty)),
                ApiType::Unknown(_) => None,
            })
            .collect::<Vec<_>>()
            .join(", ");

        let body_assignment = if params_str.contains("data") {
            "\n            body: JSON.stringify(data)"
        } else {
            ""
        };

        if params_str.contains("pathTuple") {
            let mut i = 0;
            url = url
                .split("/")
                .map(|part| {
                    if part.starts_with(":") {
                        i += 1;
                        format!("${{encodeURIComponent(pathTuple[{}])}}", i - 1)
                    } else {
                        part.to_string()
                    }
                })
                .collect::<Vec<_>>()
                .join("/");
        } else if params_str.contains("path") {
            url = url.split(":").next().unwrap().to_string();
            url += "${encodeURIComponent(path)}";
        }

        if params_str.contains("queryMap") {
            url += "?";
            url += "${new URLSearchParams(queryMap).toString()}";
        } else if params_str.contains("query") {
            url += "${query_str(query)}";
        }

        let mut fn_str = String::new();
        fn_str.push_str(&generate_docstring(&self.docs));
        fn_str.push_str(&format!(
            r#"    export async function {fn_name}({params_str}): Promise<{response_type}> {{
        return fetch_api(`${{BASE}}{url}`, {{
            method: "{method}", {body_assignment}
        }});
    }}
"#,
            fn_name = self.name,
            params_str = params_str,
            response_type = response_type.unwrap(),
            url = url,
            method = route.method.to_uppercase(),
            body_assignment = body_assignment
        ));
        Ok(fn_str)
    }
}

/// Information type.
#[derive(Clone, Debug)]
enum TypeCategory {
    Struct(TypeInfo),
    Enum(TypeInfo),
    Type(TypeInfo),
}

impl TypeCategory {
    fn resolving_dependencies(
        &self,
        dependencies: &HashMap<String, TypeCategory>,
        parsed_ts: &mut ParsedTypeScript,
    ) -> syn::Result<()> {
        match self {
            TypeCategory::Struct(type_info) => {
                for dependency in &type_info.dependencies {
                    Self::resolving_dependencies(
                        dependencies.get(dependency).ok_or(s_err(
                            proc_macro2::Span::call_site(),
                            format!("Dependency '{}' not found, add the `#[metadata] attribute to the definition", dependency),
                        ))?,
                        dependencies,
                        parsed_ts,
                    )?;
                }
                if !parsed_ts
                    .interfaces
                    .contains_key(&type_info.name.to_string())
                {
                    parsed_ts.interfaces.insert(
                        type_info.name.to_string(),
                        type_info.generate_interface(
                            &parsed_ts.interfaces,
                            &parsed_ts.enum_types,
                            &parsed_ts.type_types,
                        )?,
                    );
                }
            }
            TypeCategory::Enum(type_info) => {
                if let std::collections::btree_map::Entry::Vacant(e) =
                    parsed_ts.enum_types.entry(type_info.name.to_string())
                {
                    e.insert(type_info.generate_enum_type()?);
                }
            }
            TypeCategory::Type(type_info) => {
                for dependency in &type_info.dependencies {
                    Self::resolving_dependencies(
                        dependencies.get(dependency).ok_or(s_err(
                            proc_macro2::Span::call_site(),
                            format!("Dependency '{}' not found, add the `#[metadata] attribute to the definition", dependency),
                        ))?,
                        dependencies,
                        parsed_ts,
                    )?;
                }
                if !parsed_ts
                    .type_types
                    .contains_key(&type_info.name.to_string())
                {
                    parsed_ts.type_types.insert(
                        type_info.name.to_string(),
                        type_info.generate_type_type(
                            &parsed_ts.interfaces,
                            &parsed_ts.enum_types,
                            &parsed_ts.type_types,
                        )?,
                    );
                }
            }
        }
        Ok(())
    }
}

fn generate_struct(
    item_struct: syn::ItemStruct,
    metadata_attr: MetadataAttr,
) -> syn::Result<TypeInfo> {
    let struct_name_ident = item_struct.ident.clone();
    let struct_name = struct_name_ident.to_string();
    let generics: Vec<String> = item_struct
        .generics
        .type_params()
        .map(|type_param| type_param.ident.to_string())
        .collect();
    let docs = get_docs(&item_struct.attrs);

    let mut dependencies: Vec<String> = Vec::new();

    let item_struct_fields = item_struct.fields.clone();

    let fields = item_struct_fields
        .iter()
        .filter_map(|field| {
            let ident = match field.ident.clone() {
                Some(ident) => ident.to_string(),
                None => {
                    return Some(Err(s_err(
                        field.span(),
                        "Unnamed fields like `self` are not supported",
                    )))
                }
            };

            let meta_attr = match parse_field_attr(&field.attrs) {
                Ok(meta_attr) => meta_attr,
                Err(e) => return Some(Err(e)),
            };

            let MetaAttr { into, skip } = meta_attr;

            let field_ty = if let Some(conv_fn) = into.clone() {
                conv_fn
            } else {
                field.ty.clone()
            };

            if skip {
                return None;
            }

            if let Some(ty) = to_rust_type(&field_ty, &metadata_attr.custom) {
                process_rust_type(&ty, &mut dependencies, &generics);
                Some(Ok((ident, ty)))
            } else {
                Some(Err(s_err(field.span(), "Unsupported Rust Type")))
            }
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let fields_info: Vec<Field> = fields
        .iter()
        .map(|(ident, ty)| Field {
            name: ident.clone(),
            ty: ty.clone(),
        })
        .collect();

    Ok(TypeInfo {
        name: struct_name,
        generics,
        fields: fields_info,
        dependencies,
        docs,
    })
}

struct MetaAttr {
    into: Option<syn::Type>,
    skip: bool,
}

fn parse_field_attr(attrs: &[syn::Attribute]) -> syn::Result<MetaAttr> {
    let mut meta_attr = MetaAttr {
        into: None,
        skip: false,
    };

    for attr in attrs {
        if !attr.path().is_ident("meta") {
            continue;
        }

        attr.parse_nested_meta(|meta| {
            if meta.path.is_ident("into") {
                meta.input.parse::<syn::Token![=]>()?;
                let ty: syn::Type = meta.input.parse()?;
                meta_attr.into = Some(ty);
                return Ok(());
            }

            if meta.path.is_ident("skip") {
                meta_attr.skip = true;
                return Ok(());
            }
            Err(meta.error("Expected #[meta(into = Type)] or #[meta(skip)]"))
        })?;
    }

    Ok(meta_attr)
}

fn generate_enum(item_enum: syn::ItemEnum, _: MetadataAttr) -> syn::Result<TypeInfo> {
    if !item_enum.generics.params.is_empty() {
        return Err(s_err(
            item_enum.generics.span(),
            "Generics and Lifetimes not supported for enums",
        ));
    }

    let enum_name_ident = item_enum.ident.clone();
    let enum_name = enum_name_ident.to_string();
    let docs = get_docs(&item_enum.attrs);

    let fields = item_enum
        .variants
        .iter()
        .map(|variant| {
            if !variant.fields.is_empty() {
                return Err(s_err(
                    variant.fields.span(),
                    "Enums with values are not supported",
                ));
            }
            let ident = variant.ident.to_string();
            Ok(Field {
                name: ident,
                ty: RustType::None,
            })
        })
        .collect::<syn::Result<Vec<_>>>()?;

    Ok(TypeInfo {
        name: enum_name,
        generics: vec![],
        fields,
        dependencies: vec![],
        docs,
    })
}

fn generate_type(item_type: syn::ItemType, metadata_attr: MetadataAttr) -> syn::Result<TypeInfo> {
    let type_name_ident = item_type.ident.clone();
    let type_name = type_name_ident.to_string();
    let generics: Vec<String> = item_type
        .generics
        .type_params()
        .map(|type_param| type_param.ident.to_string())
        .collect();
    let docs = get_docs(&item_type.attrs);

    let mut dependencies: Vec<String> = Vec::new();

    let ty = to_rust_type(&item_type.ty, &metadata_attr.custom)
        .ok_or_else(|| s_err(item_type.ty.span(), "Unsupported type"))?;

    process_rust_type(&ty, &mut dependencies, &generics);

    Ok(TypeInfo {
        name: type_name,
        generics,
        fields: vec![Field {
            name: String::new(),
            ty,
        }],
        dependencies,
        docs,
    })
}

fn generate_function(item_fn: syn::ItemFn, metadata_attr: MetadataAttr) -> syn::Result<FnInfo> {
    let fn_name_ident = item_fn.sig.ident.clone();
    let name = fn_name_ident.to_string();
    let docs = get_docs(&item_fn.attrs);

    let mut dependencies = Vec::new();

    let params = item_fn
        .sig
        .inputs
        .iter()
        .filter_map(|param| match param {
            syn::FnArg::Typed(syn::PatType { pat, ty, .. }) => {
                let pat = pat.to_token_stream().to_string();
                if let Some(rust_type) = to_rust_type(ty, &metadata_attr.custom) {
                    process_rust_type(&rust_type, &mut dependencies, &[]);
                    Some(Ok((pat, rust_type)))
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
            if let Some(rust_type) = to_rust_type(ty, &metadata_attr.custom) {
                process_rust_type(&rust_type, &mut dependencies, &[]);
                rust_type
            } else {
                return Err(s_err(ty.span(), "Unsupported return type"));
            }
        }
        syn::ReturnType::Default => RustType::BuiltIn("()".to_string()),
    };

    let params_info: Vec<Field> = params
        .iter()
        .map(|(pat, ty)| Field {
            name: pat.clone(),
            ty: ty.clone(),
        })
        .collect();

    Ok(FnInfo {
        name,
        params: params_info,
        response,
        types: dependencies,
        docs,
    })
}

fn process_rust_type(rust_type: &RustType, dependencies: &mut Vec<String>, generics: &[String]) {
    match rust_type {
        RustType::Custom(inner_ty) => {
            if !dependencies.contains(inner_ty) && !generics.contains(inner_ty) {
                dependencies.push(inner_ty.clone());
            }
        }
        RustType::CustomGeneric(outer_ty, inner_tys) => {
            if !dependencies.contains(outer_ty) && !generics.contains(outer_ty) {
                dependencies.push(outer_ty.clone());
            }
            for inner_ty in inner_tys {
                process_rust_type(inner_ty, dependencies, generics);
            }
        }
        RustType::Tuple(inner_tys) => {
            for inner_ty in inner_tys {
                process_rust_type(inner_ty, dependencies, generics);
            }
        }
        RustType::Generic(_, inner_tys) => {
            for inner_ty in inner_tys {
                process_rust_type(inner_ty, dependencies, generics);
            }
        }
        _ => {}
    }
}

fn get_docs(attrs: &[syn::Attribute]) -> Vec<String> {
    attrs
        .iter()
        .filter_map(|attr| {
            if attr.path().is_ident("doc") {
                Some(
                    syn::parse::<LitStr>(
                        attr.meta
                            .require_name_value()
                            .ok()?
                            .value
                            .to_token_stream()
                            .into(),
                    )
                    .ok()?
                    .value(),
                )
            } else {
                None
            }
        })
        .map(|s| s.trim().to_string())
        .collect()
}

/// Type information.
#[derive(Clone, Debug)]
struct TypeInfo {
    name: String,
    generics: Vec<String>,
    fields: Vec<Field>,
    dependencies: Vec<String>,
    docs: Vec<String>,
}

impl TypeInfo {
    fn generate_interface(
        &self,
        interfaces: &BTreeMap<String, String>,
        enum_types: &BTreeMap<String, String>,
        type_types: &BTreeMap<String, String>,
    ) -> syn::Result<String> {
        let generics_str = if self.generics.is_empty() {
            "".to_string()
        } else {
            format!("<{}>", self.generics.join(", "))
        };

        let mut interface = String::new();
        interface.push_str(&generate_docstring(&self.docs));
        interface.push_str(&format!(
            "    export interface {}{} {{\n",
            self.name, generics_str
        ));
        for Field { name, ty } in &self.fields {
            let ty = ty.to_api_type(&self.generics, interfaces, enum_types, type_types)?;
            interface.push_str(&format!("        {}: {};\n", name, ty.unwrap()));
        }
        interface.push_str("    }\n");
        Ok(interface)
    }

    fn generate_enum_type(&self) -> syn::Result<String> {
        let mut enum_type = String::new();
        enum_type.push_str(&generate_docstring(&self.docs));
        enum_type.push_str(&format!("    export type {} = ", self.name));
        for (i, field) in self.fields.iter().enumerate() {
            enum_type.push_str(&format!(
                "\"{}\"{}",
                field.name,
                if i == self.fields.len() - 1 {
                    ";"
                } else {
                    " | "
                }
            ));
        }
        enum_type.push('\n');
        Ok(enum_type)
    }

    fn generate_type_type(
        &self,
        interfaces: &BTreeMap<String, String>,
        enum_types: &BTreeMap<String, String>,
        type_types: &BTreeMap<String, String>,
    ) -> syn::Result<String> {
        let mut type_type = String::new();
        type_type.push_str(&generate_docstring(&self.docs));
        type_type.push_str(&format!(
            "    export type {}{} = ",
            self.name,
            if self.generics.is_empty() {
                String::new()
            } else {
                format!("<{}>", &self.generics.join(", "))
            }
        ));
        let mut fields = vec![];
        for Field { ty, .. } in &self.fields {
            let ty = ty.to_api_type(&self.generics, interfaces, enum_types, type_types)?;
            fields.push(ty.unwrap());
        }
        for (i, field) in fields.iter().enumerate() {
            type_type.push_str(&format!(
                "{}{}",
                field,
                if i == fields.len() - 1 { ";" } else { " | " }
            ));
        }
        type_type.push('\n');
        Ok(type_type)
    }
}

fn generate_docstring(docs: &[String]) -> String {
    let mut docstring = String::new();
    if !docs.is_empty() {
        docstring.push_str("    /**\n");
        for doc in docs {
            docstring.push_str(&format!("        {}\n", doc));
        }
        docstring.push_str("    */\n");
    }
    docstring
}

/// Field information.
#[derive(Clone, Debug)]
struct Field {
    name: String,
    ty: RustType,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
enum ApiType {
    Unknown(String),
    Json(String),
    Path(String),
    PathTuple(String),
    Query(String),
    QueryMap(String),
}

impl ApiType {
    fn unwrap(&self) -> String {
        match self {
            ApiType::Unknown(t) => t.to_string(),
            ApiType::Json(t) => t.to_string(),
            ApiType::Path(t) => t.to_string(),
            ApiType::PathTuple(t) => t.to_string(),
            ApiType::Query(t) => t.to_string(),
            ApiType::QueryMap(t) => t.to_string(),
        }
    }
}

struct ParsedTypeScript<'a> {
    base: String,
    basic_functions: &'a str,
    namespace_start: &'a str,
    namespace_end: &'a str,
    interfaces: BTreeMap<String, String>,
    functions: BTreeMap<String, String>,
    enum_types: BTreeMap<String, String>,
    type_types: BTreeMap<String, String>,
}

impl<'a> ParsedTypeScript<'a> {
    fn new(
        base: String,
        basic_functions: &'a str,
        namespace_start: &'a str,
        namespace_end: &'a str,
    ) -> Self {
        Self {
            base,
            basic_functions,
            namespace_start,
            namespace_end,
            interfaces: BTreeMap::new(),
            functions: BTreeMap::new(),
            enum_types: BTreeMap::new(),
            type_types: BTreeMap::new(),
        }
    }

    fn filled(base: &'a str) -> ParsedTypeScript {
        let base = format!("const BASE = '{}';\n", base);
        let basic_functions = r#"    async function fetch_api(endpoint: string, options: RequestInit): Promise<any> {
        const response = await fetch(endpoint, {
            headers: {
                "Content-Type": "application/json",
                ...options.headers,
            },
            ...options,
        });
        return response.json();
    }

    function query_str(params: Record<string, any>): string {
		if (params) {
			let data: Record<string, string> = {};
			for (let key in params) {
				if (params[key] != null) data[key] = params[key].toString();
			}
			// the URLSearchParams escapes any problematic values
			return '?' + new URLSearchParams(data).toString();
		}
		return '';
	}
"#;
        let namespace_start = "namespace api {\n";
        let namespace_end = "}\n\nexport default api;";

        ParsedTypeScript::new(base, basic_functions, namespace_start, namespace_end)
    }

    fn write_to_file<P: AsRef<std::path::Path>>(&self, path: P) -> std::io::Result<()> {
        // todo: errors
        let mut file = fs::File::create(path)?;

        file.write_all(self.base.as_bytes())?;
        file.write_all(b"\n")?;

        file.write_all(self.namespace_start.as_bytes())?;

        for interface in self.interfaces.values() {
            file.write_all(interface.as_bytes())?;
            file.write_all(b"\n")?;
        }

        for enum_type in self.enum_types.values() {
            file.write_all(enum_type.as_bytes())?;
            file.write_all(b"\n")?;
        }

        for type_type in self.type_types.values() {
            file.write_all(type_type.as_bytes())?;
            file.write_all(b"\n")?;
        }

        file.write_all(self.basic_functions.as_bytes())?;
        file.write_all(b"\n")?;

        for (i, function) in self.functions.values().enumerate() {
            file.write_all(function.as_bytes())?;
            if self.functions.len() - 1 > i {
                file.write_all(b"\n")?;
            }
        }

        file.write_all(self.namespace_end.as_bytes())?;
        file.write_all(b"\n")?;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
enum RustType {
    BuiltIn(String),
    Generic(String, Vec<RustType>),
    Tuple(Vec<RustType>),
    Custom(String),
    CustomGeneric(String, Vec<RustType>),
    None,
}

impl RustType {
    fn unwrap(&self) -> String {
        match self {
            Self::BuiltIn(ty) => ty.to_string(),
            Self::Generic(ty, _) => ty.to_string(),
            Self::Custom(ty) => ty.to_string(),
            _ => String::new(),
        }
    }
    fn to_api_type<'a>(
        &self,
        generics: &[String],
        interfaces: &'a BTreeMap<String, String>,
        enum_types: &'a BTreeMap<String, String>,
        type_types: &'a BTreeMap<String, String>,
    ) -> syn::Result<ApiType> {
        if let Some(t) = generics.iter().find(|p| **p == self.unwrap()) {
            return Ok(ApiType::Unknown(t.to_string()));
        }
        if *self == Self::None {
            return Ok(ApiType::Unknown(String::new()));
        }

        match &self {
            Self::BuiltIn(ty) => match ty.as_str() {
                "str" | "String" => return Ok(ApiType::Unknown(String::from("string"))),
                "usize" | "isize" | "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64"
                | "f32" | "f64" => return Ok(ApiType::Unknown(String::from("number"))),
                "bool" => return Ok(ApiType::Unknown(String::from("boolean"))),
                "()" => return Ok(ApiType::Unknown(String::from("void"))),
                _ => {}
            },
            Self::Generic(ty, inner_tys) => match ty.as_str() {
                "Vec" => {
                    let ty = Self::join_generic(
                        inner_tys, generics, interfaces, enum_types, type_types,
                    )?;
                    return Ok(ApiType::Unknown(format!("{}[]", ty)));
                }
                "Html" => {
                    let ty = Self::join_generic(
                        inner_tys, generics, interfaces, enum_types, type_types,
                    )?;
                    return Ok(ApiType::Unknown(ty));
                }
                "Json" => {
                    let ty = Self::join_generic(
                        inner_tys, generics, interfaces, enum_types, type_types,
                    )?;
                    return Ok(ApiType::Json(ty));
                }
                "Path" => {
                    if !inner_tys.is_empty() {
                        if let RustType::Tuple(_) = &inner_tys[0] {
                            let ty = inner_tys[0]
                                .to_api_type(generics, interfaces, enum_types, type_types)?;
                            return Ok(ApiType::PathTuple(ty.unwrap()));
                        }
                    }
                    let ty = Self::join_generic(
                        inner_tys, generics, interfaces, enum_types, type_types,
                    )?;
                    return Ok(ApiType::Path(ty));
                }
                "Query" => {
                    if !inner_tys.is_empty() {
                        if let RustType::Generic(ty, _) = &inner_tys[0] {
                            if ty == "HashMap" {
                                let ty = inner_tys[0]
                                    .to_api_type(generics, interfaces, enum_types, type_types)?;
                                return Ok(ApiType::QueryMap(ty.unwrap()));
                            }
                        }
                    }
                    let ty = Self::join_generic(
                        inner_tys, generics, interfaces, enum_types, type_types,
                    )?;
                    return Ok(ApiType::Query(ty));
                }
                "HashMap" => {
                    if inner_tys.len() != 2 {
                        return Err(s_err(proc_macro2::Span::call_site(), format!(
                            "HashMap must have two inner types, found {}. When wanting to use a custom type, set that on the metadata via `#[metadata(custom = [Type])]`",
                            inner_tys.len()
                        )));
                    }
                    let key_ty =
                        inner_tys[0].to_api_type(generics, interfaces, enum_types, type_types)?;
                    let value_ty =
                        inner_tys[1].to_api_type(generics, interfaces, enum_types, type_types)?;
                    return Ok(ApiType::Unknown(format!(
                        "Record<{}, {}>",
                        key_ty.unwrap(),
                        value_ty.unwrap()
                    )));
                }
                "Result" => {
                    if inner_tys.len() != 2 {
                        return Err(s_err(proc_macro2::Span::call_site(),format!(
                            "Result type must have two inner types, found {}. When wanting to use a custom type, set that on the metadata via `#[metadata(custom = [Type])]`",
                            inner_tys.len()
                        )));
                    }
                    let ok_ty =
                        inner_tys[0].to_api_type(generics, interfaces, enum_types, type_types)?;
                    let err_ty =
                        inner_tys[1].to_api_type(generics, interfaces, enum_types, type_types)?;
                    return Ok(ApiType::Unknown(format!(
                        "{} | {}",
                        ok_ty.unwrap(),
                        err_ty.unwrap()
                    )));
                }
                "Option" => {
                    let ty =
                        inner_tys[0].to_api_type(generics, interfaces, enum_types, type_types)?;
                    return Ok(ApiType::Unknown(format!("{} | null", ty.unwrap())));
                }
                _ => {}
            },
            Self::Tuple(tys) => {
                let tys = Self::join_generic(tys, generics, interfaces, enum_types, type_types)?;
                return Ok(ApiType::Unknown(format!("[{}]", tys)));
            }
            Self::Custom(ty) => {
                if interfaces.contains_key(ty)
                    || enum_types.contains_key(ty)
                    || type_types.contains_key(ty)
                {
                    return Ok(ApiType::Unknown(ty.to_string()));
                }
            }
            Self::CustomGeneric(ty, inner_tys) => {
                if interfaces.contains_key(ty)
                    || enum_types.contains_key(ty)
                    || type_types.contains_key(ty)
                {
                    let tys = Self::join_generic(
                        inner_tys, generics, interfaces, enum_types, type_types,
                    )?;
                    return Ok(ApiType::Unknown(format!("{}<{}>", ty, tys)));
                }
            }
            _ => {}
        };
        Err(s_err(
            proc_macro2::Span::call_site(),
            format!("RustType '{:?}' couldn't be converted to TypeScript", self),
        ))
    }

    fn join_generic(
        tys: &[RustType],
        generics: &[String],
        interfaces: &BTreeMap<String, String>,
        enum_types: &BTreeMap<String, String>,
        type_types: &BTreeMap<String, String>,
    ) -> syn::Result<String> {
        Ok(tys
            .iter()
            .map(|t| t.to_api_type(generics, interfaces, enum_types, type_types))
            .collect::<syn::Result<Vec<_>>>()?
            .iter()
            .map(|t| t.unwrap())
            .collect::<Vec<_>>()
            .join(", "))
    }
}

const RUST_TYPES: &[&str] = &[
    "bool", "char", "str", "u8", "u16", "u32", "u64", "u128", "i8", "i16", "i32", "i64", "i128",
    "usize", "isize", "f32", "f64", "String",
];

const SKIP_TYPES: &[&str] = &["State", "Headers", "Bytes", "Request", "Extension"];

const BUILTIN_GENERICS: &[&str] = &[
    "Query", "HashMap", "Path", "Vec", "Json", "Option", "Result",
];

fn is_builtin_type(ident: &syn::Ident) -> bool {
    RUST_TYPES.contains(&ident.to_string().as_str())
}

fn is_skip_type(ident: &syn::Ident) -> bool {
    SKIP_TYPES.contains(&ident.to_string().as_str())
}

fn is_builtin_generic(ident: &syn::Ident) -> bool {
    BUILTIN_GENERICS.contains(&ident.to_string().as_str())
}

fn is_custom(ident: &syn::Ident, custom: &[String]) -> bool {
    custom.contains(&ident.to_string())
}

fn to_rust_type(ty: &syn::Type, custom: &[String]) -> Option<RustType> {
    match ty {
        syn::Type::Path(type_path) => {
            let segment = type_path.path.segments.last().unwrap();
            let ident = &segment.ident;

            if is_builtin_type(ident) && !is_custom(ident, custom) {
                Some(RustType::BuiltIn(ident.to_string()))
            } else if is_skip_type(ident) && !is_custom(ident, custom) {
                None
            } else if is_builtin_generic(ident) && !is_custom(ident, custom) {
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    let inner_types: Vec<RustType> = args
                        .args
                        .iter()
                        .filter_map(|arg| {
                            if let syn::GenericArgument::Type(inner_ty) = arg {
                                to_rust_type(inner_ty, custom)
                            } else {
                                None
                            }
                        })
                        .collect();
                    Some(RustType::Generic(ident.to_string(), inner_types))
                } else {
                    Some(RustType::Generic(ident.to_string(), vec![]))
                }
            } else if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                let inner_types: Vec<RustType> = args
                    .args
                    .iter()
                    .filter_map(|arg| {
                        if let syn::GenericArgument::Type(inner_ty) = arg {
                            to_rust_type(inner_ty, custom)
                        } else {
                            None
                        }
                    })
                    .collect();
                Some(RustType::CustomGeneric(ident.to_string(), inner_types))
            } else {
                Some(RustType::Custom(ident.to_string()))
            }
        }
        syn::Type::Reference(syn::TypeReference { elem, .. })
        | syn::Type::Paren(syn::TypeParen { elem, .. })
        | syn::Type::Group(syn::TypeGroup { elem, .. }) => to_rust_type(elem, custom),

        syn::Type::Tuple(type_tuple) => {
            if type_tuple.elems.is_empty() {
                return Some(RustType::BuiltIn("()".to_string()));
            }
            let inner_types: Vec<RustType> = type_tuple
                .elems
                .iter()
                .filter_map(|t| to_rust_type(t, custom))
                .collect();
            Some(RustType::Tuple(inner_types))
        }
        syn::Type::Slice(syn::TypeSlice { elem, .. })
        | syn::Type::Array(syn::TypeArray { elem, .. }) => to_rust_type(elem, custom)
            .map(|inner| RustType::Generic("Vec".to_string(), vec![inner])),
        _ => None,
    }
}
