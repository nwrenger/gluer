#![doc = include_str!("../README.md")]

use proc_macro::{self as pc};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::{
    collections::{BTreeMap, HashMap},
    fmt::{self, Debug},
    fs,
    io::Write,
    vec,
};
use syn::{
    braced, bracketed, parenthesized,
    parse::Parse,
    punctuated::Punctuated,
    spanned::Spanned,
    token::{Brace, Comma},
    Item, LitStr, Token, Type,
};

fn s_err(span: proc_macro2::Span, msg: impl fmt::Display) -> syn::Error {
    syn::Error::new(span, msg)
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
/// - `#[meta(optional)]`: Make a field optional.
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
                parse_field_attr(&field.attrs)?;
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

struct MetaAttr {
    into: Option<syn::Type>,
    skip: bool,
    optional: bool,
}

/// Parses Meta information a field by going through it's attributes
fn parse_field_attr(attrs: &[syn::Attribute]) -> syn::Result<MetaAttr> {
    let mut meta_attr = MetaAttr {
        into: None,
        skip: false,
        optional: false,
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

            if meta.path.is_ident("optional") {
                meta_attr.optional = true;
                return Ok(());
            }
            Err(meta.error("Expected #[meta(into = Type)], #[meta(skip)] or #[meta(optional)]"))
        })?;
    }

    if (meta_attr.into.is_some() || meta_attr.optional) && meta_attr.skip {
        return Err(s_err(
            proc_macro2::Span::call_site(),
            "skip allows no further arguments",
        ));
    }

    Ok(meta_attr)
}

/// Generates a TypeScript API client and axum compatible router.
///
/// ## Parameters
///
/// - `prefix`: An optional parameter that allows you to specify a prefix for all generated routes. This can be useful if your API is hosted under a common base path (e.g., `/api`).
/// - `routes`: A required parameter that specifies the API routes for which the TypeScript client and resulting Router will be generated. Each route is defined by a URL path (which can include parameters) followed by one or more HTTP methods (e.g., `get`, `post`) and their corresponding handler functions.
/// - `files`: An optional parameter that specifies the files containing the Rust source files that define the handlers and dependencies. This can be either a single string literal (e.g., `"src"`) or an array of string literals (e.g., `["src/db", "src", "src/error.rs"]`). These paths are used to extract type information for the TypeScript client. Ensure that these paths are correct and point to the appropriate directories or files. The default of `"src"` should handle most cases appropriately.
/// - `output`: A required parameter that specifies the path to the output file where the generated TypeScript client code will be written. Ensure that this path is correct and points to a writable location.
///
/// ## Note
///
/// - **Prefix URL:** The `prefix` parameter is used to prepend a common base path to all routes. It should not end with a `/`. If the prefix is not provided, it defaults to an empty string (`""`), meaning no prefix will be added.
///
/// ## Example
///
/// ```rust, ignore
/// use axum::Router;
/// use gluer::{generate, metadata};
///
/// // Define a handler function
/// #[metadata]
/// fn root() -> String {
///     "root".to_string()
/// }
///
/// // Use the `generate` macro to create the API client and router
/// let _app: Router<()> = generate! {
///     prefix = "", // Sets the prefix to `""`
///     // This can be omitted due to being the same as the default value
///     routes = { // Defines the API routes
///         "/" = get(root), // Route for the root path, using the `root` handler for GET requests
///     },
///     files = "src", // Specifies a single directory containing the handler implementations
///     // This can be omitted due to being the same as the default value
///     // You can also specify multiple directories:
///     // files = ["src/db", "src"],
///
///     output = "tests/api.ts", // Specifies the output file for the generated TypeScript client
/// };
/// ```
#[proc_macro]
pub fn generate(input: pc::TokenStream) -> pc::TokenStream {
    match generate_inner(input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn generate_inner(input: TokenStream) -> syn::Result<TokenStream> {
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

    let parsed_routes: Vec<Route> = routes
        .clone()
        .unwrap()
        .iter()
        .flat_map(|f| f.to_routes())
        .collect();

    let mut fn_infos = HashMap::new();
    let mut type_infos = HashMap::new();

    let mut parsed_ts = ParsedTypeScript::filled(&prefix);
    let mut needs_query_parser = false;

    fn process_paths(
        files: &[std::path::PathBuf],
        fn_infos: &mut HashMap<String, FnInfo>,
        type_infos: &mut HashMap<String, TypeCategory>,
    ) -> syn::Result<()> {
        for path in files {
            if path.is_dir() {
                process_single_dir(path, fn_infos, type_infos)?;
            } else if path.extension().and_then(|s: &std::ffi::OsStr| s.to_str()) == Some("rs") {
                process_single_file(path, fn_infos, type_infos)?;
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
                process_single_dir(&path, fn_infos, type_infos)?;
            } else if path.extension().and_then(|s| s.to_str()) == Some("rs") {
                process_single_file(&path, fn_infos, type_infos)?;
            }
        }
        Ok(())
    }

    fn process_single_file(
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
                            let fn_info =
                                FnInfo::from_function_tokens(item_fn.clone(), metadata_attr)?;
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
        let fn_info = fn_infos.get(&route.handler).ok_or(s_err(
            proc_macro2::Span::call_site(),
            format!(
                "Function '{}' not found, add the `#[metadata]` attribute to the definition and make sure it's included in the `files` of the `generate!` macro",
                route.handler
            ),
        ))?;
        for ty in &fn_info.types {
            let ty = type_infos.get(ty).ok_or(s_err(
                proc_macro2::Span::call_site(),
                format!(
                    "Dependency '{}' not found, add the `#[metadata]` attribute to the definition and make sure it's included in the `files` of the `generate!` macro",
                    ty
                ),
            ))?;
            ty.resolving_dependencies(&type_infos, &mut parsed_ts)?;
        }

        if parsed_ts.functions.contains_key(&fn_info.name) {
            return Err(s_err(
                proc_macro2::Span::call_site(),
                format!("Function with name '{}' already exists", fn_info.name),
            ));
        } else {
            parsed_ts.functions.insert(
                fn_info.name.to_string(),
                fn_info.generate_ts_function(route, &parsed_ts, &mut needs_query_parser)?,
            );
        }
    }

    if needs_query_parser {
        parsed_ts.fill_query_parser();
    }

    parsed_ts
        .write_to_file(output.unwrap())
        .map_err(|e| s_err(proc_macro2::Span::call_site(), e))?;

    let routes_quote = MethodRoutes(routes.unwrap());

    Ok(quote! { #routes_quote })
}

struct GenerateArgs {
    prefix: String,
    routes: Option<Vec<MethodRouter>>,
    files: Vec<String>,
    output: Option<String>,
}

impl GenerateArgs {
    fn new() -> Self {
        Self {
            prefix: String::new(),
            routes: None,
            files: vec![String::from("src")],
            output: None,
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
                    ret.routes = Some(parsed_content.iter().map(|lit| lit.to_owned()).collect());
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
                    ret.output = Some(input.parse::<LitStr>()?.value());
                }
                _ => return Err(s_err(ident.span(), "unknown argument")),
            };
            if !input.is_empty() {
                <Token![,]>::parse(input)?;
            }
        }

        if ret.routes.is_none() || ret.output.is_none() {
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
struct MethodRouter {
    url: LitStr,
    methods: Vec<Method>,
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
        let routes = self.0.iter().map(|route| {
            let url = &route.url;
            let handlers = route.methods.iter().map(|method| {
                let method_name = &method.method;
                let handler_name = &method.handler;
                quote! {
                    #method_name(#handler_name)
                }
            });

            quote! {
                .route(#url, #(#handlers).*)
            }
        });

        let expanded = quote! {
            Router::new()
            #(#routes)*
        };

        tokens.extend(expanded);
    }
}

#[derive(Debug, Clone)]
struct Method {
    method: syn::Ident,
    handler: syn::Ident,
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
    /// Generates function info from token input
    fn from_function_tokens(
        item_fn: syn::ItemFn,
        metadata_attr: MetadataAttr,
    ) -> syn::Result<FnInfo> {
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
                    if let Some(rust_type) = RustType::from_tokens(ty, &metadata_attr.custom) {
                        process_rust_type(&rust_type, &mut dependencies, &[]);
                        Some(Ok((pat, rust_type)))
                    } else {
                        None
                    }
                }
                syn::FnArg::Receiver(_) => Some(Err(s_err(
                    param.span(),
                    format!("Receiver parameter in function '{}' not allowed", name),
                ))),
            })
            .collect::<syn::Result<Vec<_>>>()?;

        let response = match &item_fn.sig.output {
            syn::ReturnType::Type(_, ty) => {
                if let Some(rust_type) = RustType::from_tokens(ty, &metadata_attr.custom) {
                    process_rust_type(&rust_type, &mut dependencies, &[]);
                    rust_type
                } else {
                    return Err(s_err(
                        ty.span(),
                        format!("Unsupported return type in function '{}'", name),
                    ));
                }
            }
            syn::ReturnType::Default => RustType::BuiltIn("()".to_string()),
        };

        let params_info: Vec<Field> = params
            .iter()
            .map(|(pat, ty)| Field {
                name: pat.clone(),
                ty: ty.clone(),
                docs: vec![],
                optional: false,
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

    fn generate_ts_function(
        &self,
        route: &Route,
        parsed_ts: &ParsedTypeScript,
        needs_query_parser: &mut bool,
    ) -> syn::Result<String> {
        let mut url = route.url.to_string();

        let params_type = self
            .params
            .iter()
            .map(|Field { ty, .. }| {
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
                ApiType::Query(ty) => {
                    *needs_query_parser = true;
                    Some(format!("query: {}", ty))
                }
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
        fn_str.push_str(&generate_docstring(&self.docs, "    "));
        fn_str.push_str(&format!(
            r#"    export async function {fn_name}({params_str}): Promise<{response_type}> {{
        return fetch_api(`${{PREFIX}}{url}`, {{
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
                            format!("Dependency '{}' not found, add the `#[metadata]` attribute to the definition and make sure it's included in the `files` of the `generate!` macro", dependency),
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
                            format!("Dependency '{}' not found, add the `#[metadata]` attribute to the definition and make sure it's included in the `files` of the `generate!` macro", dependency),
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
    /// Generates type info of structs from token input
    fn from_struct_tokens(
        item_struct: syn::ItemStruct,
        metadata_attr: MetadataAttr,
    ) -> syn::Result<Self> {
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

                let docs = get_docs(&field.attrs);

                let meta_attr = match parse_field_attr(&field.attrs) {
                    Ok(meta_attr) => meta_attr,
                    Err(_) => {
                        return Some(Err(s_err(
                            field.span(),
                            format!(
                                "An error occurred when parsing field attributes on struct '{}'",
                                struct_name
                            ),
                        )))
                    }
                };

                let MetaAttr {
                    into,
                    skip,
                    optional,
                } = meta_attr;

                let field_ty = if let Some(conv_fn) = into.clone() {
                    conv_fn
                } else {
                    field.ty.clone()
                };

                if skip {
                    return None;
                }

                if let Some(ty) = RustType::from_tokens(&field_ty, &metadata_attr.custom) {
                    process_rust_type(&ty, &mut dependencies, &generics);
                    Some(Ok(Field {
                        name: ident,
                        ty,
                        docs,
                        optional,
                    }))
                } else {
                    Some(Err(s_err(field.span(), "Unsupported Rust Type")))
                }
            })
            .collect::<syn::Result<Vec<_>>>()?;

        Ok(Self {
            name: struct_name,
            generics,
            fields,
            dependencies,
            docs,
        })
    }

    /// Generates type info of enums from token input
    fn from_enum_tokens(item_enum: syn::ItemEnum, _: MetadataAttr) -> syn::Result<Self> {
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
                let docs = get_docs(&variant.attrs);
                Ok(Field {
                    name: ident,
                    ty: RustType::None,
                    docs,
                    optional: false,
                })
            })
            .collect::<syn::Result<Vec<_>>>()?;

        Ok(Self {
            name: enum_name,
            generics: vec![],
            fields,
            dependencies: vec![],
            docs,
        })
    }

    /// Generates type info of types from token input
    fn from_type_tokens(
        item_type: syn::ItemType,
        metadata_attr: MetadataAttr,
    ) -> syn::Result<Self> {
        let type_name_ident = item_type.ident.clone();
        let type_name = type_name_ident.to_string();
        let generics: Vec<String> = item_type
            .generics
            .type_params()
            .map(|type_param| type_param.ident.to_string())
            .collect();
        let docs = get_docs(&item_type.attrs);

        let mut dependencies: Vec<String> = Vec::new();

        let ty = RustType::from_tokens(&item_type.ty, &metadata_attr.custom)
            .ok_or_else(|| s_err(item_type.ty.span(), "Unsupported type"))?;

        process_rust_type(&ty, &mut dependencies, &generics);

        Ok(Self {
            name: type_name,
            generics,
            fields: vec![Field {
                name: String::new(),
                ty,
                docs: vec![],
                optional: false,
            }],
            dependencies,
            docs,
        })
    }

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
        interface.push_str(&generate_docstring(&self.docs, "    "));
        interface.push_str(&format!(
            "    export interface {}{} {{\n",
            self.name, generics_str
        ));
        for field in &self.fields {
            interface.push_str(&generate_docstring(&field.docs, "        "));
            let ty = field
                .ty
                .to_api_type(&self.generics, interfaces, enum_types, type_types)?;
            interface.push_str(&format!(
                "        {}{}: {};\n",
                field.name,
                if field.optional { "?" } else { "" },
                ty.unwrap()
            ));
        }
        interface.push_str("    }\n");
        Ok(interface)
    }

    fn generate_enum_type(&self) -> syn::Result<String> {
        let mut enum_type = String::new();
        enum_type.push_str(&generate_docstring(&self.docs, "    "));
        enum_type.push_str(&format!("    export enum {} {{\n", self.name));
        for field in &self.fields {
            enum_type.push_str(&generate_docstring(&field.docs, "        "));
            enum_type.push_str(&format!("        {} = \"{}\",\n", field.name, field.name));
        }
        enum_type.push_str("    }\n");
        Ok(enum_type)
    }

    fn generate_type_type(
        &self,
        interfaces: &BTreeMap<String, String>,
        enum_types: &BTreeMap<String, String>,
        type_types: &BTreeMap<String, String>,
    ) -> syn::Result<String> {
        let mut type_type = String::new();
        type_type.push_str(&generate_docstring(&self.docs, "    "));
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

fn generate_docstring(docs: &[String], ident: &str) -> String {
    let mut docstring = String::new();
    if !docs.is_empty() {
        docstring.push_str(&format!("{}/**\n", ident));
        for doc in docs {
            docstring.push_str(&format!("{}    {}\n", ident, doc));
        }
        docstring.push_str(&format!("{}*/\n", ident));
    }
    docstring
}

/// Fills the dependencies for `Custom` and `CustomGeneric` types
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

/// Gets docs by collecting the `LitStr` of `doc` attributes
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

/// Field information.
#[derive(Clone, Debug)]
struct Field {
    name: String,
    ty: RustType,
    docs: Vec<String>,
    optional: bool,
}

/// Type wrapper for `axum`'s types.
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

/// Contains all TypeScript code.
struct ParsedTypeScript<'a> {
    prefix: String,
    basic_functions: String,
    namespace_start: &'a str,
    namespace_end: &'a str,
    interfaces: BTreeMap<String, String>,
    functions: BTreeMap<String, String>,
    enum_types: BTreeMap<String, String>,
    type_types: BTreeMap<String, String>,
}

impl<'a> ParsedTypeScript<'a> {
    fn new(
        prefix: String,
        basic_functions: String,
        namespace_start: &'a str,
        namespace_end: &'a str,
    ) -> Self {
        Self {
            prefix,
            basic_functions,
            namespace_start,
            namespace_end,
            interfaces: BTreeMap::new(),
            functions: BTreeMap::new(),
            enum_types: BTreeMap::new(),
            type_types: BTreeMap::new(),
        }
    }

    /// Creates the `ParsedTypeScript` struct with a few default stuff
    fn filled(prefix: &'a str) -> ParsedTypeScript {
        let prefix = format!("const PREFIX = '{}';\n", prefix);
        let basic_functions =
            r#"    async function fetch_api(endpoint: string, options: RequestInit): Promise<any> {
        const response = await fetch(endpoint, {
            headers: {
                "Content-Type": "application/json",
                ...options.headers,
            },
            ...options,
        });
        if (response.headers.get('Content-Length') === '0') {
			return;
		} else {
			return response.json();
		}
    }
"#
            .to_string();
        let namespace_start = "namespace api {\n";
        let namespace_end = "}\n\nexport default api;";

        ParsedTypeScript::new(prefix, basic_functions, namespace_start, namespace_end)
    }

    /// Adds the query parser to the `basic_functions`
    fn fill_query_parser(&mut self) {
        self.basic_functions += r#"
    function query_str(params: Record<string, any>): string {
		if (params) {
			let data: Record<string, string> = {};
			for (let key in params) {
				if (params[key] != null) data[key] = params[key].toString();
			}
			return '?' + new URLSearchParams(data).toString();
		}
		return '';
	}
"#;
    }

    /// Write the parsed type script to the `path`
    fn write_to_file<P: AsRef<std::path::Path>>(&self, path: P) -> std::io::Result<()> {
        let mut file = fs::File::create(path)?;

        file.write_all(self.prefix.as_bytes())?;
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

/// The parsed Rust Type.
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
    /// Unwrap a Rust Type into the Type String
    fn unwrap(&self) -> String {
        match self {
            Self::BuiltIn(ty) => ty.to_string(),
            Self::Generic(ty, _) => ty.to_string(),
            Self::Custom(ty) => ty.to_string(),
            _ => String::new(),
        }
    }

    /// Generate an api type if needed, the knowledge of already generated types is therefore necessary
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
                "char" | "str" | "String" | "u128" | "i128" | "f128" => {
                    return Ok(ApiType::Unknown(String::from("string")))
                }
                "usize" | "isize" | "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64"
                | "f16" | "f32" | "f64" => return Ok(ApiType::Unknown(String::from("number"))),
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

    /// Joins generics into a string for TypeScript syntax together
    fn join_generic(
        tys: &[Self],
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

    const RUST_TYPES: &'static [&'static str] = &[
        "bool", "char", "str", "u8", "u16", "u32", "u64", "u128", "i8", "i16", "i32", "i64",
        "i128", "usize", "isize", "f16", "f32", "f64", "f128", "String",
    ];

    const SKIP_TYPES: &'static [&'static str] =
        &["State", "Headers", "Bytes", "Request", "Extension"];

    const BUILTIN_GENERICS: &'static [&'static str] = &[
        "Query", "HashMap", "Path", "Vec", "Json", "Option", "Result",
    ];

    fn is_builtin_type(ident: &syn::Ident) -> bool {
        Self::RUST_TYPES.contains(&ident.to_string().as_str())
    }

    fn is_skip_type(ident: &syn::Ident) -> bool {
        Self::SKIP_TYPES.contains(&ident.to_string().as_str())
    }

    fn is_builtin_generic(ident: &syn::Ident) -> bool {
        Self::BUILTIN_GENERICS.contains(&ident.to_string().as_str())
    }

    fn is_custom(ident: &syn::Ident, custom: &[String]) -> bool {
        custom.contains(&ident.to_string())
    }

    /// Returns an optional Rust Type, if `None` the type needs to be skipped
    fn from_tokens(ty: &syn::Type, custom: &[String]) -> Option<Self> {
        match ty {
            syn::Type::Path(type_path) => {
                let segment = type_path.path.segments.last().unwrap();
                let ident = &segment.ident;

                if Self::is_builtin_type(ident) && !Self::is_custom(ident, custom) {
                    Some(Self::BuiltIn(ident.to_string()))
                } else if Self::is_skip_type(ident) && !Self::is_custom(ident, custom) {
                    None
                } else if Self::is_builtin_generic(ident) && !Self::is_custom(ident, custom) {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        let inner_types = Self::extract_inner_types(args, custom);
                        Some(Self::Generic(ident.to_string(), inner_types))
                    } else {
                        Some(Self::Generic(ident.to_string(), vec![]))
                    }
                } else if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    let inner_types = Self::extract_inner_types(args, custom);
                    Some(Self::CustomGeneric(ident.to_string(), inner_types))
                } else {
                    Some(Self::Custom(ident.to_string()))
                }
            }
            syn::Type::Reference(syn::TypeReference { elem, .. })
            | syn::Type::Paren(syn::TypeParen { elem, .. })
            | syn::Type::Group(syn::TypeGroup { elem, .. }) => Self::from_tokens(elem, custom),

            syn::Type::Tuple(type_tuple) => {
                if type_tuple.elems.is_empty() {
                    return Some(Self::BuiltIn(String::from("()")));
                }
                let inner_types: Vec<Self> = type_tuple
                    .elems
                    .iter()
                    .filter_map(|t| Self::from_tokens(t, custom))
                    .collect();
                Some(Self::Tuple(inner_types))
            }
            syn::Type::Slice(syn::TypeSlice { elem, .. })
            | syn::Type::Array(syn::TypeArray { elem, .. }) => Self::from_tokens(elem, custom)
                .map(|inner| Self::Generic("Vec".to_string(), vec![inner])),
            _ => None,
        }
    }

    /// Recursively get inner types
    fn extract_inner_types(
        args: &syn::AngleBracketedGenericArguments,
        custom: &[String],
    ) -> Vec<Self> {
        args.args
            .iter()
            .filter_map(|arg| {
                if let syn::GenericArgument::Type(inner_ty) = arg {
                    Self::from_tokens(inner_ty, custom)
                } else {
                    None
                }
            })
            .collect()
    }
}
