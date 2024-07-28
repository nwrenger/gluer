use lazy_static::lazy_static;
use okapi::openapi3::{
    MediaType, Object, OpenApi, Operation, Parameter, ParameterValue, RefOr, Response, Responses,
    SchemaObject,
};
use proc_macro as pc;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::{
    collections::{BTreeMap, HashMap},
    env::current_dir,
    fmt,
    io::Write,
    sync::Mutex,
};
use syn::{parenthesized, parse::Parse, spanned::Spanned};

fn s_err(span: proc_macro2::Span, msg: impl fmt::Display) -> syn::Error {
    syn::Error::new(span, msg)
}

lazy_static! {
    static ref ROUTES: Mutex<Vec<Route>> = Mutex::new(Vec::new());
}

/// Adds a route to the router. Use for each api endpoint you want to expose to the frontend.
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
        responses,
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
            responses: responses.clone(),
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
    responses: HashMap<String, String>,
}

impl Parse for MethodCall {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let method: syn::Ident = input.parse()?;
        let content;
        parenthesized!(content in input);
        let r#fn: syn::Path = content.parse()?;

        // Retrieve the function name as a string
        let fn_name = r#fn.segments.last().unwrap().ident.to_string();

        // Determine the file to parse based on the path segments
        let segments: Vec<_> = r#fn
            .segments
            .iter()
            .map(|seg| seg.ident.to_string())
            .collect();
        let current_dir = current_dir().map_err(|_| {
            syn::Error::new(
                proc_macro2::Span::call_site(),
                "Failed to get current directory",
            )
        })?;

        let file_path = if segments.len() == 1 {
            // Function is in the same file, check if it's in main.rs or lib.rs (for tests)
            let main_path = current_dir.join("src/main.rs");
            let lib_path = current_dir.join("src/lib.rs");
            if main_path.exists() {
                main_path
            } else if lib_path.exists() {
                current_dir.join("tests/main.rs")
            } else {
                return Err(syn::Error::new(
                    proc_macro2::Span::call_site(),
                    "Neither main.rs nor lib.rs found",
                ));
            }
        } else {
            // Function is in a different module
            let module_path = &segments[0];
            let file_path_mod = current_dir.join(format!("src/{}/mod.rs", module_path));
            let file_path_alt = current_dir.join(format!("src/{}.rs", module_path));
            if file_path_mod.exists() {
                file_path_mod
            } else if file_path_alt.exists() {
                file_path_alt
            } else {
                return Err(syn::Error::new(
                    proc_macro2::Span::call_site(),
                    format!("Module file not found for {}", module_path),
                ));
            }
        };

        let (params, responses) =
            extract_function_params_and_responses_from_file(&fn_name, file_path.to_str().unwrap())?;

        Ok(MethodCall {
            method,
            r#fn,
            params,
            responses,
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

fn extract_function_params_and_responses_from_file(
    fn_name: &str,
    file_path: &str,
) -> syn::Result<(Vec<syn::FnArg>, HashMap<String, String>)> {
    let source = std::fs::read_to_string(file_path)
        .map_err(|e| syn::Error::new(proc_macro2::Span::mixed_site(), e.to_string()))?;
    let syntax = syn::parse_file(&source)?;

    let mut params_map: HashMap<String, Vec<syn::FnArg>> = HashMap::new();
    let mut responses_map: HashMap<String, HashMap<String, String>> = HashMap::new();

    for item in syntax.items {
        if let syn::Item::Fn(syn::ItemFn { sig, .. }) = item {
            let fn_name = sig.ident.to_string();
            let params: Vec<syn::FnArg> = sig.inputs.iter().cloned().collect();
            params_map.insert(fn_name.clone(), params);

            let mut responses = HashMap::new();
            let ty: String = match sig.output {
                syn::ReturnType::Default => "()".to_string(),
                syn::ReturnType::Type(_, ty) => ty.into_token_stream().to_string(),
            };

            responses.insert("200".to_string(), ty);
            responses_map.insert(fn_name, responses);
        }
    }

    let params = params_map.get(fn_name).cloned().ok_or_else(|| {
        syn::Error::new(
            proc_macro2::Span::call_site(),
            "Function parameters not found",
        )
    })?;

    let responses = responses_map.get(fn_name).cloned().ok_or_else(|| {
        syn::Error::new(
            proc_macro2::Span::call_site(),
            "Function responses not found",
        )
    })?;

    Ok((params, responses))
}

struct Route {
    route: String,
    method: String,
    fn_name: String,
    // should be syn::Type but that's not thread safe
    params: Vec<String>,
    responses: HashMap<String, String>,
}

/// Generates an OpenAPI spec from the routes added with `add_route!`. Specify the title, version of the spec, and path to save the spec to.
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
    let title = args.title.value();
    let version = args.version.value();

    let routes = ROUTES.lock().unwrap();
    let mut openapi = OpenApi::new();
    openapi.info.title = title;
    openapi.info.version = version;

    for route in routes.iter() {
        let mut path_item = openapi.paths.get(&route.route).cloned().unwrap_or_default();

        let operation = Operation {
            operation_id: Some(route.fn_name.clone() + "_" + &route.method),
            parameters: route
                .params
                .iter()
                .filter_map(|param| {
                    if param.starts_with("State<") {
                        None
                    } else {
                        Some(RefOr::Object(Parameter {
                            name: param.clone(),
                            location: route.method.clone(),
                            description: None,
                            required: true,
                            deprecated: false,
                            allow_empty_value: false,
                            value: ParameterValue::Schema {
                                style: None,
                                explode: None,
                                allow_reserved: true,
                                schema: SchemaObject::default(),
                                example: None,
                                examples: None,
                            },
                            extensions: Object::default(),
                        }))
                    }
                })
                .collect(),
            request_body: None,
            responses: Responses {
                responses: route
                    .responses
                    .iter()
                    .map(|(status, ty)| {
                        (
                            status.clone(),
                            RefOr::Object(Response {
                                description: format!("{} response", status),
                                content: {
                                    let mut content = BTreeMap::new();
                                    content.insert(
                                        "application/json".to_string(),
                                        MediaType {
                                            schema: Some(SchemaObject::new_ref(ty.to_string())),
                                            example: None,
                                            examples: None,
                                            encoding: BTreeMap::default(),
                                            extensions: BTreeMap::default(),
                                        },
                                    );
                                    content
                                },
                                headers: BTreeMap::default(),
                                links: BTreeMap::default(),
                                extensions: Object::default(),
                            }),
                        )
                    })
                    .collect(),
                ..Default::default()
            },
            deprecated: false,
            security: None,
            servers: None,
            extensions: Object::default(),
            ..Default::default()
        };

        match route.method.as_str() {
            "get" => path_item.get = Some(operation),
            "post" => path_item.post = Some(operation),
            "put" => path_item.put = Some(operation),
            "delete" => path_item.delete = Some(operation),
            _ => {
                return Err(s_err(
                    span,
                    "Unsupported HTTP method ".to_string() + &route.method,
                ))
            }
        }

        openapi.paths.insert(route.route.clone(), path_item);
    }

    let spec_string = serde_yaml::to_string(&openapi)
        .map_err(|e| s_err(span, format!("Failed to serialize OpenAPI spec: {}", e)))?;

    let mut file = std::fs::File::create(path)
        .map_err(|e| s_err(span, format!("Failed to create file: {}", e)))?;
    file.write_all(spec_string.as_bytes())
        .map_err(|e| s_err(span, format!("Failed to write to file: {}", e)))?;

    Ok(quote! {})
}

struct GenArgs {
    title: syn::LitStr,
    version: syn::LitStr,
    path: syn::LitStr,
}

impl Parse for GenArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let title = input.parse()?;
        <syn::Token![,]>::parse(input)?;
        let version = input.parse()?;
        <syn::Token![,]>::parse(input)?;
        let path = input.parse()?;
        Ok(GenArgs {
            title,
            version,
            path,
        })
    }
}
