use proc_macro::{self as pc};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::{collections::HashMap, fmt, vec};
use syn::{parenthesized, parse::Parse, spanned::Spanned, TypeParam};

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
    let ExtractArgs {
        routes: original_routes,
    } = syn::parse2::<ExtractArgs>(input.clone())?;

    let routes = original_routes.iter().map(|Route { method, handler }| {
        let method_name = method.to_string();
        let handler_name = handler.to_string();

        let fn_info = syn::Ident::new(&handler_name, proc_macro2::Span::call_site());
        let fn_info = quote! { #fn_info::metadata() };

        quote! {
            {
                const ROUTE: gluer::Route<'static> = gluer::Route {
                    url: "",
                    method: #method_name,
                    fn_name: #handler_name,
                    fn_info: #fn_info,
                };
                ROUTE
            }
        }
    });

    Ok(quote! { ( #(#original_routes).*, &[#(#routes,)*] )})
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

impl ToTokens for Route {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let method = &self.method;
        let handler = &self.handler;
        tokens.extend(quote! { #method(#handler::#handler) });
    }
}

/// Put before structs, functions or enums to generated metadata
/// which will be later used by the api via `extract!`.
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
    let _args = syn::parse2::<NoArgs>(args)?;

    let out = match item {
        syn::Item::Struct(item_struct) => generate_struct(item_struct)?,
        syn::Item::Enum(item_enum) => generate_enum(item_enum)?,
        syn::Item::Fn(item_fn) => generate_function(item_fn)?,
        _ => return Err(s_err(span, "Expected struct, function or enum")),
    };

    Ok(quote! {
        #out
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

fn generate_struct(mut item_struct: syn::ItemStruct) -> syn::Result<TokenStream> {
    let struct_name_ident = item_struct.ident.clone();
    let generics_ident_no_types =
        if let Some(g) = extract_type_params_as_type(&item_struct.generics)? {
            quote! { #g }
        } else {
            quote! {}
        };
    let generics_ident = item_struct.generics.clone();
    let struct_name = struct_name_ident.to_string();
    let vis = &item_struct.vis;
    let generics: Vec<String> = item_struct
        .generics
        .params
        .iter()
        .filter_map(|generic| {
            if let syn::GenericParam::Type(TypeParam { ident, .. }) = generic {
                return Some(ident.to_string());
            }
            None
        })
        .collect();

    let mut dependencies: Vec<String> = Vec::new();

    let item_struct_fields = item_struct.fields.clone();

    let fields = item_struct_fields
        .iter()
        .enumerate()
        .filter_map(|(i, field)| {
            let ident = match field.ident.clone() {
                Some(ident) => ident.to_string(),
                None => {
                    return Some(Err(syn::Error::new(
                        field.span(),
                        "Unnamed field not supported",
                    )))
                }
            };

            let meta_attr = match parse_field_attr(&field.attrs) {
                Ok(meta_attr) => meta_attr,
                Err(e) => return Some(Err(e)),
            };

            let MetaAttr { into, skip } = meta_attr;

            // Clean off all "meta" attributes
            if let Some(field) = item_struct.fields.iter_mut().nth(i) {
                field.attrs.retain(|attr| !attr.path().is_ident("meta"));
            }

            let field_ty = if let Some(conv_fn) = into.clone() {
                conv_fn.to_token_stream().to_string()
            } else {
                field.ty.to_token_stream().to_string()
            };

            if skip {
                return None;
            }

            if into.is_none() {
                match check(&field.ty, field.span(), &mut dependencies, &generics) {
                    Ok(_) => {}
                    Err(e) => return Some(Err(e)),
                };
                fn check(
                    ty: &syn::Type,
                    span: proc_macro2::Span,
                    dependencies: &mut Vec<String>,
                    generics: &Vec<String>,
                ) -> syn::Result<()> {
                    match check_rust_type(ty) {
                        Some(RustType::Custom(inner_ty))
                        | Some(RustType::CustomGeneric(inner_ty, _)) => {
                            if !dependencies.contains(&inner_ty) && !generics.contains(&inner_ty) {
                                dependencies.push(inner_ty);
                            }
                        }

                        Some(RustType::Generic(_, inner_tys)) => {
                            for inner in inner_tys {
                                let ty =
                                    syn::parse_str::<syn::Type>(&inner.get_tokens().to_string())?;
                                check(&ty, span, dependencies, generics)?;
                            }
                        }

                        Some(_) => {}
                        None => return Err(syn::Error::new(span, "Unsupported field type")),
                    }
                    Ok(())
                }
            }

            Some(Ok((ident, field_ty)))
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let const_value = fields.iter().map(|(ident, ty)| {
        quote! { gluer::Field { name: #ident, ty: #ty } }
    });

    let dependencies_quote = dependencies.iter().map(|struct_name| {
        let struct_ident = syn::Ident::new(struct_name, proc_macro2::Span::call_site());
        quote! { #struct_ident::metadata() }
    });

    let generics_quote = generics.iter().map(|generic| {
        quote! { #generic }
    });

    let item_struct = quote! { #item_struct };

    Ok(quote! {
        #item_struct

        impl #generics_ident #struct_name_ident #generics_ident_no_types {
            #vis const fn metadata() -> gluer::TypeInfo<'static> {
                const TYPE_INFO: gluer::TypeInfo<'static> = gluer::TypeInfo {
                    name: #struct_name,
                    generics: &[#(#generics_quote),*],
                    fields: &[#(#const_value),*],
                    dependencies: &[#(#dependencies_quote),*],
                    is_enum: false,
                };
                TYPE_INFO
            }
        }
    })
}

fn extract_type_params_as_type(generics: &syn::Generics) -> syn::Result<Option<syn::Generics>> {
    let type_params: Vec<String> = generics
        .type_params()
        .map(|type_param| type_param.ident.to_string())
        .collect();

    if type_params.is_empty() {
        return Ok(None);
    }

    Ok(Some(syn::parse_str(&format!(
        "<{}>",
        type_params.join(", ")
    ))?))
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
            Err(meta.error("expected #[meta(into = Type)] or #[meta(skip)]"))
        })?;
    }

    Ok(meta_attr)
}

fn generate_enum(item_enum: syn::ItemEnum) -> syn::Result<TokenStream> {
    if !item_enum.generics.params.is_empty() {
        return Err(syn::Error::new(
            item_enum.generics.span(),
            "Generics and Lifetimes not supported for enums",
        ));
    }

    let enum_name_ident = item_enum.ident.clone();
    let enum_name = enum_name_ident.to_string();
    let vis = &item_enum.vis;

    let variants = item_enum
        .variants
        .iter()
        .map(|variant| {
            if !variant.fields.is_empty() {
                return Err(syn::Error::new(
                    variant.fields.span(),
                    "Enums with values are not supported",
                ));
            }
            let ident = variant.ident.to_string();
            Ok(quote! {gluer::Field { name: #ident, ty: "" }})
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let item_enum = quote! { #item_enum };

    Ok(quote! {
        #item_enum

        impl #enum_name_ident {
            #vis const fn metadata() -> gluer::TypeInfo<'static> {
                const TYPE_INFO: gluer::TypeInfo<'static> = gluer::TypeInfo {
                    name: #enum_name,
                    generics: &[],
                    fields: &[#(#variants),*],
                    dependencies: &[],
                    is_enum: true,
                };
                TYPE_INFO
            }
        }
    })
}

fn generate_function(item_fn: syn::ItemFn) -> syn::Result<proc_macro2::TokenStream> {
    let fn_name_ident = item_fn.sig.ident.clone();
    let vis = &item_fn.vis;
    let mut structs = HashMap::new();

    let params = item_fn
        .sig
        .inputs
        .iter()
        .filter_map(|param| match param {
            syn::FnArg::Typed(syn::PatType { pat, ty, .. }) => {
                let pat = pat.to_token_stream().to_string();
                if let Some(rust_type) = check_rust_type(ty) {
                    process_rust_type(&rust_type, &mut structs);

                    Some(Ok((pat, rust_type)))
                } else {
                    None
                }
            }
            syn::FnArg::Receiver(_) => Some(Err(syn::Error::new(
                param.span(),
                "Receiver parameter not allowed",
            ))),
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let response = match &item_fn.sig.output {
        syn::ReturnType::Type(_, ty) => {
            if let Some(rust_type) = check_rust_type(ty) {
                process_rust_type(&rust_type, &mut structs);

                rust_type
            } else {
                return Err(syn::Error::new(ty.span(), "Unsupported return type"));
            }
        }
        syn::ReturnType::Default => RustType::BuiltIn("()".to_string()),
    };

    let params_types = params.iter().map(|(pat, ty)| {
        let ty = ty.to_token_stream().to_string();
        quote! { gluer::Field { name: #pat, ty: #ty } }
    });

    let response = {
        let ty = response.to_token_stream().to_string();
        quote! { #ty }
    };

    let structs_quote = structs
        .iter()
        .map(|(struct_name, generics_info)| generate_struct_metadata(struct_name, generics_info))
        .collect::<Result<Vec<_>, syn::Error>>()?;

    Ok(quote! {
        #[allow(non_camel_case_types, missing_docs)]
        #vis struct #fn_name_ident;

        impl #fn_name_ident {
            #item_fn

            #vis const fn metadata() -> gluer::FnInfo<'static> {
                const FN_INFO: gluer::FnInfo<'static> = gluer::FnInfo {
                    params: &[#(#params_types),*],
                    response: #response,
                    structs: &[#(#structs_quote),*]
                };
                FN_INFO
            }
        }
    })
}

fn process_rust_type(rust_type: &RustType, structs: &mut HashMap<String, Vec<RustType>>) {
    match rust_type {
        RustType::Custom(inner_ty) => {
            if !structs.contains_key(inner_ty) {
                structs.entry(inner_ty.clone()).or_default();
            }
        }
        RustType::CustomGeneric(outer_ty, inner_tys) => {
            if !structs.contains_key(outer_ty) {
                structs
                    .entry(outer_ty.clone())
                    .or_default()
                    .extend(inner_tys.clone());
            }
            for inner_ty in inner_tys {
                process_rust_type(inner_ty, structs);
            }
        }
        RustType::Tuple(inner_tys) => {
            for inner_ty in inner_tys {
                process_rust_type(inner_ty, structs);
            }
        }
        RustType::Generic(_, inner_tys) => {
            for inner_ty in inner_tys {
                process_rust_type(inner_ty, structs);
            }
        }
        _ => {}
    }
}
fn generate_struct_metadata(
    struct_name: &str,
    generics_info: &[RustType],
) -> syn::Result<proc_macro2::TokenStream> {
    let struct_ident = syn::Ident::new(struct_name, proc_macro2::Span::call_site());

    let times = generics_info.len();
    let generics_placeholder = (0..times).map(|_| quote! { () });

    Ok(quote! {
        #struct_ident::<#(#generics_placeholder),*>::metadata()
    })
}

const RUST_TYPES: &[&str] = &[
    "bool", "char", "str", "u8", "u16", "u32", "u64", "u128", "i8", "i16", "i32", "i64", "i128",
    "usize", "isize", "f32", "f64", "String",
];

const SKIP_TYPES: &[&str] = &["State", "Headers", "Bytes", "Request", "Extension"];

const BUILTIN_GENERICS: &[&str] = &[
    "Query", "HashMap", "Path", "Vec", "Json", "Option", "Result",
];

#[derive(Debug, PartialEq, Clone)]
enum RustType {
    BuiltIn(String),
    Generic(String, Vec<RustType>),
    Tuple(Vec<RustType>),
    Custom(String),
    CustomGeneric(String, Vec<RustType>),
}

impl RustType {
    fn get_tokens(&self) -> TokenStream {
        let mut tokens = TokenStream::new();
        self.to_tokens(&mut tokens);
        tokens
    }
}

impl ToTokens for RustType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            RustType::BuiltIn(name) => {
                let ty = syn::parse_str::<syn::Type>(name).unwrap();
                tokens.extend(quote! { #ty });
            }
            RustType::Generic(name, inner) => {
                let inner = inner.iter().map(|inner| {
                    let ty = syn::parse_str::<syn::Type>(&inner.get_tokens().to_string()).unwrap();
                    quote! { #ty }
                });
                let ty = syn::parse_str::<syn::Type>(name).unwrap();
                tokens.extend(quote! { #ty<#(#inner),*> });
            }
            RustType::Tuple(inner) => {
                let inner = inner.iter().map(|inner| {
                    let ty = syn::parse_str::<syn::Type>(&inner.get_tokens().to_string()).unwrap();
                    quote! { #ty }
                });
                tokens.extend(quote! { (#(#inner),*) });
            }
            RustType::Custom(name) => {
                let ty = syn::parse_str::<syn::Type>(name).unwrap();
                tokens.extend(quote! { #ty });
            }
            RustType::CustomGeneric(name, inner) => {
                let inner = inner.iter().map(|inner| {
                    let ty = syn::parse_str::<syn::Type>(&inner.get_tokens().to_string()).unwrap();
                    quote! { #ty }
                });
                let ty = syn::parse_str::<syn::Type>(name).unwrap();
                tokens.extend(quote! { #ty<#(#inner),*> });
            }
        }
    }
}

fn is_builtin_type(ident: &syn::Ident) -> bool {
    RUST_TYPES.contains(&ident.to_string().as_str())
}

fn is_skip_type(ident: &syn::Ident) -> bool {
    SKIP_TYPES.contains(&ident.to_string().as_str())
}

fn check_rust_type(ty: &syn::Type) -> Option<RustType> {
    match ty {
        syn::Type::Path(type_path) => {
            let segment = type_path.path.segments.last().unwrap();
            let ident = &segment.ident;

            if is_builtin_type(ident) {
                Some(RustType::BuiltIn(ident.to_string()))
            } else if is_skip_type(ident) {
                None
            } else if BUILTIN_GENERICS.contains(&ident.to_string().as_str()) {
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    let inner_types: Vec<RustType> = args
                        .args
                        .iter()
                        .filter_map(|arg| {
                            if let syn::GenericArgument::Type(inner_ty) = arg {
                                check_rust_type(inner_ty)
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
                            check_rust_type(inner_ty)
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
        | syn::Type::Group(syn::TypeGroup { elem, .. }) => check_rust_type(elem),

        syn::Type::Tuple(type_tuple) => {
            if type_tuple.elems.is_empty() {
                return Some(RustType::BuiltIn("()".to_string()));
            }
            let inner_types: Vec<RustType> = type_tuple
                .elems
                .iter()
                .filter_map(check_rust_type)
                .collect();
            Some(RustType::Tuple(inner_types))
        }
        syn::Type::Slice(syn::TypeSlice { elem, .. })
        | syn::Type::Array(syn::TypeArray { elem, .. }) => {
            check_rust_type(elem).map(|inner| RustType::Generic("Vec".to_string(), vec![inner]))
        }
        _ => None,
    }
}
