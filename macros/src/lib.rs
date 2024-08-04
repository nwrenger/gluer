use proc_macro::{self as pc};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens, TokenStreamExt};
use std::{collections::HashMap, fmt, vec};
use syn::{
    bracketed, parenthesized, parse::Parse, punctuated::Punctuated, spanned::Spanned, token::Comma,
    Type,
};

fn s_err(span: proc_macro2::Span, msg: impl fmt::Display) -> syn::Error {
    syn::Error::new(span, msg)
}

/// Extract the metadata of `axum`'s `MethodRouter` syntax. Use inside the `Api::route` function.
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
            gluer::Route {
                url: String::new(),
                method: String::from(#method_name),
                fn_name: String::from(#handler_name),
                fn_info: #fn_info,
            }
        }
    });

    Ok(quote! { ( #(#original_routes).*, vec![#(#routes,)*] )})
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

/// Use before structs, functions, enums or types to generate metadata for
/// the API via `extract!` or for dependent elements.
///
/// ## Attributes
/// - `custom = [Type, *]`: Specify here types which are named equally to std types but are custom.
///
/// ## Struct Attributes
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
    let args = syn::parse2::<MetadataAttr>(args)?;

    let out = match item {
        syn::Item::Struct(item_struct) => generate_struct(item_struct, args)?,
        syn::Item::Enum(item_enum) => generate_enum(item_enum, args)?,
        syn::Item::Type(item_type) => generate_type(item_type, args)?,
        syn::Item::Fn(item_fn) => generate_function(item_fn, args)?,
        _ => return Err(s_err(span, "Expected struct, function, enum or type")),
    };

    Ok(quote! {
        #out
    })
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
                            _ => return Err(s_err(ty.span(), "expected the type")),
                        }
                    }
                }
                _ => return Err(s_err(ident.span(), "unknown argument")),
            };
            if !input.is_empty() {
                <syn::Token![,]>::parse(input)?;
            }
        }

        Ok(ret)
    }
}

fn generate_struct(
    mut item_struct: syn::ItemStruct,
    metadata_attr: MetadataAttr,
) -> syn::Result<TokenStream> {
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
        .type_params()
        .map(|type_param| type_param.ident.to_string())
        .collect();

    let mut dependencies: HashMap<String, Vec<RustType>> = HashMap::new();

    let item_struct_fields = item_struct.fields.clone();

    let fields = item_struct_fields
        .iter()
        .enumerate()
        .filter_map(|(i, field)| {
            let ident = match field.ident.clone() {
                Some(ident) => ident.to_string(),
                None => return Some(Err(s_err(field.span(), "Unnamed field not supported"))),
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
                conv_fn
            } else {
                field.ty.clone()
            };

            if skip {
                return None;
            }

            if let Some(ty) = check_rust_type(&field_ty, &metadata_attr.custom) {
                process_rust_type(&ty, &mut dependencies, &generics);
                Some(Ok((ident, ty)))
            } else {
                Some(Err(s_err(field.span(), "Unsupported field type")))
            }
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let generics_quote = generics.iter().map(|generic| {
        quote! { String::from(#generic) }
    });

    let fields_quote = fields.iter().map(|(ident, ty)| {
        quote! { gluer::Field { name: String::from(#ident), ty: #ty } }
    });

    let dependencies_quote = dependencies
        .iter()
        .map(|(struct_name, generics_info)| generate_type_metadata(struct_name, generics_info))
        .collect::<Result<Vec<_>, syn::Error>>()?;

    let item_struct = quote! { #item_struct };

    Ok(quote! {
        #item_struct

        impl #generics_ident #struct_name_ident #generics_ident_no_types {
            #vis fn metadata() -> gluer::TypeCategory {
                gluer::TypeCategory::Struct(
                    gluer::TypeInfo {
                        name: String::from(#struct_name),
                        generics: vec![#(#generics_quote),*],
                        fields: vec![#(#fields_quote),*],
                        dependencies: vec![#(#dependencies_quote),*],
                    }
                )
            }
        }
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
            Err(meta.error("expected #[meta(into = Type)] or #[meta(skip)]"))
        })?;
    }

    Ok(meta_attr)
}

fn generate_enum(item_enum: syn::ItemEnum, _: MetadataAttr) -> syn::Result<TokenStream> {
    if !item_enum.generics.params.is_empty() {
        return Err(s_err(
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
                return Err(s_err(
                    variant.fields.span(),
                    "Enums with values are not supported",
                ));
            }
            let ident = variant.ident.to_string();
            Ok(quote! { gluer::Field { name: String::from(#ident), ty: gluer::RustType::None }})
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let item_enum = quote! { #item_enum };

    Ok(quote! {
        #item_enum

        impl #enum_name_ident {
            #vis fn metadata() -> gluer::TypeCategory {
                gluer::TypeCategory::Enum(
                    gluer::TypeInfo {
                        name: String::from(#enum_name),
                        generics: vec![],
                        fields: vec![#(#variants),*],
                        dependencies: vec![],
                    }
                )
            }
        }
    })
}

fn generate_type(
    item_type: syn::ItemType,
    metadata_attr: MetadataAttr,
) -> syn::Result<TokenStream> {
    let type_name_ident = item_type.ident.clone();
    let type_name = type_name_ident.to_string();
    let vis = &item_type.vis;
    let generics_ident_no_types = if let Some(g) = extract_type_params_as_type(&item_type.generics)?
    {
        quote! { #g }
    } else {
        quote! {}
    };
    let generics_ident = item_type.generics.clone();
    let generics: Vec<String> = item_type
        .generics
        .type_params()
        .map(|type_param| type_param.ident.to_string())
        .collect();

    let mut dependencies: HashMap<String, Vec<RustType>> = HashMap::new();

    let ty = check_rust_type(&item_type.ty, &metadata_attr.custom)
        .ok_or_else(|| s_err(item_type.ty.span(), "Unsupported type"))?;

    process_rust_type(&ty, &mut dependencies, &generics);

    let trait_ident = syn::Ident::new(
        &format!("{}Metadata", type_name),
        proc_macro2::Span::call_site(),
    );

    let generics_quote = generics.iter().map(|generic| {
        quote! { String::from(#generic) }
    });

    let dependencies_quote = dependencies
        .iter()
        .map(|(struct_name, generics_info)| generate_type_metadata(struct_name, generics_info))
        .collect::<Result<Vec<_>, syn::Error>>()?;

    Ok(quote! {
        #item_type

        #vis trait #trait_ident {
            fn metadata() -> gluer::TypeCategory;
        }

        impl #generics_ident #trait_ident for #type_name_ident #generics_ident_no_types {
            fn metadata() -> gluer::TypeCategory {
                gluer::TypeCategory::Type(
                    gluer::TypeInfo {
                        name: String::from(#type_name),
                        generics: vec![#(#generics_quote),*],
                        fields: vec![gluer::Field { name: String::new(), ty: #ty }],
                        dependencies: vec![#(#dependencies_quote)*],
                    }
                )
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

fn generate_function(
    item_fn: syn::ItemFn,
    metadata_attr: MetadataAttr,
) -> syn::Result<proc_macro2::TokenStream> {
    let fn_name_ident = item_fn.sig.ident.clone();
    let vis = &item_fn.vis;
    let mut dependencies = HashMap::new();

    let params = item_fn
        .sig
        .inputs
        .iter()
        .filter_map(|param| match param {
            syn::FnArg::Typed(syn::PatType { pat, ty, .. }) => {
                let pat = pat.to_token_stream().to_string();
                if let Some(rust_type) = check_rust_type(ty, &metadata_attr.custom) {
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
            if let Some(rust_type) = check_rust_type(ty, &metadata_attr.custom) {
                process_rust_type(&rust_type, &mut dependencies, &[]);

                rust_type
            } else {
                return Err(s_err(ty.span(), "Unsupported return type"));
            }
        }
        syn::ReturnType::Default => RustType::BuiltIn("()".to_string()),
    };

    let params_types = params.iter().map(|(pat, ty)| {
        quote! { gluer::Field { name: String::from(#pat), ty: #ty } }
    });

    let dependencies_quote = dependencies
        .iter()
        .map(|(struct_name, generics_info)| generate_type_metadata(struct_name, generics_info))
        .collect::<Result<Vec<_>, syn::Error>>()?;

    Ok(quote! {
        #[allow(non_camel_case_types, missing_docs)]
        #vis struct #fn_name_ident;

        impl #fn_name_ident {
            #item_fn

            #vis fn metadata() -> gluer::FnInfo {
                gluer::FnInfo {
                    params: vec![#(#params_types),*],
                    response: #response,
                    types: vec![#(#dependencies_quote),*],
                }
            }
        }
    })
}

fn process_rust_type(
    rust_type: &RustType,
    dependencies: &mut HashMap<String, Vec<RustType>>,
    generics: &[String],
) {
    match rust_type {
        RustType::Custom(inner_ty) => {
            if !dependencies.contains_key(inner_ty) && !generics.contains(inner_ty) {
                dependencies.entry(inner_ty.clone()).or_default();
            }
        }
        RustType::CustomGeneric(outer_ty, inner_tys) => {
            if !dependencies.contains_key(outer_ty) && !generics.contains(outer_ty) {
                dependencies
                    .entry(outer_ty.clone())
                    .or_default()
                    .extend(inner_tys.clone());
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

fn generate_type_metadata(
    type_name: &str,
    generics_info: &[RustType],
) -> syn::Result<proc_macro2::TokenStream> {
    let struct_ident = syn::Ident::new(type_name, proc_macro2::Span::call_site());

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

impl ToTokens for RustType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match &self {
            RustType::BuiltIn(value) => {
                tokens.append_all(quote! { gluer::RustType::BuiltIn(String::from(#value)) });
            }
            RustType::Generic(name, types) => {
                let types = types.iter().map(|ty| {
                    quote! { #ty, }
                });
                tokens.append_all(
                    quote! { gluer::RustType::Generic(String::from(#name), vec![#(#types)*]) },
                );
            }
            RustType::Tuple(types) => {
                let types = types.iter().map(|ty| {
                    quote! { #ty, }
                });
                tokens.append_all(quote! { gluer::RustType::Tuple(vec![#(#types)*]) });
            }
            RustType::Custom(name) => {
                tokens.append_all(quote! { gluer::RustType::Custom(String::from(#name)) });
            }
            RustType::CustomGeneric(name, types) => {
                let types = types.iter().map(|ty| {
                    quote! { #ty, }
                });
                tokens.append_all(
                    quote! { gluer::RustType::CustomGeneric(String::from(#name), vec![#(#types)*]) },
                );
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

fn is_builtin_generic(ident: &syn::Ident) -> bool {
    BUILTIN_GENERICS.contains(&ident.to_string().as_str())
}

fn is_custom(ident: &syn::Ident, custom: &[String]) -> bool {
    custom.contains(&ident.to_string())
}

fn check_rust_type(ty: &syn::Type, custom: &[String]) -> Option<RustType> {
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
                                check_rust_type(inner_ty, custom)
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
                            check_rust_type(inner_ty, custom)
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
        | syn::Type::Group(syn::TypeGroup { elem, .. }) => check_rust_type(elem, custom),

        syn::Type::Tuple(type_tuple) => {
            if type_tuple.elems.is_empty() {
                return Some(RustType::BuiltIn("()".to_string()));
            }
            let inner_types: Vec<RustType> = type_tuple
                .elems
                .iter()
                .filter_map(|t| check_rust_type(t, custom))
                .collect();
            Some(RustType::Tuple(inner_types))
        }
        syn::Type::Slice(syn::TypeSlice { elem, .. })
        | syn::Type::Array(syn::TypeArray { elem, .. }) => check_rust_type(elem, custom)
            .map(|inner| RustType::Generic("Vec".to_string(), vec![inner])),
        _ => None,
    }
}
