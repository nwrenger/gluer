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
    let item = syn::parse2::<syn::Item>(input)?;
    let _args = syn::parse2::<NoArgs>(args)?;

    let (generated_const, ret) = match item {
        syn::Item::Struct(item_struct) => generate_struct_const(item_struct)?,
        syn::Item::Fn(item_fn) => generate_fn_const(item_fn)?,
        _ => return Err(s_err(span, "Expected struct or function")),
    };

    Ok(quote! {
        #generated_const
        #ret
    })
}

fn generate_struct_const(
    mut item_struct: syn::ItemStruct,
) -> syn::Result<(TokenStream, TokenStream)> {
    let struct_name = item_struct.ident.to_string();
    let vis = &item_struct.vis;
    let mut dependencies = HashMap::new();

    let fields = item_struct
        .fields
        .iter_mut()
        .map(|field| {
            let ident = field
                .ident
                .clone()
                .ok_or_else(|| s_err(field.span(), "Unnamed field not supported"))?
                .to_string();

            let conversion_fn = parse_field_attr(&field.attrs)?;

            // clean off all "info" attributes
            field.attrs = field
                .attrs
                .iter()
                .filter(|attr| attr.path().is_ident("info"))
                .cloned()
                .collect();

            let field_ty = if let Some(conv_fn) = conversion_fn.clone() {
                conv_fn.to_token_stream().to_string()
            } else {
                field.ty.to_token_stream().to_string()
            };

            if conversion_fn.is_none() {
                if let Some((is_basic, _, inner_ty)) = basic_rust_type(&field.ty)? {
                    if !is_basic {
                        dependencies.insert(
                            inner_ty.clone(),
                            format!("STRUCT_{}", inner_ty.to_uppercase()),
                        );
                    }
                } else {
                    return Err(s_err(field.span(), "Unsupported field type"));
                }
            }

            Ok((ident, field_ty))
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let const_ident = syn::Ident::new(
        &format!("STRUCT_{}", struct_name.to_uppercase()),
        proc_macro2::Span::call_site(),
    );

    let const_value = fields.iter().map(|(ident, ty)| {
        quote! { gluer::Field { name: #ident, ty: #ty } }
    });

    let dependencies_quote = dependencies.values().map(|struct_name| {
        let struct_const = syn::Ident::new(struct_name, proc_macro2::Span::call_site());
        quote! { #struct_const }
    });

    let item_struct = quote! { #item_struct };

    Ok((
        quote! {
            #vis const #const_ident: gluer::StructInfo = gluer::StructInfo { name: #struct_name, fields: &[#(#const_value),*], dependencies: &[#(#dependencies_quote),*] };
        },
        item_struct,
    ))
}

fn parse_field_attr(attrs: &[syn::Attribute]) -> syn::Result<Option<syn::Type>> {
    if let Some(attr) = attrs.iter().next() {
        if !attr.path().is_ident("into") {
            return Ok(None);
        }

        if let syn::Meta::List(meta) = &attr.meta {
            let path: syn::Expr = meta.parse_args()?;
            let path = syn::parse2::<syn::Type>(quote!(#path))?;
            return Ok(Some(path.clone()));
        }

        if let syn::Meta::NameValue(meta) = &attr.meta {
            if let syn::Expr::Lit(expr) = &meta.value {
                if let syn::Lit::Str(lit_str) = &expr.lit {
                    return lit_str.parse().map(Some);
                }
            }
        }

        let message = "expected #[into = \"...\"]";
        return Err(syn::Error::new_spanned(attr, message));
    }
    Ok(None)
}

fn generate_fn_const(
    item_fn: syn::ItemFn,
) -> syn::Result<(proc_macro2::TokenStream, proc_macro2::TokenStream)> {
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

    let structs_quote = structs.values().map(|struct_name| {
        let struct_ident = syn::Ident::new(struct_name, proc_macro2::Span::call_site());
        quote! { #struct_ident }
    });

    let item_fn = quote! { #item_fn };

    Ok((
        quote! {
            #vis const #const_ident: gluer::FnInfo = gluer::FnInfo {
                params: &[#(#const_value),*],
                response: #response,
                structs: &[#(#structs_quote),*]
            };
        },
        item_fn,
    ))
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
        syn::Type::Reference(syn::TypeReference { elem, .. })
        | syn::Type::Paren(syn::TypeParen { elem, .. })
        | syn::Type::Group(syn::TypeGroup { elem, .. }) => return basic_rust_type(elem),
        syn::Type::Tuple(elems) => {
            if elems.elems.len() == 1 {
                return basic_rust_type(&elems.elems[0]);
            } else if elems.elems.is_empty() {
                return Ok(Some((true, "()".to_string(), "()".to_string())));
            }
        }
        syn::Type::Array(syn::TypeArray { elem, .. })
        | syn::Type::Slice(syn::TypeSlice { elem, .. }) => {
            if let Some((is_basic, outer_ty, inner_ty)) = basic_rust_type(elem)? {
                let vec_type = format!("Vec<{}>", outer_ty);
                return Ok(Some((is_basic, vec_type, inner_ty)));
            } else {
                return Ok(None);
            }
        }
        _ => {}
    }
    Err(s_err(
        proc_macro2::Span::call_site(),
        format!("Failed to parse type {}", ty_str),
    ))
}
