use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::vec;
use syn::{bracketed, punctuated::Punctuated, spanned::Spanned, token::Comma, Type};

use crate::parsing::s_err;

pub fn inner(args: TokenStream, input: TokenStream) -> syn::Result<TokenStream> {
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

pub struct MetadataAttr {
    pub custom: Vec<String>,
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

pub struct MetaAttr {
    pub into: Option<syn::Type>,
    pub skip: bool,
    pub optional: bool,
}

/// Parses Meta information a field by going through it's attributes
pub fn parse_field_attr(attrs: &[syn::Attribute]) -> syn::Result<MetaAttr> {
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
