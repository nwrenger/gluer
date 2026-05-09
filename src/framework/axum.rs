use proc_macro2::TokenStream;
use quote::quote;

use crate::{
    framework::RouteParamKind,
    parsing::{generate::MethodRouter, rust::RustType},
};

pub(crate) struct Axum;

impl Axum {
    pub fn is_skip_type(ident: &syn::Ident) -> bool {
        matches!(
            ident.to_string().as_str(),
            "State" | "Headers" | "Bytes" | "Request" | "Extension"
        )
    }

    pub fn is_extractor_type(ident: &syn::Ident) -> bool {
        matches!(ident.to_string().as_str(), "Json" | "Path" | "Query")
    }

    pub fn classify_route_param(ty: &RustType) -> Option<RouteParamKind> {
        match ty {
            RustType::Generic(name, _) if name == "Json" => Some(RouteParamKind::Body),

            RustType::Generic(name, inner) if name == "Path" => {
                if matches!(inner.first(), Some(RustType::Tuple(_))) {
                    Some(RouteParamKind::PathTuple)
                } else {
                    Some(RouteParamKind::Path)
                }
            }

            RustType::Generic(name, inner) if name == "Query" => {
                if matches!(
                    inner.first(),
                    Some(RustType::Generic(inner_name, _)) if inner_name == "HashMap"
                ) {
                    Some(RouteParamKind::QueryMap)
                } else {
                    Some(RouteParamKind::Query)
                }
            }

            _ => None,
        }
    }

    pub fn router_tokens(routes: &[MethodRouter]) -> TokenStream {
        let routes = routes.iter().map(|route| {
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

        quote! {
            Router::new()
            #(#routes)*
        }
    }
}
