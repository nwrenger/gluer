#[cfg(feature = "actix")]
pub(crate) mod actix;

#[cfg(feature = "axum")]
pub(crate) mod axum;

#[cfg(all(feature = "axum", feature = "actix"))]
compile_error!("features `axum` and `actix` cannot currently be enabled together");

#[cfg(not(any(feature = "axum", feature = "actix")))]
compile_error!("enable one framework feature: `axum` or `actix`");

use proc_macro2::TokenStream;

use crate::parsing::{generate::MethodRouter, rust::RustType};

pub struct Framework;

impl Framework {
    pub fn is_skip_type(ident: &syn::Ident) -> bool {
        #[cfg(feature = "axum")]
        {
            crate::framework::axum::Axum::is_skip_type(ident)
        }
        #[cfg(all(not(feature = "axum"), feature = "actix"))]
        {
            crate::framework::actix::Actix::is_skip_type(ident)
        }
    }

    pub fn is_extractor_type(ident: &syn::Ident) -> bool {
        #[cfg(feature = "axum")]
        {
            crate::framework::axum::Axum::is_extractor_type(ident)
        }
        #[cfg(all(not(feature = "axum"), feature = "actix"))]
        {
            crate::framework::actix::Actix::is_extractor_type(ident)
        }
    }

    pub fn classify_route_param(ty: &RustType) -> Option<RouteParamKind> {
        #[cfg(feature = "axum")]
        {
            crate::framework::axum::Axum::classify_route_param(ty)
        }
        #[cfg(all(not(feature = "axum"), feature = "actix"))]
        {
            crate::framework::actix::Actix::classify_route_param(ty)
        }
    }

    pub fn router_tokens(routes: &[MethodRouter]) -> TokenStream {
        #[cfg(feature = "axum")]
        {
            crate::framework::axum::Axum::router_tokens(routes)
        }
        #[cfg(all(not(feature = "axum"), feature = "actix"))]
        {
            crate::framework::actix::Actix::router_tokens(routes)
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RouteParamKind {
    Body,
    Path,
    PathTuple,
    Query,
    QueryMap,
}
