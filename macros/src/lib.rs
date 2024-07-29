use proc_macro as pc;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::fmt;
use syn::{parenthesized, parse::Parse, spanned::Spanned};

fn s_err(span: proc_macro2::Span, msg: impl fmt::Display) -> syn::Error {
    syn::Error::new(span, msg)
}

fn logic_err(span: proc_macro2::Span) -> syn::Error {
    s_err(
        span,
        "Fatal logic error when trying to extract data from rust types",
    )
}

/// Adds a route to the router. Use for each api endpoint you want to expose to the frontend.
/// `Inline Functions` are currently not supported.
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

    let routes_ident = args.routes_ident;
    let app_ident = args.app_ident;
    let route = args.route;
    let handler = args.handler;

    let mut routes = Vec::new();

    for MethodCall { method, r#fn } in &handler {
        let fn_name = r#fn
            .segments
            .last()
            .ok_or_else(|| logic_err(span))?
            .ident
            .to_string();

        routes.push(Route {
            route: route.clone(),
            method: method.to_string(),
            fn_name,
        });
    }

    Ok(quote! {
        #app_ident = #app_ident.route(#route, #(#handler).*);
        #routes_ident.extend_from_slice(&[#(#routes),*]);
    })
}

struct RouterArgs {
    routes_ident: syn::Ident,
    app_ident: syn::Ident,
    route: String,
    handler: Vec<MethodCall>,
}

impl Parse for RouterArgs {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let routes_ident = input.parse()?;
        input.parse::<syn::Token![,]>()?;
        let app_ident = input.parse()?;
        input.parse::<syn::Token![,]>()?;
        let route = input.parse::<syn::LitStr>()?.value();
        input.parse::<syn::Token![,]>()?;
        let handler = input.parse_terminated(MethodCall::parse, syn::Token![.])?;
        let handler: Vec<MethodCall> = handler.into_iter().collect();

        Ok(RouterArgs {
            routes_ident,
            app_ident,
            route,
            handler,
        })
    }
}

struct MethodCall {
    method: syn::Ident,
    r#fn: syn::Path,
}

impl Parse for MethodCall {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let method: syn::Ident = input.parse()?;
        let content;
        parenthesized!(content in input);
        let r#fn: syn::Path = content.parse()?;

        Ok(MethodCall { method, r#fn })
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

struct Route {
    route: String,
    method: String,
    fn_name: String,
}

impl ToTokens for Route {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let route = &self.route;
        let method = &self.method;
        let fn_name = &self.fn_name;

        tokens.extend(quote! {
            (#route, #method, #fn_name)
        });
    }
}

/// Put before structs or functions to be usable by the `glue` crate.
#[proc_macro_attribute]
pub fn cached(args: pc::TokenStream, input: pc::TokenStream) -> pc::TokenStream {
    match cached_inner(args.into(), input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn cached_inner(args: TokenStream, input: TokenStream) -> syn::Result<TokenStream> {
    let span = input.span();
    let item = syn::parse2::<syn::Item>(input.clone())?;
    let _args = syn::parse2::<NoArgs>(args)?;

    let generated_const = match item.clone() {
        syn::Item::Struct(item_struct) => generate_struct_const(item_struct)?,
        syn::Item::Fn(item_fn) => generate_fn_const(item_fn)?,
        _ => return Err(s_err(span, "Expected struct or function")),
    };

    Ok(quote! {
        #generated_const
        #item
    })
}

fn generate_struct_const(item_struct: syn::ItemStruct) -> syn::Result<TokenStream> {
    let struct_name = item_struct.ident.to_string();
    let fields = item_struct
        .fields
        .into_iter()
        .map(|field| {
            let ident = field
                .ident
                .clone()
                .ok_or_else(|| s_err(field.span(), "Unnamed field not supported"))?
                .to_string();
            let ty = field.ty.into_token_stream().to_string();
            Ok((ident, ty))
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let const_ident = syn::Ident::new(
        &format!("STRUCT_{}", struct_name.to_uppercase()),
        proc_macro2::Span::call_site(),
    );

    let const_value = fields.iter().map(|(ident, ty)| {
        quote! { (#ident, #ty) }
    });

    Ok(quote! {
        pub const #const_ident: (&str, &[(&str, &str)]) = (#struct_name, &[#(#const_value),*]);
    })
}

fn generate_fn_const(item_fn: syn::ItemFn) -> syn::Result<TokenStream> {
    let fn_name = item_fn.sig.ident.to_string();
    let params = item_fn
        .sig
        .inputs
        .iter()
        .map(|param| match param {
            syn::FnArg::Typed(syn::PatType { pat, ty, .. }) => {
                let pat = pat.to_token_stream().to_string();
                let ty = ty.to_token_stream().to_string();
                Ok((pat, ty))
            }
            syn::FnArg::Receiver(_) => Err(s_err(param.span(), "Receiver parameter not allowed")),
        })
        .collect::<syn::Result<Vec<_>>>()?;

    let response = match &item_fn.sig.output {
        syn::ReturnType::Type(_, ty) => ty.into_token_stream().to_string(),
        syn::ReturnType::Default => "()".to_string(),
    };

    let const_ident = syn::Ident::new(
        &format!("FN_{}", fn_name.to_uppercase()),
        proc_macro2::Span::call_site(),
    );

    let const_value = params.iter().map(|(pat, ty)| {
        quote! { (#pat, #ty) }
    });

    Ok(quote! {
        pub const #const_ident: (&str, &[(&str, &str)], &str) = (#fn_name, &[#(#const_value),*], #response);
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
