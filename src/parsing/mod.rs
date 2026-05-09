pub(crate) mod generate;
pub(crate) mod metadata;
pub(crate) mod rust;

pub fn s_err(span: proc_macro2::Span, msg: impl std::fmt::Display) -> syn::Error {
    syn::Error::new(span, msg)
}
