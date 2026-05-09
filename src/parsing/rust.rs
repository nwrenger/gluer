use quote::ToTokens;
use std::{fmt::Debug, vec};
use syn::{spanned::Spanned, LitStr};

use crate::{
    framework::Framework,
    parsing::{
        metadata::{parse_field_attr, MetaAttr, MetadataAttr},
        s_err,
    },
};

/// Function information.
#[derive(Clone, Debug)]
pub struct FnInfo {
    pub name: String,
    pub params: Vec<Field>,
    pub response: RustType,
    pub types: Vec<String>,
    pub docs: Vec<String>,
}

impl FnInfo {
    /// Generates function info from token input.
    pub fn from_tokens(item_fn: syn::ItemFn, metadata_attr: MetadataAttr) -> syn::Result<FnInfo> {
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
}

/// Information type.
#[derive(Clone, Debug)]
pub enum TypeCategory {
    Struct(TypeInfo),
    Enum(TypeInfo),
    Type(TypeInfo),
}

/// Type information.
#[derive(Clone, Debug)]
pub struct TypeInfo {
    pub name: String,
    pub generics: Vec<String>,
    pub fields: Vec<Field>,
    pub dependencies: Vec<String>,
    pub docs: Vec<String>,
}

impl TypeInfo {
    /// Generates type info of structs from token input
    pub fn from_struct_tokens(
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
    pub fn from_enum_tokens(item_enum: syn::ItemEnum, _: MetadataAttr) -> syn::Result<Self> {
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
    pub fn from_type_tokens(
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
pub struct Field {
    pub name: String,
    pub ty: RustType,
    pub docs: Vec<String>,
    pub optional: bool,
}

/// The parsed Rust Type.
#[derive(Debug, PartialEq, Clone)]
pub enum RustType {
    BuiltIn(String),
    Generic(String, Vec<RustType>),
    Tuple(Vec<RustType>),
    Custom(String),
    CustomGeneric(String, Vec<RustType>),
    None,
}

impl RustType {
    const RUST_TYPES: &'static [&'static str] = &[
        "bool", "char", "str", "u8", "u16", "u32", "u64", "u128", "i8", "i16", "i32", "i64",
        "i128", "usize", "isize", "f16", "f32", "f64", "f128", "String",
    ];

    const BUILTIN_GENERICS: &'static [&'static str] = &["HashMap", "Vec", "Option", "Result"];

    fn is_builtin_type(ident: &syn::Ident) -> bool {
        Self::RUST_TYPES.contains(&ident.to_string().as_str())
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
                } else if Framework::is_skip_type(ident) && !Self::is_custom(ident, custom) {
                    None
                } else if (Framework::is_extractor_type(ident) || Self::is_builtin_generic(ident))
                    && !Self::is_custom(ident, custom)
                {
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
