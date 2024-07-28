use std::{collections::HashMap, env::current_dir};

use quote::ToTokens;

pub(crate) fn extract_function(
    fn_name: &str,
    file_path: std::path::PathBuf,
) -> syn::Result<(Vec<syn::FnArg>, String)> {
    let source = std::fs::read_to_string(file_path)
        .map_err(|e| syn::Error::new(proc_macro2::Span::mixed_site(), e.to_string()))?;
    let syntax = syn::parse_file(&source)?;

    let mut params_map: HashMap<String, Vec<syn::FnArg>> = HashMap::new();
    let mut responses_map: HashMap<String, String> = HashMap::new();

    for item in syntax.items {
        if let syn::Item::Fn(syn::ItemFn { sig, .. }) = item {
            let fn_name = sig.ident.to_string();
            let params: Vec<syn::FnArg> = sig.inputs.iter().cloned().collect();
            params_map.insert(fn_name.clone(), params);

            let ty: String = match sig.output {
                syn::ReturnType::Default => "()".to_string(),
                syn::ReturnType::Type(_, ty) => ty.into_token_stream().to_string(),
            };

            responses_map.insert(fn_name, ty);
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

pub(crate) fn extract_struct(
    struct_name: &str,
    file_path: std::path::PathBuf,
) -> syn::Result<Vec<(String, String)>> {
    let source = std::fs::read_to_string(&file_path)
        .map_err(|e| syn::Error::new(proc_macro2::Span::mixed_site(), e.to_string()))?;
    let syntax = syn::parse_file(&source)?;

    for item in syntax.items {
        if let syn::Item::Struct(syn::ItemStruct { ident, fields, .. }) = item {
            let name = ident.to_string().trim().to_string();

            if name == struct_name {
                let mut field_vec = Vec::new();

                if let syn::Fields::Named(fields) = fields {
                    for field in fields.named {
                        let field_name = field.ident.unwrap().to_string();
                        let field_type = field.ty.into_token_stream().to_string();
                        field_vec.push((field_name, field_type));
                    }
                }

                return Ok(field_vec);
            }
        }
    }

    Err(syn::Error::new(
        proc_macro2::Span::call_site(),
        "Struct definition not found in ".to_string() + file_path.to_string_lossy().as_ref(),
    ))
}

pub(crate) fn resolve_path(segments: Vec<String>) -> syn::Result<std::path::PathBuf> {
    let current_dir = current_dir().map_err(|_| {
        syn::Error::new(
            proc_macro2::Span::call_site(),
            "Failed to get current directory",
        )
    })?;

    if segments.len() == 1 {
        // Function is in the same file, check if it's in main.rs or lib.rs (for tests)
        let main_path = current_dir.join("src/main.rs");
        let lib_path = current_dir.join("src/lib.rs");
        if main_path.exists() {
            Ok(main_path)
        } else if lib_path.exists() {
            Ok(current_dir.join("tests/main.rs"))
        } else {
            Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                "Neither main.rs nor lib.rs found",
            ))?
        }
    } else {
        // Function is in a different module
        let module_path = &segments[0];
        let file_path_mod = current_dir.join(format!("src/{}/mod.rs", module_path));
        let file_path_alt = current_dir.join(format!("src/{}.rs", module_path));
        if file_path_mod.exists() {
            Ok(file_path_mod)
        } else if file_path_alt.exists() {
            Ok(file_path_alt)
        } else {
            Err(syn::Error::new(
                proc_macro2::Span::call_site(),
                format!("Module file not found for {}", module_path),
            ))?
        }
    }
}
