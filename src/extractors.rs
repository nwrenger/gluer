use std::collections::HashMap;

use quote::ToTokens;

use crate::s_err;

pub(crate) fn extract_function(
    span: proc_macro2::Span,
    fn_name: &str,
    file_paths: Vec<std::path::PathBuf>,
) -> syn::Result<(Vec<syn::FnArg>, String)> {
    let mut params_map: HashMap<String, Vec<syn::FnArg>> = HashMap::new();
    let mut responses_map: HashMap<String, String> = HashMap::new();

    fn extract_from_syntax(
        syntax: syn::File,
        params_map: &mut HashMap<String, Vec<syn::FnArg>>,
        responses_map: &mut HashMap<String, String>,
    ) -> syn::Result<()> {
        for item in syntax.items {
            match item {
                syn::Item::Fn(syn::ItemFn { sig, .. }) => {
                    let fn_name = sig.ident.to_string();
                    let params: Vec<syn::FnArg> = sig.inputs.iter().cloned().collect();
                    params_map.insert(fn_name.clone(), params);

                    let ty: String = match sig.output {
                        syn::ReturnType::Default => "()".to_string(),
                        syn::ReturnType::Type(_, ty) => ty.into_token_stream().to_string(),
                    };

                    responses_map.insert(fn_name, ty);
                }
                syn::Item::Mod(syn::ItemMod {
                    content: Some((_, items)),
                    ..
                }) => {
                    extract_from_syntax(
                        syn::File {
                            shebang: None,
                            attrs: vec![],
                            items,
                        },
                        params_map,
                        responses_map,
                    )?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    for file_path in file_paths {
        let source = std::fs::read_to_string(&file_path)
            .map_err(|e| s_err(span, format!("'{}' {e}", file_path.display())))?;
        let syntax = syn::parse_file(&source)?;
        extract_from_syntax(syntax, &mut params_map, &mut responses_map)?;
    }

    let params = params_map
        .get(fn_name)
        .cloned()
        .ok_or_else(|| s_err(span, "Function parameters not found"))?;

    let responses = responses_map
        .get(fn_name)
        .cloned()
        .ok_or_else(|| s_err(span, "Function responses not found"))?;

    Ok((params, responses))
}

pub(crate) fn extract_struct(
    span: proc_macro2::Span,
    struct_name: &str,
    file_paths: Vec<std::path::PathBuf>,
) -> syn::Result<Vec<(String, String)>> {
    fn extract_from_syntax(
        syntax: syn::File,
        struct_name: &str,
    ) -> syn::Result<Option<Vec<(String, String)>>> {
        for item in syntax.items {
            match item {
                syn::Item::Struct(syn::ItemStruct { ident, fields, .. }) => {
                    let name = ident.to_string().trim().to_string();
                    let name = name.split("::").last().unwrap();

                    if name == struct_name {
                        let mut field_vec = Vec::new();

                        if let syn::Fields::Named(fields) = fields {
                            for field in fields.named {
                                let field_name = field.ident.unwrap().to_string();
                                let field_type = field.ty.into_token_stream().to_string();
                                field_vec.push((field_name, field_type));
                            }
                        }

                        return Ok(Some(field_vec));
                    }
                }
                syn::Item::Mod(syn::ItemMod {
                    content: Some((_, items)),
                    ..
                }) => {
                    if let Some(result) = extract_from_syntax(
                        syn::File {
                            shebang: None,
                            attrs: vec![],
                            items,
                        },
                        struct_name,
                    )? {
                        return Ok(Some(result));
                    }
                }
                _ => {}
            }
        }
        Ok(None)
    }

    for file_path in file_paths {
        let source = std::fs::read_to_string(&file_path)
            .map_err(|e| s_err(span, format!("'{}' {e}", file_path.display())))?;
        let syntax = syn::parse_file(&source)?;

        if let Some(result) = extract_from_syntax(syntax, struct_name)? {
            return Ok(result);
        }
    }

    Err(s_err(
        span,
        format!("Struct definition not found for {}", struct_name),
    ))
}

/// Resolves the path to the file containing the module. Note: Two or more nested modules are not supported.
pub(crate) fn resolve_path(
    span: proc_macro2::Span,
    segments: Vec<String>,
) -> syn::Result<Vec<std::path::PathBuf>> {
    let current_dir =
        std::env::current_dir().map_err(|_| s_err(span, "Failed to get current directory"))?;
    let src_dir = current_dir.join("src");
    let test_dir = current_dir.join("tests");
    let mut possible_paths = Vec::new();

    let working_dir = [src_dir, test_dir];

    dbg!(&segments);

    for dir in working_dir {
        if segments.len() > 2 {
            return Err(s_err(
                span,
                "Twice or more nested modules are currently not supported",
            ));
        } else {
            scan_dir(&dir, &mut possible_paths)?;
        }
    }

    if possible_paths.is_empty() {
        return Err(s_err(span, "No matching files found"));
    }

    Ok(possible_paths)
}

fn scan_dir(
    dir: &std::path::Path,
    possible_paths: &mut Vec<std::path::PathBuf>,
) -> syn::Result<()> {
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_file() && path.extension().map_or(false, |ext| ext == "rs") {
                possible_paths.push(path);
            } else if path.is_dir() {
                let mod_path = path.join("mod.rs");
                if mod_path.exists() {
                    possible_paths.push(mod_path);
                }
            }
        }
    }
    Ok(())
}
