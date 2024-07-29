pub use gluer_macros::{add_route, cached};

use std::collections::BTreeMap;
use std::fs::File;
use std::io::Write;
use std::path::Path;

/// Generates an api ts file at the `path` from the `routes` generated via `add_route!`, `fns` generated via `#[cached]` and `structs` generated via `#[cached]`.
/// ```rust,no_run
/// use gluer::{add_route, cached, gen_ts};
/// use axum::{routing::get, Router};
/// 
/// #[cached]
/// async fn test() {}
/// 
/// let mut app: Router<()> = Router::new();
/// let mut routes = vec![];
/// 
/// add_route!(routes, app, "/", get(test));
/// 
/// gen_ts(
///     "tests/api.ts",
///     routes,
///     &[FN_TEST],
///     &[],
/// )
/// .unwrap();
/// ```
pub fn gen_ts<P: AsRef<Path>>(
    path: P,
    routes: Vec<(&str, &str, &str)>,       // (url, method, fn_name)
    fns: &[(&str, &[(&str, &str)], &str)], // (fn_name, params, response)
    structs: &[(&str, &[(&str, &str)])],   // (struct_name, fields)
) -> Result<(), String> {
    let mut ts_functions = BTreeMap::new();
    let mut ts_interfaces = BTreeMap::new();

    for (url, method, fn_name) in routes {
        let function = fns
            .iter()
            .find(|&&(name, _, _)| fn_name == name)
            .ok_or_else(|| {
                format!(
                    "Function '{}' not found in the provided functions list",
                    fn_name
                )
            })?;

        println!("function: {:?}", function);

        let (fn_name, params, response) = function;
        let params_type = collect_params(params, &mut ts_interfaces, structs)?;
        let response_type = collect_response(response, &mut ts_interfaces, structs)?;

        let params_str = if !params_type.is_empty() {
            format!("params: {}", params_type)
        } else {
            String::new()
        };

        let body_assignment = if !params_type.is_empty() {
            "JSON.stringify(params)"
        } else {
            "undefined"
        };

        let function_str = format!(
            r#"export async function {fn_name}({params_str}): Promise<{response_type} | any> {{
    const response = await fetch("{url}", {{
        method: "{method}",
        headers: {{
            "Content-Type": "application/json"
        }},
        body: {body_assignment}
    }});
    return response.json();
}}

"#,
            fn_name = fn_name,
            params_str = params_str,
            response_type = response_type,
            url = url,
            method = method.to_uppercase(),
            body_assignment = body_assignment
        );

        ts_functions.insert(fn_name.to_string(), function_str);
    }

    write_to_file(path, ts_interfaces, ts_functions)?;

    Ok(())
}

fn collect_params(
    params: &[(&str, &str)],
    ts_interfaces: &mut BTreeMap<String, String>,
    structs: &[(&str, &[(&str, &str)])],
) -> Result<String, String> {
    for &(_, param_type) in params {
        if param_type.contains("Json") {
            let struct_name = extract_struct_name(param_type)?;
            if let Some(fields) = structs
                .iter()
                .find(|&&(s_name, _)| s_name == struct_name)
                .map(|(_, fields)| fields)
            {
                ts_interfaces
                    .entry(struct_name.to_string())
                    .or_insert_with(|| generate_ts_interface(&struct_name, fields));
                return Ok(struct_name);
            } else {
                return Err(format!(
                    "Struct '{}' not found in the provided structs list",
                    struct_name
                ));
            }
        }
    }
    Ok(String::new())
}

fn collect_response(
    response: &str,
    ts_interfaces: &mut BTreeMap<String, String>,
    structs: &[(&str, &[(&str, &str)])],
) -> Result<String, String> {
    let response = response.replace(' ', "");
    if let Some(response_type) = convert_rust_type_to_ts(&response) {
        return Ok(response_type);
    }

    if response.contains("Json") {
        let struct_name = extract_struct_name(&response)?;
        if let Some(fields) = structs
            .iter()
            .find(|&&(s_name, _)| s_name == struct_name)
            .map(|(_, fields)| fields)
        {
            ts_interfaces
                .entry(struct_name.to_string())
                .or_insert_with(|| generate_ts_interface(&struct_name, fields));
            return Ok(struct_name);
        }
    }

    Err(format!(
        "Struct '{}' not found in the provided structs list",
        response
    ))
}

fn extract_struct_name(type_str: &str) -> Result<String, String> {
    type_str
        .split('<')
        .nth(1)
        .and_then(|s| s.split('>').next())
        .map(|s| s.split("::").last().unwrap_or("").trim().to_string())
        .ok_or_else(|| format!("Failed to extract struct name from '{}'", type_str))
}

fn write_to_file<P: AsRef<Path>>(
    path: P,
    ts_interfaces: BTreeMap<String, String>,
    ts_functions: BTreeMap<String, String>,
) -> Result<(), String> {
    let mut file = File::create(path).map_err(|e| format!("Failed to create file: {}", e))?;

    for interface in ts_interfaces.values() {
        file.write_all(interface.as_bytes())
            .map_err(|e| format!("Failed to write to file: {}", e))?;
    }

    for function in ts_functions.values() {
        file.write_all(function.as_bytes())
            .map_err(|e| format!("Failed to write to file: {}", e))?;
    }

    Ok(())
}

fn convert_rust_type_to_ts(rust_type: &str) -> Option<String> {
    let rust_type = rust_type.trim();
    Some(match rust_type {
        "str" | "String" => "string".to_string(),
        "usize" | "isize" | "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64" | "f32"
        | "f64" => "number".to_string(),
        "bool" => "boolean".to_string(),
        "()" => "void".to_string(),
        t if t.starts_with("Vec<") => format!(
            "{}[]",
            convert_rust_type_to_ts(&t[4..t.len() - 1]).unwrap_or_default()
        ),
        t if t.starts_with("Option<") => return convert_rust_type_to_ts(&t[7..t.len() - 1]),
        t if t.starts_with("Result<") => return convert_rust_type_to_ts(&t[7..t.len() - 1]),
        t if t.starts_with("Json<") => return convert_rust_type_to_ts(&t[5..t.len() - 1]),
        t if t.starts_with('&') => return convert_rust_type_to_ts(&t[1..]),
        t if t.starts_with("'static") => return convert_rust_type_to_ts(&t[8..]),
        _ => return None,
    })
}

fn generate_ts_interface(struct_name: &str, fields: &[(&str, &str)]) -> String {
    let mut interface = format!("export interface {} {{\n", struct_name);
    for &(ident, ty) in fields {
        let ty = convert_rust_type_to_ts(ty).unwrap_or_default();
        interface.push_str(&format!("    {}: {};\n", ident, ty));
    }
    interface.push_str("}\n\n");
    interface
}
