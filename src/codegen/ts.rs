use std::collections::{BTreeMap, HashMap};

use crate::{
    codegen::{generated_comment, get_prefix},
    framework::{Framework, RouteParamKind},
    parsing::{
        generate::Route,
        rust::{Field, FnInfo, RustType, TypeCategory, TypeInfo},
    },
};

const FETCH_API: &str = r#"    async function fetch_api(endpoint: string, options: RequestInit): Promise<any> {
        const response = await fetch(endpoint, {
            headers: {
                "Content-Type": "application/json",
                ...options.headers,
            },
            ...options,
        });
        const text = await response.text();
        if (!text) {
            return;
        } else {
            return JSON.parse(text);
        }
    }
"#;

const NAMESPACE_START: &str = "namespace api {\n";
const NAMESPACE_END: &str = "}\n\nexport default api;\n";
const QUERY_PARSER: &str = r#"    function query_str(params: Record<string, any>): string {
        if (params) {
            let data: Record<string, string> = {};
            for (let key in params) {
                if (params[key] != null) data[key] = params[key].toString();
            }
            return '?' + new URLSearchParams(data).toString();
        }
        return '';
    }
"#;

/// Joins generics into a string for TypeScript syntax together.
fn join_generic(tys: &[RustType]) -> String {
    tys.iter()
        .filter_map(to_ts_type)
        .collect::<Vec<_>>()
        .join(", ")
}

/// Converts a Rust type into a TypeScript type. Returns `None` if the type cannot be converted.
pub fn to_ts_type(ty: &RustType) -> Option<String> {
    match &ty {
        RustType::BuiltIn(ty) => match ty.as_str() {
            "char" | "str" | "String" | "u128" | "i128" | "f128" => Some(String::from("string")),
            "usize" | "isize" | "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64"
            | "f16" | "f32" | "f64" => Some(String::from("number")),
            "bool" => Some(String::from("boolean")),
            "()" => Some(String::from("void")),
            _ => None,
        },
        RustType::Generic(ty, inner_tys) => match ty.as_str() {
            "Vec" => {
                let ty = join_generic(inner_tys);
                Some(format!("{}[]", ty))
            }
            "Html" => {
                let ty = join_generic(inner_tys);
                Some(ty)
            }
            "Json" => {
                let ty = join_generic(inner_tys);
                Some(ty)
            }
            "Path" => {
                if !inner_tys.is_empty() {
                    if let RustType::Tuple(_) = &inner_tys[0] {
                        return to_ts_type(&inner_tys[0]);
                    }
                }
                let ty = join_generic(inner_tys);
                Some(ty)
            }
            "Query" => {
                if !inner_tys.is_empty() {
                    if let RustType::Generic(ty, _) = &inner_tys[0] {
                        if ty == "HashMap" {
                            return to_ts_type(&inner_tys[0]);
                        }
                    }
                }
                let ty = join_generic(inner_tys);
                Some(ty)
            }
            "HashMap" => {
                if inner_tys.len() != 2 {
                    return None;
                }

                let key_ty = to_ts_type(&inner_tys[0])?;
                let value_ty = to_ts_type(&inner_tys[1])?;
                Some(format!("Record<{}, {}>", key_ty, value_ty))
            }
            "Result" => {
                if inner_tys.len() != 2 {
                    return None;
                }

                let ok_ty = to_ts_type(&inner_tys[0])?;
                let err_ty = to_ts_type(&inner_tys[1])?;
                Some(format!("{} | {}", ok_ty, err_ty))
            }
            "Option" => {
                let ty = to_ts_type(&inner_tys[0])?;
                Some(format!("{} | null", ty))
            }
            _ => None,
        },
        RustType::Tuple(tys) => {
            let tys = join_generic(tys);
            Some(format!("[{}]", tys))
        }
        RustType::Custom(ty) => Some(ty.to_string()),
        RustType::CustomGeneric(ty, inner_tys) => {
            let tys = join_generic(inner_tys);
            Some(format!("{}<{}>", ty, tys))
        }
        _ => None,
    }
}

fn generate_docstring(docs: &[String], ident: &str) -> String {
    let mut docstring = String::new();
    if !docs.is_empty() {
        docstring.push_str(&format!("{}/**\n", ident));
        for doc in docs {
            docstring.push_str(&format!("{}    {}\n", ident, doc));
        }
        docstring.push_str(&format!("{}*/\n", ident));
    }
    docstring
}

pub fn generate_route(route: Route, ty: &FnInfo, query_parser: &mut bool) -> Option<String> {
    let mut url = route.url.to_string();

    let params = ty
        .params
        .iter()
        .filter_map(|Field { ty, .. }| {
            let ts_ty = to_ts_type(ty)?;
            match Framework::classify_route_param(ty) {
                Some(RouteParamKind::Body) => Some(format!("data: {}", ts_ty)),
                Some(RouteParamKind::Path) => Some(format!("path: {}", ts_ty)),
                Some(RouteParamKind::PathTuple) => Some(format!("pathTuple: {}", ts_ty)),
                Some(RouteParamKind::Query) => {
                    *query_parser = true;
                    Some(format!("query: {}", ts_ty))
                }
                Some(RouteParamKind::QueryMap) => Some(format!("queryMap: {}", ts_ty)),
                None => None,
            }
        })
        .collect::<Vec<_>>()
        .join(", ");

    let response_type = to_ts_type(&ty.response)?;

    let body_assignment = if params.contains("data: ") {
        "\n            body: JSON.stringify(data),"
    } else {
        ""
    };

    // Rewrite URL segments for path params
    if params.contains("pathTuple: ") {
        let mut i = 0;
        url = url
            .split('/')
            .map(|part| {
                if part.starts_with(':') || part.starts_with('{') {
                    let idx = i;
                    i += 1;
                    format!("${{encodeURIComponent(pathTuple[{}])}}", idx)
                } else {
                    part.to_string()
                }
            })
            .collect::<Vec<_>>()
            .join("/");
    } else if params.contains("path: ") {
        url = url.split(':').next().unwrap().to_string();
        url = url.split('{').next().unwrap().to_string();
        url += "${encodeURIComponent(path)}";
    }

    // Append query string
    if params.contains("queryMap: ") {
        url += "?${new URLSearchParams(queryMap).toString()}";
    } else if params.contains("query: ") {
        url += "${query_str(query)}";
    }

    let docstring = generate_docstring(&ty.docs, "    ");

    Some(format!(
        r#"{docstring}    export async function {fn_name}({params}): Promise<{response_type}> {{
        return fetch_api(`${{PREFIX}}{url}`, {{
            method: "{method}",{body_assignment}
        }});
    }}
"#,
        docstring = docstring,
        fn_name = ty.name,
        params = params,
        response_type = response_type,
        url = url,
        method = route.method.to_uppercase(),
        body_assignment = body_assignment,
    ))
}

pub fn generate_interface(ty: &TypeInfo) -> String {
    let generics_str = if ty.generics.is_empty() {
        "".to_string()
    } else {
        format!("<{}>", ty.generics.join(", "))
    };

    let mut interface = String::new();
    interface.push_str(&generate_docstring(&ty.docs, "    "));
    interface.push_str(&format!(
        "    export interface {}{} {{\n",
        ty.name, generics_str
    ));

    for field in &ty.fields {
        let ty = to_ts_type(&field.ty);
        if ty.is_none() {
            continue;
        }

        interface.push_str(&generate_docstring(&field.docs, "        "));
        interface.push_str(&format!(
            "        {}{}: {};\n",
            field.name,
            if field.optional { "?" } else { "" },
            ty.unwrap()
        ));
    }

    interface.push_str("    }\n");
    interface
}

pub fn generate_enum(ty: &TypeInfo) -> String {
    let mut enum_type = String::new();
    enum_type.push_str(&generate_docstring(&ty.docs, "    "));
    enum_type.push_str(&format!("    export enum {} {{\n", ty.name));

    for field in &ty.fields {
        enum_type.push_str(&generate_docstring(&field.docs, "        "));
        enum_type.push_str(&format!("        {} = \"{}\",\n", field.name, field.name));
    }

    enum_type.push_str("    }\n");
    enum_type
}

pub fn generate_type(ty: &TypeInfo) -> String {
    let mut type_type = String::new();

    type_type.push_str(&generate_docstring(&ty.docs, "    "));
    type_type.push_str(&format!(
        "    export type {}{} = ",
        ty.name,
        if ty.generics.is_empty() {
            String::new()
        } else {
            format!("<{}>", &ty.generics.join(", "))
        }
    ));

    let mut fields = vec![];
    for Field { ty, .. } in &ty.fields {
        let ty = to_ts_type(ty);
        if let Some(ty) = ty {
            fields.push(ty);
        }
    }

    for (i, field) in fields.iter().enumerate() {
        type_type.push_str(&format!(
            "{}{}",
            field,
            if i == fields.len() - 1 { ";" } else { " | " }
        ));
    }

    type_type.push('\n');
    type_type
}

pub fn generate(
    prefix: String,
    routes: Vec<Route>,
    fn_infos: HashMap<String, FnInfo>,
    type_infos: HashMap<String, TypeCategory>,
) -> String {
    let mut out = String::new();
    out.push_str(generated_comment().as_str());
    out.push('\n');

    out.push_str(get_prefix(prefix).as_str());
    out.push('\n');

    out.push_str(NAMESPACE_START);

    let mut generated_interfaces = BTreeMap::new();
    let mut generated_enums = BTreeMap::new();
    let mut generated_types = BTreeMap::new();
    for ty in type_infos.values() {
        match ty {
            TypeCategory::Struct(s) => {
                generated_interfaces.insert(s.name.clone(), generate_interface(s))
            }
            TypeCategory::Enum(e) => generated_enums.insert(e.name.clone(), generate_enum(e)),
            TypeCategory::Type(t) => generated_types.insert(t.name.clone(), generate_type(t)),
        };
    }
    for interface in generated_interfaces.values() {
        out.push_str(interface);
        out.push('\n');
    }
    for enums in generated_enums.values() {
        out.push_str(enums);
        out.push('\n');
    }
    for r#type in generated_types.values() {
        out.push_str(r#type);
        out.push('\n');
    }

    out.push_str(FETCH_API);
    out.push('\n');

    let mut query_parser = false;
    let mut generated_routes = BTreeMap::new();
    for route in routes {
        if let Some(fn_info) = fn_infos.get(&route.handler) {
            // Generate route function.
            let route_ts = generate_route(route, fn_info, &mut query_parser);
            if let Some(route_ts) = route_ts {
                generated_routes.insert(fn_info.name.clone(), route_ts);
            }
        }
    }

    if query_parser {
        out.push_str(QUERY_PARSER);
        out.push('\n');
    }

    for (i, route) in generated_routes.values().enumerate() {
        out.push_str(route.as_str());
        if generated_routes.len() - 1 > i {
            out.push('\n');
        }
    }

    out.push_str(NAMESPACE_END);
    out
}
