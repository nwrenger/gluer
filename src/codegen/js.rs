use std::collections::{BTreeMap, HashMap};

use crate::{
    codegen::{generated_comment, get_prefix},
    parsing::{
        generate::Route,
        rust::{Field, FnInfo, RustType, TypeCategory, TypeInfo},
    },
};

const FETCH_API: &str = r#"    async function fetch_api(endpoint, options) {
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

const NAMESPACE_START: &str = "const api = (() => {\n    const exports = {};\n";
pub const NAMESPACE_END: &str = "    return { ...exports };\n})();\n\nexport default api;\n";
const QUERY_PARSER: &str = r#"    function query_str(params) {
        if (params) {
            let data = {};
            for (let key in params) {
                if (params[key] != null) data[key] = params[key].toString();
            }
            return '?' + new URLSearchParams(data).toString();
        }
        return '';
    }
"#;

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

fn generate_enum(ty: &TypeInfo) -> String {
    let mut out = String::new();
    out.push_str(&generate_docstring(&ty.docs, "    "));
    out.push_str(&format!("    exports.{} = Object.freeze({{\n", ty.name));
    for field in &ty.fields {
        out.push_str(&generate_docstring(&field.docs, "        "));
        out.push_str(&format!("        {}: \"{}\",\n", field.name, field.name));
    }
    out.push_str("    });\n");
    out
}

fn generate_route(route: Route, ty: &FnInfo, query_parser: &mut bool) -> Option<String> {
    use crate::codegen::ts::to_ts_type;

    let mut url = route.url.to_string();

    let params = ty
        .params
        .iter()
        .filter_map(|Field { ty, .. }| {
            // Validate type if it's convertible.
            to_ts_type(ty)?;
            match ty {
                RustType::Generic(name, ..) if name == "Json" => Some("data".to_string()),
                RustType::Generic(name, inner) if name == "Path" => {
                    if !inner.is_empty() {
                        if let RustType::Tuple(_) = &inner[0] {
                            return Some("pathTuple".to_string());
                        }
                    }
                    Some("path".to_string())
                }
                RustType::Generic(name, inner) if name == "Query" => {
                    if !inner.is_empty() {
                        if let RustType::Generic(inner_name, _) = &inner[0] {
                            if inner_name == "HashMap" {
                                return Some("queryMap".to_string());
                            }
                        }
                    }
                    *query_parser = true;
                    Some("query".to_string())
                }
                _ => None,
            }
        })
        .collect::<Vec<_>>()
        .join(", ");

    // Validate type if it's convertible.
    to_ts_type(&ty.response)?;

    let body_assignment = if params.contains("data") {
        "\n            body: JSON.stringify(data),"
    } else {
        ""
    };

    if params.contains("pathTuple") {
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
    } else if params.contains("path") {
        url = url.split(':').next().unwrap().to_string();
        url = url.split('{').next().unwrap().to_string();
        url += "${encodeURIComponent(path)}";
    }

    if params.contains("queryMap") {
        url += "?${new URLSearchParams(queryMap).toString()}";
    } else if params.contains("query") {
        url += "${query_str(query)}";
    }

    let docstring = generate_docstring(&ty.docs, "    ");

    Some(format!(
        r#"{docstring}    exports.{fn_name} = async function({params}) {{
        return fetch_api(`${{PREFIX}}{url}`, {{
            method: "{method}",{body_assignment}
        }});
    }}
"#,
        docstring = docstring,
        fn_name = ty.name,
        params = params,
        url = url,
        method = route.method.to_uppercase(),
        body_assignment = body_assignment,
    ))
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

    let mut generated_enums: BTreeMap<String, String> = BTreeMap::new();
    for ty in type_infos.values() {
        if let TypeCategory::Enum(e) = ty {
            generated_enums.insert(e.name.clone(), generate_enum(e));
        }
    }
    for enums in generated_enums.values() {
        out.push_str(enums);
        out.push('\n');
    }

    out.push_str(FETCH_API);
    out.push('\n');

    let mut query_parser = false;
    let mut generated_routes: BTreeMap<String, String> = BTreeMap::new();
    for route in routes {
        if let Some(fn_info) = fn_infos.get(&route.handler) {
            if let Some(route_ts) = generate_route(route, fn_info, &mut query_parser) {
                generated_routes.insert(fn_info.name.clone(), route_ts);
            }
        }
    }

    if query_parser {
        out.push_str(QUERY_PARSER);
        out.push('\n');
    }

    for route in generated_routes.values() {
        out.push_str(route.as_str());
        out.push('\n');
    }

    out.push_str(NAMESPACE_END);

    out
}
