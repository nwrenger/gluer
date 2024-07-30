#![doc = include_str!("../README.md")]

pub use gluer_macros::{extract, metadata};

use axum::routing::MethodRouter;
use axum::Router;
use std::{collections::BTreeMap, fs::File, io::Write};

use crate::Type::{Json, Path, Query, QueryMap, Unknown};

/// Wrapper around `axum::Router` that allows for generating TypeScript API clients.
pub struct Api<'a, S = ()> {
    router: Router<S>,
    api_routes: Vec<Route<'a>>,
}

impl<'a, S> Api<'a, S>
where
    S: Clone + Send + Sync + 'static,
{
    /// Create a new `Api`.
    pub fn new() -> Self {
        Self {
            router: Router::new(),
            api_routes: vec![],
        }
    }

    /// Add a route to the API and `axum::Router`.
    pub fn route(
        mut self,
        path: &'a str,
        extracted_metadata: (MethodRouter<S>, &'a [Route<'a>]),
    ) -> Self {
        let router = self.router.route(path, extracted_metadata.0);
        self.api_routes.extend({
            let mut routes = vec![];
            for route in extracted_metadata.1 {
                routes.push(Route {
                    url: path,
                    method: route.method,
                    fn_name: route.fn_name,
                    fn_info: route.fn_info,
                });
            }
            routes
        });

        Self {
            router,
            api_routes: self.api_routes,
        }
    }

    /// Access the inner `axum::Router` via a Function `f`.
    pub fn inner_router<F, S2>(self, f: F) -> Api<'a, S2>
    where
        F: Fn(Router<S>) -> Router<S2>,
    {
        Api {
            router: f(self.router),
            api_routes: self.api_routes,
        }
    }

    /// Generate frontend TypeScript API client from the API routes.
    pub fn generate_client<P: AsRef<std::path::Path>>(&self, path: P) -> Result<(), String> {
        let mut ts_functions = BTreeMap::new();
        let mut ts_interfaces: BTreeMap<String, String> = BTreeMap::new();

        for route in &self.api_routes {
            if !route.fn_info.structs.is_empty() {
                for struct_info in route.fn_info.structs {
                    ts_interfaces
                        .entry(struct_info.name.to_string())
                        .or_insert_with(|| {
                            generate_ts_interface(struct_info.name, struct_info.fields)
                        });
                }
            }

            let params_type = route
                .fn_info
                .params
                .iter()
                .map(|Field { name: _, ty }| ty_to_ts(ty, &ts_interfaces).unwrap())
                .collect::<Vec<_>>();
            let response_type = ty_to_ts(route.fn_info.response, &ts_interfaces).unwrap();

            if ts_functions.contains_key(route.fn_name) {
                return Err(format!(
                    "Function with name '{}' already exists",
                    route.fn_name,
                ));
            } else {
                ts_functions.insert(
                    route.fn_name.to_string(),
                    generate_ts_function(
                        route.url,
                        route.method,
                        route.fn_name,
                        params_type,
                        response_type,
                    ),
                );
            }
        }

        write_to_file(path, ts_interfaces, ts_functions)?;

        Ok(())
    }

    /// Convert into an `axum::Router`.
    pub fn into_router(self) -> Router<S> {
        self.router
    }
}

impl<'a, S> Default for Api<'a, S>
where
    S: Clone + Send + Sync + 'static,
{
    fn default() -> Self {
        Self::new()
    }
}

/// Route information.
pub struct Route<'a> {
    pub url: &'a str,
    pub method: &'a str,
    pub fn_name: &'a str,
    pub fn_info: FnInfo<'a>,
}

/// Function information.
#[derive(Clone, Copy)]
pub struct FnInfo<'a> {
    pub params: &'a [Field<'a>],
    pub response: &'a str,
    pub structs: &'a [StructInfo<'a>],
}

/// Struct information.
pub struct StructInfo<'a> {
    pub name: &'a str,
    pub fields: &'a [Field<'a>],
}

/// Field information.
pub struct Field<'a> {
    pub name: &'a str,
    pub ty: &'a str,
}

fn generate_ts_interface(struct_name: &str, fields: &[Field]) -> String {
    let mut interface = format!("export interface {} {{\n", struct_name);
    for Field { name, ty } in fields {
        let bind = BTreeMap::new();
        let ty = ty_to_ts(ty, &bind).unwrap();
        interface.push_str(&format!("    {}: {};\n", name, ty.unwrap()));
    }
    interface.push_str("}\n\n");
    interface
}

fn generate_ts_function(
    url: &str,
    method: &str,
    fn_name: &str,
    params_type: Vec<Type<String>>,
    response_type: Type<String>,
) -> String {
    let mut url = url.to_string();

    let params_str = params_type
        .iter()
        .filter_map(|ty| match ty {
            Json(ty) => Some(format!("data: {}", ty)),
            Path(ty) => Some(format!("path: {}", ty)),
            Query(ty) => Some(format!("query: {}", ty)),
            QueryMap(ty) => Some(format!("queryMap: Record<string, {}>", ty)),
            Unknown(_) => None,
        })
        .collect::<Vec<_>>()
        .join(", ");

    let body_assignment = if params_str.contains("data") {
        "JSON.stringify(data)"
    } else {
        "undefined"
    };

    if params_str.contains("path") {
        url = url.split(":").next().unwrap().to_string();
        url += "${encodeURIComponent(path)}";
    }

    if params_str.contains("queryMap") {
        url += "?";
        url += "${new URLSearchParams(queryMap).toString()}";
    } else if params_str.contains("query") {
        url += "?";
        url += "${encodeURIComponent(query)}";
    }

    format!(
        r#"export async function {fn_name}({params_str}): Promise<{response_type}> {{
    const response = await fetch(`{url}`, {{
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
        response_type = response_type.unwrap(),
        url = url,
        method = method.to_uppercase(),
        body_assignment = body_assignment
    )
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone)]
enum Type<T> {
    Unknown(T),
    Json(T),
    Path(T),
    Query(T),
    QueryMap(T),
}

impl<T> Type<T> {
    fn unwrap(self) -> T {
        match self {
            Type::Unknown(t) => t,
            Type::Json(t) => t,
            Type::Path(t) => t,
            Type::Query(t) => t,
            Type::QueryMap(t) => t,
        }
    }
}

fn ty_to_ts<'a>(
    ty: &'a str,
    ts_interfaces: &'a BTreeMap<String, String>,
) -> Result<Type<String>, String> {
    if ts_interfaces.contains_key(ty) {
        return Ok(Unknown(ty.to_string()));
    }
    Ok(match ty {
        "str" | "String" => Unknown(String::from("string")),
        "usize" | "isize" | "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64" | "f32"
        | "f64" => Unknown(String::from("number")),
        "bool" => Unknown(String::from("boolean")),
        "()" => Unknown(String::from("void")),
        t if t.starts_with("Vec<") => {
            let ty = ty_to_ts(&t[4..t.len() - 1], ts_interfaces)?.unwrap();
            Unknown(format!("{}[]", ty))
        }
        t if t.starts_with("Html<") => {
            let ty = ty_to_ts(&t[5..t.len() - 1], ts_interfaces)?.unwrap();
            Unknown(format!("{}[]", ty))
        }
        t if t.starts_with("Json<") => {
            let ts_type = ty_to_ts(&t[5..t.len() - 1], ts_interfaces)?;
            return Ok(Json(ts_type.unwrap()));
        }
        t if t.starts_with("Path<") => {
            let ts_type = ty_to_ts(&t[5..t.len() - 1], ts_interfaces)?;
            return Ok(Path(ts_type.unwrap()));
        }
        t if t.starts_with("Query<HashMap<") => {
            let ts_type = ty_to_ts(&t[14..t.len() - 2], ts_interfaces)?;
            return Ok(QueryMap(ts_type.unwrap()));
        }
        t if t.starts_with("Query<") => {
            let ts_type = ty_to_ts(&t[6..t.len() - 1], ts_interfaces)?;
            return Ok(Query(ts_type.unwrap()));
        }
        _ => return Err(format!("Type '{}' couldn't be converted to TypeScript", ty)),
    })
}

fn write_to_file<P: AsRef<std::path::Path>>(
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
