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
        let fetch_api_function = r#"    async function fetchApi(endpoint: string, options: RequestInit): Promise<any> {
        const response = await fetch(endpoint, {
            headers: {
                "Content-Type": "application/json",
                ...options.headers,
            },
            ...options,
        });
        return response.json();
    }
"#;
        let namespace_start = "namespace api {\n";
        let namespace_end = "}\n\nexport default api;";

        let mut ts_functions = BTreeMap::new();
        let mut ts_interfaces: BTreeMap<String, String> = BTreeMap::new();

        for route in &self.api_routes {
            if !route.fn_info.structs.is_empty() {
                for struct_info in route.fn_info.structs {
                    Self::deps(struct_info.dependencies, &mut ts_interfaces);
                }
                Self::deps(route.fn_info.structs, &mut ts_interfaces);
            }

            let params_type = route
                .fn_info
                .params
                .iter()
                .map(|Field { name: _, ty }| ty_to_ts(ty, &[], &ts_interfaces).unwrap())
                .collect::<Vec<_>>();
            let response_type = ty_to_ts(route.fn_info.response, &[], &ts_interfaces).unwrap();

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

        write_to_file(
            path,
            fetch_api_function,
            namespace_start,
            namespace_end,
            ts_interfaces,
            ts_functions,
        )
        .map_err(|e| format!("Failed to write to file: {}", e))?;

        Ok(())
    }

    fn deps(dependencies: &[StructInfo], ts_interfaces: &mut BTreeMap<String, String>) {
        for StructInfo {
            name,
            generics,
            fields,
            dependencies,
        } in dependencies
        {
            if !ts_interfaces.contains_key(&name.to_string()) {
                let ts_interfaces_clone = ts_interfaces.clone();
                ts_interfaces.insert(
                    name.to_string(),
                    generate_ts_interface(name, generics, fields, &ts_interfaces_clone),
                );
            }
            Self::deps(dependencies, ts_interfaces);
        }
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

#[derive(Debug)]
/// Route information.
pub struct Route<'a> {
    pub url: &'a str,
    pub method: &'a str,
    pub fn_name: &'a str,
    pub fn_info: FnInfo<'a>,
}

/// Function information.
#[derive(Clone, Copy, Debug)]
pub struct FnInfo<'a> {
    pub params: &'a [Field<'a>],
    pub response: &'a str,
    pub structs: &'a [StructInfo<'a>],
}

/// Struct information.
#[derive(Clone, Debug)]
pub struct StructInfo<'a> {
    pub name: &'a str,
    pub generics: &'a [&'a str],
    pub fields: &'a [Field<'a>],
    pub dependencies: &'a [StructInfo<'a>],
}

/// Field information.
#[derive(Debug)]
pub struct Field<'a> {
    pub name: &'a str,
    pub ty: &'a str,
}

fn generate_ts_interface(
    struct_name: &str,
    generics: &[&str],
    fields: &[Field],
    ts_interfaces: &BTreeMap<String, String>,
) -> String {
    let generics_str = if generics.is_empty() {
        "".to_string()
    } else {
        format!("<{}>", generics.join(", "))
    };
    let mut interface = format!("    export interface {}{} {{\n", struct_name, generics_str);
    for Field { name, ty } in fields {
        let ty = ty_to_ts(ty, generics, ts_interfaces).unwrap();
        interface.push_str(&format!("        {}: {};\n", name, ty.unwrap()));
    }
    interface.push_str("    }\n");
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
        "\n        body: JSON.stringify(data)"
    } else {
        ""
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
        r#"    export async function {fn_name}({params_str}): Promise<{response_type}> {{
        return fetchApi(`{url}`, {{
            method: "{method}", {body_assignment}
        }});
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

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
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
    generics: &[&str],
    ts_interfaces: &'a BTreeMap<String, String>,
) -> Result<Type<String>, String> {
    let ty = ty.trim().replace(" ", "");
    if ts_interfaces.contains_key(ty.as_str()) {
        return Ok(Unknown(ty.to_string()));
    }

    Ok(match ty.as_str() {
        "str" | "String" => Unknown(String::from("string")),
        "usize" | "isize" | "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64" | "f32"
        | "f64" => Unknown(String::from("number")),
        "bool" => Unknown(String::from("boolean")),
        "()" => Unknown(String::from("void")),
        t if t.starts_with("Vec<") => {
            let inner_ty = &t[4..t.len() - 1];
            let ty = ty_to_ts(inner_ty, generics, ts_interfaces)?.unwrap();
            Unknown(format!("{}[]", ty))
        }
        t if t.starts_with("Html<") => {
            let inner_ty = &t[5..t.len() - 1];
            let ty = ty_to_ts(inner_ty, generics, ts_interfaces)?.unwrap();
            Unknown(ty)
        }
        t if t.starts_with("Json<") => {
            let inner_ty = &t[5..t.len() - 1];
            let ty = ty_to_ts(inner_ty, generics, ts_interfaces)?.unwrap();
            Json(ty)
        }
        t if t.starts_with("Path<") => {
            let inner_ty = &t[5..t.len() - 1];
            let ty = ty_to_ts(inner_ty, generics, ts_interfaces)?.unwrap();
            Path(ty)
        }
        t if t.starts_with("Query<HashMap<") => {
            let inner_ty = &t[14..t.len() - 2];
            let ty = ty_to_ts(inner_ty, generics, ts_interfaces)?.unwrap();
            QueryMap(ty)
        }
        t if t.starts_with("Query<") => {
            let inner_ty = &t[6..t.len() - 1];
            let ty = ty_to_ts(inner_ty, generics, ts_interfaces)?.unwrap();
            Query(ty)
        }
        t if t.starts_with("Result<") => {
            let inner_ty = &t[7..t.len() - 1];
            let ty = ty_to_ts(inner_ty, generics, ts_interfaces)?.unwrap();
            Unknown(format!("{} | any", ty))
        }
        t if t.starts_with("Option<") => {
            let inner_ty = &t[7..t.len() - 1];
            let ty = ty_to_ts(inner_ty, generics, ts_interfaces)?.unwrap();
            Unknown(format!("{} | null", ty))
        }
        t if t.starts_with("&") => ty_to_ts(&t[1..t.len()], generics, ts_interfaces)?,
        t if t.starts_with("'static") => ty_to_ts(&t[7..t.len()], generics, ts_interfaces)?,
        t if t.contains('<') && t.contains('>') => {
            let split: Vec<&str> = t.split('<').collect();
            let base_ty = split[0];
            let generic_params = &split[1][..split[1].len() - 1];

            let generic_ts = generic_params
                .split(',')
                .map(|param| ty_to_ts(param, generics, ts_interfaces))
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .map(|t| t.unwrap())
                .collect::<Vec<_>>()
                .join(", ");

            Unknown(format!("{}<{}>", base_ty, generic_ts))
        }
        t => {
            if let Some(t) = generics.iter().find(|p| **p == t) {
                Unknown(t.to_string())
            } else {
                return Err(format!("Type '{}' couldn't be converted to TypeScript", ty));
            }
        }
    })
}

fn write_to_file<P: AsRef<std::path::Path>>(
    path: P,
    fetch_api_function: &str,
    namespace_start: &str,
    namespace_end: &str,
    ts_interfaces: BTreeMap<String, String>,
    ts_functions: BTreeMap<String, String>,
) -> std::io::Result<()> {
    let mut file = File::create(path)?;

    file.write_all(namespace_start.as_bytes())?;

    for interface in ts_interfaces.values() {
        file.write_all(interface.as_bytes())?;
        file.write_all(b"\n").unwrap();
    }

    file.write_all(fetch_api_function.as_bytes())?;
    file.write_all(b"\n").unwrap();

    for (i, function) in ts_functions.values().enumerate() {
        file.write_all(function.as_bytes())?;
        if ts_functions.len() - 1 > i {
            file.write_all(b"\n").unwrap();
        }
    }

    file.write_all(namespace_end.as_bytes())?;
    file.write_all(b"\n").unwrap();

    Ok(())
}
