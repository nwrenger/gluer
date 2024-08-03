#![doc = include_str!("../README.md")]

pub mod error;

pub use gluer_macros::{extract, metadata};

use axum::routing::MethodRouter;
use axum::Router;
use error::{Error, Result};
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

    /// Generates a TypeScript API client for the frontend from the API routes.
    ///
    /// ## Parameters
    /// - `path`: Specifies the directory and filename where the generated file will be saved.
    /// - `base`: Specifies the base URL for the API.
    ///
    /// ## Notes
    /// Ensure that the `base` URL does not end with a slash (`/`). For example:
    /// - Use `""` for no base URL if you are utilizing `axum`'s static file serving.
    /// - Use `"http://localhost:8080"` for a local server.
    pub fn generate_client<P: AsRef<std::path::Path>>(&self, path: P, base: &'a str) -> Result<()> {
        let base = format!("const BASE = '{}';\n", base);
        let basic_functions = r#"    async function fetch_api(endpoint: string, options: RequestInit): Promise<any> {
        const response = await fetch(endpoint, {
            headers: {
                "Content-Type": "application/json",
                ...options.headers,
            },
            ...options,
        });
        return response.json();
    }

    function query_str(params: Record<string, any>): string {
		if (params) {
			let data: Record<string, string> = {};
			for (let key in params) {
				if (params[key] != null) data[key] = params[key].toString();
			}
			// the URLSearchParams escapes any problematic values
			return '?' + new URLSearchParams(data).toString();
		}
		return '';
	}
"#;
        let namespace_start = "namespace api {\n";
        let namespace_end = "}\n\nexport default api;";

        let mut parsed_ts =
            ParsedTypeScript::new(&base, basic_functions, namespace_start, namespace_end);

        for route in &self.api_routes {
            Route::resolving_dependencies(
                route.fn_info.structs,
                &mut parsed_ts.interfaces,
                &mut parsed_ts.enum_types,
            )?;

            let params_type = route
                .fn_info
                .params
                .iter()
                .map(|Field { name: _, ty }| {
                    Type::<String>::ty_to_ts(ty, &[], &parsed_ts.interfaces, &parsed_ts.enum_types)
                })
                .collect::<Result<Vec<_>>>()?;
            let response_type = Type::<String>::ty_to_ts(
                route.fn_info.response,
                &[],
                &parsed_ts.interfaces,
                &parsed_ts.enum_types,
            )?;

            if parsed_ts.functions.contains_key(route.fn_name) {
                return Err(Error::Ts(format!(
                    "Function with name '{}' already exists",
                    route.fn_name,
                )));
            } else {
                parsed_ts.functions.insert(
                    route.fn_name.to_string(),
                    route.generate_ts_function(params_type, response_type),
                );
            }
        }

        parsed_ts.write_to_file(path)
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

impl<'a> Route<'a> {
    fn resolving_dependencies(
        dependencies: &[TypeInfo],
        interfaces: &mut BTreeMap<String, String>,
        enum_types: &mut BTreeMap<String, String>,
    ) -> Result<()> {
        for type_info in dependencies {
            Self::resolving_dependencies(type_info.dependencies, interfaces, enum_types)?;
            if !interfaces.contains_key(&type_info.name.to_string()) {
                if type_info.is_enum {
                    enum_types.insert(type_info.name.to_string(), type_info.generate_enum_type()?);
                } else {
                    interfaces.insert(
                        type_info.name.to_string(),
                        type_info.generate_interface(interfaces, enum_types)?,
                    );
                }
            }
        }
        Ok(())
    }

    fn generate_ts_function(
        &self,
        params_type: Vec<Type<String>>,
        response_type: Type<String>,
    ) -> String {
        let mut url = self.url.to_string();

        let params_str = params_type
            .iter()
            .filter_map(|ty| match ty {
                Json(ty) => Some(format!("data: {}", ty)),
                Path(ty) => Some(format!("path: {}", ty)),
                Query(ty) => Some(format!("query: {}", ty)),
                QueryMap(ty) => Some(format!("queryMap: Record{}", ty)),
                Unknown(_) => None,
            })
            .collect::<Vec<_>>()
            .join(", ");

        let body_assignment = if params_str.contains("data") {
            "\n            body: JSON.stringify(data)"
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
            url += "${query_str(query)}";
        }

        format!(
            r#"    export async function {fn_name}({params_str}): Promise<{response_type}> {{
        return fetch_api(`${{BASE}}{url}`, {{
            method: "{method}", {body_assignment}
        }});
    }}
"#,
            fn_name = self.fn_name,
            params_str = params_str,
            response_type = response_type.unwrap(),
            url = url,
            method = self.method.to_uppercase(),
            body_assignment = body_assignment
        )
    }
}

/// Function information.
#[derive(Clone, Copy, Debug)]
pub struct FnInfo<'a> {
    pub params: &'a [Field<'a>],
    pub response: &'a str,
    pub structs: &'a [TypeInfo<'a>],
}

/// Struct information.
#[derive(Clone, Debug)]
pub struct TypeInfo<'a> {
    pub name: &'a str,
    pub generics: &'a [&'a str],
    pub fields: &'a [Field<'a>],
    pub dependencies: &'a [TypeInfo<'a>],
    pub is_enum: bool,
}

impl<'a> TypeInfo<'a> {
    fn generate_interface(
        &self,
        interfaces: &BTreeMap<String, String>,
        enum_types: &BTreeMap<String, String>,
    ) -> Result<String> {
        let generics_str = if self.generics.is_empty() {
            "".to_string()
        } else {
            format!("<{}>", self.generics.join(", "))
        };
        let mut interface = format!("    export interface {}{} {{\n", self.name, generics_str);
        for Field { name, ty } in self.fields {
            let ty = Type::<String>::ty_to_ts(ty, self.generics, interfaces, enum_types)?;
            interface.push_str(&format!("        {}: {};\n", name, ty.unwrap()));
        }
        interface.push_str("    }\n");
        Ok(interface)
    }

    fn generate_enum_type(&self) -> Result<String> {
        let mut enum_type = format!("    export type {} = ", self.name);
        for (i, field) in self.fields.iter().enumerate() {
            enum_type.push_str(&format!(
                "\"{}\"{}",
                field.name,
                if i == self.fields.len() - 1 {
                    ";"
                } else {
                    " | "
                }
            ));
        }
        enum_type.push('\n');
        Ok(enum_type)
    }
}

/// Field information.
#[derive(Debug)]
pub struct Field<'a> {
    pub name: &'a str,
    pub ty: &'a str,
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

    fn ty_to_ts<'a>(
        ty: &'a str,
        generics: &[&str],
        interfaces: &'a BTreeMap<String, String>,
        enum_types: &'a BTreeMap<String, String>,
    ) -> Result<Type<String>> {
        let ty = ty.trim().replace(" ", "");
        if interfaces.contains_key(ty.as_str()) || enum_types.contains_key(ty.as_str()) {
            return Ok(Unknown(ty.to_string()));
        }

        Ok(match ty.as_str() {
            "str" | "String" => Unknown(String::from("string")),
            "usize" | "isize" | "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64"
            | "f32" | "f64" => Unknown(String::from("number")),
            "bool" => Unknown(String::from("boolean")),
            "()" => Unknown(String::from("void")),
            t if t.starts_with("Vec<") => {
                let inner_ty = &t[4..t.len() - 1];
                let ty = Self::ty_to_ts(inner_ty, generics, interfaces, enum_types)?.unwrap();
                Unknown(format!("{}[]", ty))
            }
            t if t.starts_with("Html<") => {
                let inner_ty = &t[5..t.len() - 1];
                let ty = Self::ty_to_ts(inner_ty, generics, interfaces, enum_types)?.unwrap();
                Unknown(ty)
            }
            t if t.starts_with("Json<") => {
                let inner_ty = &t[5..t.len() - 1];
                let ty = Self::ty_to_ts(inner_ty, generics, interfaces, enum_types)?.unwrap();
                Json(ty)
            }
            t if t.starts_with("Path<") => {
                let inner_ty = &t[5..t.len() - 1];
                let ty = Self::ty_to_ts(inner_ty, generics, interfaces, enum_types)?.unwrap();
                Path(ty)
            }
            t if t.starts_with("Query<HashMap") => {
                let inner_ty = &t[13..t.len() - 1];
                let ty = Self::ty_to_ts(inner_ty, generics, interfaces, enum_types)?.unwrap();
                QueryMap(ty)
            }
            t if t.starts_with("Query<") => {
                let inner_ty = &t[6..t.len() - 1];
                let ty = Self::ty_to_ts(inner_ty, generics, interfaces, enum_types)?.unwrap();
                Query(ty)
            }
            t if t.starts_with("Result<") => {
                let inner_ty = &t[7..t.len() - 1];
                let ty = Self::ty_to_ts(inner_ty, generics, interfaces, enum_types)?.unwrap();
                Unknown(format!("{} | any", ty))
            }
            t if t.starts_with("Option<") => {
                let inner_ty = &t[7..t.len() - 1];
                let ty = Self::ty_to_ts(inner_ty, generics, interfaces, enum_types)?.unwrap();
                Unknown(format!("{} | null", ty))
            }
            t if t.starts_with("&") => {
                Self::ty_to_ts(&t[1..t.len()], generics, interfaces, enum_types)?
            }
            t if t.starts_with("'static") => {
                Self::ty_to_ts(&t[7..t.len()], generics, interfaces, enum_types)?
            }
            t if t.contains('<') && t.contains('>') => {
                Self::parse_generic_type(t, generics, interfaces, enum_types)?
            }
            t => {
                if let Some(t) = generics.iter().find(|p| **p == t) {
                    Unknown(t.to_string())
                } else {
                    return Err(Error::Ts(format!(
                        "Type '{}' couldn't be converted to TypeScript",
                        ty
                    )));
                }
            }
        })
    }

    fn parse_generic_type<'a>(
        t: &'a str,
        generics: &[&str],
        interfaces: &'a BTreeMap<String, String>,
        enum_types: &'a BTreeMap<String, String>,
    ) -> Result<Type<String>> {
        let mut base_ty = String::new();
        let mut generic_params = String::new();
        let mut depth = 0;
        let mut inside_generic = false;

        for c in t.chars() {
            if c == '<' {
                depth += 1;
                if depth == 1 {
                    inside_generic = true;
                    continue;
                }
            } else if c == '>' {
                depth -= 1;
                if depth == 0 {
                    inside_generic = false;
                    continue;
                }
            }

            if inside_generic {
                generic_params.push(c);
            } else {
                base_ty.push(c);
            }
        }

        let mut params = Vec::new();
        let mut current_param = String::new();
        let mut param_depth = 0;

        for c in generic_params.chars() {
            if c == '<' {
                param_depth += 1;
            } else if c == '>' {
                param_depth -= 1;
            } else if c == ',' && param_depth == 0 {
                params.push(current_param.trim().to_string());
                current_param.clear();
                continue;
            }
            current_param.push(c);
        }
        if !current_param.is_empty() {
            params.push(current_param.trim().to_string());
        }

        let generic_ts = params
            .into_iter()
            .map(|param| Self::ty_to_ts(&param, generics, interfaces, enum_types))
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .map(|t| t.unwrap())
            .collect::<Vec<_>>()
            .join(", ");

        Ok(Unknown(format!("{}<{}>", base_ty, generic_ts)))
    }
}

struct ParsedTypeScript<'a> {
    base: &'a str,
    basic_functions: &'a str,
    namespace_start: &'a str,
    namespace_end: &'a str,
    interfaces: BTreeMap<String, String>,
    functions: BTreeMap<String, String>,
    enum_types: BTreeMap<String, String>,
}

impl<'a> ParsedTypeScript<'a> {
    fn new(
        base: &'a str,
        basic_functions: &'a str,
        namespace_start: &'a str,
        namespace_end: &'a str,
    ) -> Self {
        Self {
            base,
            basic_functions,
            namespace_start,
            namespace_end,
            interfaces: BTreeMap::new(),
            functions: BTreeMap::new(),
            enum_types: BTreeMap::new(),
        }
    }

    fn write_to_file<P: AsRef<std::path::Path>>(&self, path: P) -> Result<()> {
        let mut file = File::create(path)?;

        file.write_all(self.base.as_bytes())?;
        file.write_all(b"\n").unwrap();

        file.write_all(self.namespace_start.as_bytes())?;

        for interface in self.interfaces.values() {
            file.write_all(interface.as_bytes())?;
            file.write_all(b"\n").unwrap();
        }

        for enum_type in self.enum_types.values() {
            file.write_all(enum_type.as_bytes())?;
            file.write_all(b"\n").unwrap();
        }

        file.write_all(self.basic_functions.as_bytes())?;
        file.write_all(b"\n").unwrap();

        for (i, function) in self.functions.values().enumerate() {
            file.write_all(function.as_bytes())?;
            if self.functions.len() - 1 > i {
                file.write_all(b"\n").unwrap();
            }
        }

        file.write_all(self.namespace_end.as_bytes())?;
        file.write_all(b"\n").unwrap();

        Ok(())
    }
}
