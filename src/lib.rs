#![doc = include_str!("../README.md")]

pub mod error;

pub use gluer_macros::{extract, metadata};

use axum::routing::MethodRouter;
use axum::Router;
use error::{Error, Result};
use std::{collections::BTreeMap, fs::File, io::Write, vec};

/// Wrapper around `axum::Router` that allows for generating TypeScript API clients.
pub struct Api<S = ()> {
    router: Router<S>,
    api_routes: Vec<Route>,
}

impl<S> Api<S>
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
    pub fn route(mut self, path: &str, extracted_metadata: (MethodRouter<S>, Vec<Route>)) -> Self {
        let router = self.router.route(path, extracted_metadata.0);
        self.api_routes.extend({
            let mut routes = vec![];
            for route in extracted_metadata.1 {
                routes.push(Route {
                    url: path.to_string(),
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
    pub fn inner_router<F, S2>(self, f: F) -> Api<S2>
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
    pub fn generate_client<P: AsRef<std::path::Path>>(&self, path: P, base: &str) -> Result<()> {
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
                &route.fn_info.types,
                &mut parsed_ts.interfaces,
                &mut parsed_ts.enum_types,
                &mut parsed_ts.type_types,
            )?;

            let params_type = route
                .fn_info
                .params
                .iter()
                .map(|Field { name: _, ty }| {
                    ty.to_api_type(
                        &[],
                        &parsed_ts.interfaces,
                        &parsed_ts.enum_types,
                        &parsed_ts.type_types,
                    )
                })
                .collect::<Result<Vec<_>>>()?;
            let response_type = route.fn_info.response.to_api_type(
                &[],
                &parsed_ts.interfaces,
                &parsed_ts.enum_types,
                &parsed_ts.type_types,
            )?;

            if parsed_ts.functions.contains_key(&route.fn_name) {
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

impl<S> Default for Api<S>
where
    S: Clone + Send + Sync + 'static,
{
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
/// Route information.
pub struct Route {
    pub url: String,
    pub method: String,
    pub fn_name: String,
    pub fn_info: FnInfo,
}

impl Route {
    fn resolving_dependencies(
        dependencies: &[TypeCategory],
        interfaces: &mut BTreeMap<String, String>,
        enum_types: &mut BTreeMap<String, String>,
        type_types: &mut BTreeMap<String, String>,
    ) -> Result<()> {
        for type_info in dependencies {
            match type_info {
                TypeCategory::Struct(type_info) => {
                    Self::resolving_dependencies(
                        &type_info.dependencies,
                        interfaces,
                        enum_types,
                        type_types,
                    )?;
                    if !interfaces.contains_key(&type_info.name.to_string()) {
                        interfaces.insert(
                            type_info.name.to_string(),
                            type_info.generate_interface(interfaces, enum_types, type_types)?,
                        );
                    }
                }
                TypeCategory::Enum(type_info) => {
                    if let std::collections::btree_map::Entry::Vacant(e) =
                        enum_types.entry(type_info.name.to_string())
                    {
                        e.insert(type_info.generate_enum_type()?);
                    }
                }
                TypeCategory::Type(type_info) => {
                    Self::resolving_dependencies(
                        &type_info.dependencies,
                        interfaces,
                        enum_types,
                        type_types,
                    )?;
                    if !type_types.contains_key(&type_info.name.to_string()) {
                        type_types.insert(
                            type_info.name.to_string(),
                            type_info.generate_type_type(interfaces, enum_types, type_types)?,
                        );
                    }
                }
            }
        }
        Ok(())
    }

    fn generate_ts_function(&self, params_type: Vec<ApiType>, response_type: ApiType) -> String {
        let mut url = self.url.to_string();

        let params_str = params_type
            .iter()
            .filter_map(|ty| match ty {
                ApiType::Json(ty) => Some(format!("data: {}", ty)),
                ApiType::Path(ty) => Some(format!("path: {}", ty)),
                ApiType::PathTuple(ty) => Some(format!("pathTuple: {}", ty)),
                ApiType::Query(ty) => Some(format!("query: {}", ty)),
                ApiType::QueryMap(ty) => Some(format!("queryMap: {}", ty)),
                ApiType::Unknown(_) => None,
            })
            .collect::<Vec<_>>()
            .join(", ");

        let body_assignment = if params_str.contains("data") {
            "\n            body: JSON.stringify(data)"
        } else {
            ""
        };

        if params_str.contains("pathTuple") {
            let mut i = 0;
            url = url
                .split("/")
                .map(|part| {
                    if part.starts_with(":") {
                        i += 1;
                        format!("${{encodeURIComponent(pathTuple[{}])}}", i - 1)
                    } else {
                        part.to_string()
                    }
                })
                .collect::<Vec<_>>()
                .join("/");
        } else if params_str.contains("path") {
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
#[derive(Clone, Debug)]
pub struct FnInfo {
    pub params: Vec<Field>,
    pub response: RustType,
    pub types: Vec<TypeCategory>,
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
    pub dependencies: Vec<TypeCategory>,
}

impl TypeInfo {
    fn generate_interface(
        &self,
        interfaces: &BTreeMap<String, String>,
        enum_types: &BTreeMap<String, String>,
        type_types: &BTreeMap<String, String>,
    ) -> Result<String> {
        let generics_str = if self.generics.is_empty() {
            "".to_string()
        } else {
            format!("<{}>", self.generics.join(", "))
        };
        let mut interface = format!("    export interface {}{} {{\n", self.name, generics_str);
        for Field { name, ty } in &self.fields {
            let ty = ty.to_api_type(&self.generics, interfaces, enum_types, type_types)?;
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

    fn generate_type_type(
        &self,
        interfaces: &BTreeMap<String, String>,
        enum_types: &BTreeMap<String, String>,
        type_types: &BTreeMap<String, String>,
    ) -> Result<String> {
        let mut type_type = format!(
            "    export type {}{} = ",
            self.name,
            if self.generics.is_empty() {
                String::new()
            } else {
                format!("<{}>", &self.generics.join(", "))
            }
        );
        let mut fields = vec![];
        for Field { ty, .. } in &self.fields {
            let ty = ty.to_api_type(&self.generics, interfaces, enum_types, type_types)?;
            fields.push(ty.unwrap());
        }
        for (i, field) in fields.iter().enumerate() {
            type_type.push_str(&format!(
                "{}{}",
                field,
                if i == fields.len() - 1 { ";" } else { " | " }
            ));
        }
        type_type.push('\n');
        Ok(type_type)
    }
}

/// Field information.
#[derive(Clone, Debug)]
pub struct Field {
    pub name: String,
    pub ty: RustType,
}

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
    fn unwrap(&self) -> String {
        match self {
            Self::BuiltIn(ty) => ty.to_string(),
            Self::Generic(ty, _) => ty.to_string(),
            Self::Custom(ty) => ty.to_string(),
            _ => String::new(),
        }
    }
    fn to_api_type<'a>(
        &self,
        generics: &[String],
        interfaces: &'a BTreeMap<String, String>,
        enum_types: &'a BTreeMap<String, String>,
        type_types: &'a BTreeMap<String, String>,
    ) -> Result<ApiType> {
        if let Some(t) = generics.iter().find(|p| **p == self.unwrap()) {
            return Ok(ApiType::Unknown(t.to_string()));
        }

        match &self {
            Self::BuiltIn(ty) => match ty.as_str() {
                "str" | "String" => return Ok(ApiType::Unknown(String::from("string"))),
                "usize" | "isize" | "u8" | "u16" | "u32" | "u64" | "i8" | "i16" | "i32" | "i64"
                | "f32" | "f64" => return Ok(ApiType::Unknown(String::from("number"))),
                "bool" => return Ok(ApiType::Unknown(String::from("boolean"))),
                "()" => return Ok(ApiType::Unknown(String::from("void"))),
                _ => {}
            },
            Self::Generic(ty, inner_tys) => match ty.as_str() {
                "Vec" => {
                    let ty = Self::join_generic(
                        inner_tys, generics, interfaces, enum_types, type_types,
                    )?;
                    return Ok(ApiType::Unknown(format!("{}[]", ty)));
                }
                "Html" => {
                    let ty = Self::join_generic(
                        inner_tys, generics, interfaces, enum_types, type_types,
                    )?;
                    return Ok(ApiType::Unknown(ty));
                }
                "Json" => {
                    let ty = Self::join_generic(
                        inner_tys, generics, interfaces, enum_types, type_types,
                    )?;
                    return Ok(ApiType::Json(ty));
                }
                "Path" => {
                    if !inner_tys.is_empty() {
                        if let RustType::Tuple(_) = &inner_tys[0] {
                            let ty = inner_tys[0]
                                .to_api_type(generics, interfaces, enum_types, type_types)?;
                            return Ok(ApiType::PathTuple(ty.unwrap()));
                        }
                    }
                    let ty = Self::join_generic(
                        inner_tys, generics, interfaces, enum_types, type_types,
                    )?;
                    return Ok(ApiType::Path(ty));
                }
                "Query" => {
                    if !inner_tys.is_empty() {
                        if let RustType::Generic(ty, _) = &inner_tys[0] {
                            if ty == "HashMap" {
                                let ty = inner_tys[0]
                                    .to_api_type(generics, interfaces, enum_types, type_types)?;
                                return Ok(ApiType::QueryMap(ty.unwrap()));
                            }
                        }
                    }
                    let ty = Self::join_generic(
                        inner_tys, generics, interfaces, enum_types, type_types,
                    )?;
                    return Ok(ApiType::Query(ty));
                }
                "HashMap" => {
                    if inner_tys.len() != 2 {
                        return Err(Error::Ts(format!(
                            "HashMap must have two inner types, found {}. When wanting to use a custom type, set that on the metadata via `#[metadata(custom = [Type])]`",
                            inner_tys.len()
                        )));
                    }
                    let key_ty =
                        inner_tys[0].to_api_type(generics, interfaces, enum_types, type_types)?;
                    let value_ty =
                        inner_tys[1].to_api_type(generics, interfaces, enum_types, type_types)?;
                    return Ok(ApiType::Unknown(format!(
                        "Record<{}, {}>",
                        key_ty.unwrap(),
                        value_ty.unwrap()
                    )));
                }
                "Result" => {
                    if inner_tys.len() != 2 {
                        return Err(Error::Ts(format!(
                            "Result type must have two inner types, found {}. When wanting to use a custom type, set that on the metadata via `#[metadata(custom = [Type])]`",
                            inner_tys.len()
                        )));
                    }
                    let ok_ty =
                        inner_tys[0].to_api_type(generics, interfaces, enum_types, type_types)?;
                    let err_ty =
                        inner_tys[1].to_api_type(generics, interfaces, enum_types, type_types)?;
                    return Ok(ApiType::Unknown(format!(
                        "{} | {}",
                        ok_ty.unwrap(),
                        err_ty.unwrap()
                    )));
                }
                "Option" => {
                    let ty =
                        inner_tys[0].to_api_type(generics, interfaces, enum_types, type_types)?;
                    return Ok(ApiType::Unknown(format!("{} | null", ty.unwrap())));
                }
                _ => {}
            },
            Self::Tuple(tys) => {
                let tys = Self::join_generic(tys, generics, interfaces, enum_types, type_types)?;
                return Ok(ApiType::Unknown(format!("[{}]", tys)));
            }
            Self::Custom(ty) => {
                if interfaces.contains_key(ty)
                    || enum_types.contains_key(ty)
                    || type_types.contains_key(ty)
                {
                    return Ok(ApiType::Unknown(ty.to_string()));
                }
            }
            Self::CustomGeneric(ty, inner_tys) => {
                if interfaces.contains_key(ty)
                    || enum_types.contains_key(ty)
                    || type_types.contains_key(ty)
                {
                    let tys = Self::join_generic(
                        inner_tys, generics, interfaces, enum_types, type_types,
                    )?;
                    return Ok(ApiType::Unknown(format!("{}<{}>", ty, tys)));
                }
            }
            _ => {}
        };
        Err(Error::Ts(format!(
            "RustType '{:?}' couldn't be converted to TypeScript",
            self
        )))
    }

    fn join_generic(
        tys: &[RustType],
        generics: &[String],
        interfaces: &BTreeMap<String, String>,
        enum_types: &BTreeMap<String, String>,
        type_types: &BTreeMap<String, String>,
    ) -> Result<String> {
        Ok(tys
            .iter()
            .map(|t| t.to_api_type(generics, interfaces, enum_types, type_types))
            .collect::<Result<Vec<_>>>()?
            .iter()
            .map(|t| t.unwrap())
            .collect::<Vec<_>>()
            .join(", "))
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
enum ApiType {
    Unknown(String),
    Json(String),
    Path(String),
    PathTuple(String),
    Query(String),
    QueryMap(String),
}

impl ApiType {
    fn unwrap(&self) -> String {
        match self {
            ApiType::Unknown(t) => t.to_string(),
            ApiType::Json(t) => t.to_string(),
            ApiType::Path(t) => t.to_string(),
            ApiType::PathTuple(t) => t.to_string(),
            ApiType::Query(t) => t.to_string(),
            ApiType::QueryMap(t) => t.to_string(),
        }
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
    type_types: BTreeMap<String, String>,
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
            type_types: BTreeMap::new(),
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

        for type_type in self.type_types.values() {
            file.write_all(type_type.as_bytes())?;
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
