#![doc = include_str!("../README.md")]

pub(crate) mod codegen;
pub(crate) mod framework;
pub(crate) mod parsing;
pub(crate) mod util;

use proc_macro::{self as pc};

/// Use before structs, functions, enums or types to be findable by the `generate!` macro.
///
/// ## Attributes
/// - `custom = [Type, *]`: Specify here types which are named equally to std types but are custom.
///
/// ## Struct Attributes
///
/// - `#[meta(into = Type)]`: Specify a type to convert the field into.
/// - `#[meta(skip)]`: Skip the field.
/// - `#[meta(optional)]`: Make a field optional.
#[proc_macro_attribute]
pub fn metadata(args: pc::TokenStream, input: pc::TokenStream) -> pc::TokenStream {
    match parsing::metadata::inner(args.into(), input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

/// Generates a TypeScript API client and axum compatible router.
///
/// ## Parameters
///
/// - `prefix`: An optional parameter that allows you to specify a prefix for all generated routes. This can be useful if your API is hosted under a common base path (e.g., `/api`).
/// - `routes`: A required parameter that specifies the API routes for which the TypeScript client and resulting Router will be generated. Each route is defined by a URL path (which can include parameters) followed by one or more HTTP methods (e.g., `get`, `post`) and their corresponding handler functions.
/// - `files`: An optional parameter that specifies the files containing the Rust source files that define the handlers and dependencies. This can be either a single string literal (e.g., `"src"`) or an array of string literals (e.g., `["src/db", "src", "src/error.rs"]`). These paths are used to extract type information for the TypeScript client. Ensure that these paths are correct and point to the appropriate directories or files. The default of `"src"` should handle most cases appropriately.
/// - `output`: A required parameter that specifies the path to the output file where the generated JavaScript/TypeScript client code will be written. Ensure that this path is correct and points to a writable location. The extension of the output file determines whether a JavaScript or TypeScript client is generated (e.g., `.ts` for TypeScript, `.js` for JavaScript).
///
/// ## Note
///
/// - **Prefix URL:** The `prefix` parameter is used to prepend a common base path to all routes. It should not end with a `/`. If the prefix is not provided, it defaults to an empty string (`""`), meaning no prefix will be added.
///
/// ## Example with Axum
///
/// ```rust, ignore
/// use axum::Router;
/// use gluer::{generate, metadata};
///
/// // Define a handler function
/// #[metadata]
/// fn root() -> String {
///     "root".to_string()
/// }
///
/// // Use the `generate` macro to create the API client and router
/// let _app: Router<()> = generate! {
///     prefix = "", // Sets the prefix to `""`
///     // This can be omitted due to being the same as the default value
///     routes = { // Defines the API routes
///         "/" = get(root), // Route for the root path, using the `root` handler for GET requests
///     },
///     files = "src", // Specifies a single directory containing the handler implementations
///     // This can be omitted due to being the same as the default value
///     // You can also specify multiple directories:
///     // files = ["src/db", "src"],
///
///     output = "tests/api.ts", // Specifies the output file for the generated JavaScript/TypeScript client
///     // The extension of the output file determines whether a JavaScript or TypeScript client is generated.
///     // In this case, a TypeScript client will be generated due to the `.ts` extension.
/// };
/// ```
#[proc_macro]
pub fn generate(input: pc::TokenStream) -> pc::TokenStream {
    match parsing::generate::inner(input.into()) {
        Ok(result) => result.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
