# gluer

[![crates.io](https://img.shields.io/crates/v/gluer.svg)](https://crates.io/crates/gluer)
[![crates.io](https://img.shields.io/crates/d/gluer.svg)](https://crates.io/crates/gluer)
[![docs.rs](https://docs.rs/gluer/badge.svg)](https://docs.rs/gluer)

A wrapper for Rust frameworks that eliminates redundant type and function definitions between the frontend and backend. Currently, it supports only the `axum` framework.

## Origin of the Name

The name "gluer" is inspired by the tool's primary function, **gluing together different parts of a Rust-based web application**. Just as glue binds different materials together to form a cohesive whole, `gluer` integrates various components of the frontend and backend, ensuring they work seamlessly without redundant code.

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
gluer = "0.8.2"
```

## Features

- Define routing and API generation as outlined in [How to use](#how-to-use).
- Everything is done on macro expansion (compile time), even the generating of the TypeScript file.
- Support `axum`'s types completely.
- Generate a TypeScript file with:
  - A custom prefix for the URL of the request
  - Functions to access the api, infers input and output types for that
  - Structs as Interfaces, supports changing the generated type via the `#[meta(...)]` attribute to be skipped, a different rust type or an optional (the `?` in TypeScript)
  - Enums as the TypeScript equivalent, enums with values are not supported, because of the lack of that feature in TypeScript
  - Types as the TypeScript equivalent
  - Supports converting docstring to the TypeScript equivalent, even of fields of structs and enums
  - Tuples as the TypeScript equivalent, also supports tuples in `axum`'s path
  - Supports converting rust specific types as `Result` as custom ones using the `custom = [Type, *]` attribute
  - Generics, even multiple and nested ones, look for that [here](#complete-examples)
  - No extra dependencies

## How to use

`gluer` generates an api endpoint `.ts` file. To use it, follow these steps:

### Step 1: Define Structs and Functions

To define your structs, functions and enums, use the `#[metadata]` macro along with the `#[meta(...)] `attribute. This enables the `generate!` macro to find those and converting them into the TypeScript equivalent.
```rust
use axum::{
    Json,
    extract::Path,
};
use gluer::metadata;
use serde::{Serialize, Deserialize};

/// Define a struct with the metadata macro
/// Note: This is a docstring and will be
/// converted to the TypeScript equivalent
#[metadata(custom = [Result])]
#[derive(Serialize, Deserialize)]
struct Book {
    /// This will also be converted to a docstring
    name: String,
    // When you use types as `Result`, `Option` or `Vec` the
    // macro sees them as a default rust type, meaning when
    // you wanting to use custom ones you have to specify that
    // via the `custom` attribute on `#[metadata]`
    some_result: Result<String>,
    // Sometimes you don't have access to certain data types,
    // so you can override them using `#[meta(into = Type)]`
    // or skip them entirely via `#[meta(skip)]`
    // or set them to be optional via `#[meta(optional)]`
    // (-> an interface field with a `?`)
    #[meta(into = String)]
    user: User,
    #[meta(skip)]
    borrower: User,
    #[meta(optional)]
    reservation: String,
}

// Everything you want to use, even if it's just a
// dependency of struct or type, needs to be declared
// with the `#[metadata]` macro
#[metadata]
type Result<T> = std::result::Result<T, String>;

#[derive(Default, Serialize, Deserialize)]
struct User {
    name: String,
    password: String,
}

// Define an enum with the metadata macro
// Note: Enums with values are not supported
#[metadata]
#[derive(Default, Serialize, Deserialize)]
enum BookState {
    #[default]
    None,
    Reserved,
    Borrowed,
}

// Define the functions with the metadata macro
#[metadata]
async fn root() -> Json<String> {
    "Hello, World!".to_string().into()
}

// Supports axum's input types
#[metadata]
async fn book(Json(b): Json<Book>) -> Json<Book> {
    Json(b)
}

// Also tuples in paths
#[metadata]
async fn path(Path(p): Path<(String, String)>) -> Json<(String, String)> {
    p.into()
}

// Supports enums
#[metadata]
async fn book_state() -> Json<BookState> {
    BookState::default().into()
}
```

### Step 2: Add Routes && Generate API

Use the `generate!` macro to define your router and other telemetry to generate the API. You have to define the `output` location of the TypeScript file and the `routes`. Note that inline functions cannot be used in the `router` field because the function names in the generated TypeScript file are inferred from the handler function names.

```rust, ignore
use axum::{
    routing::get,
    Json,
    Router,
    extract::Path,
};
use gluer::{generate, metadata};

// without `#[metadata]`, it's non-API-important
async fn root() -> String {
    "Hello, World!".to_string()
}

// done like above
#[metadata]
async fn hello(Path(h): Path<String>) -> Json<String> {
    h.into()
}

let mut app: Router<()> = generate! {
    routes = {
        // Add API-important inside the routes field
        "hello" = get(hello),
    },
    output = "tests/api.ts",
}
// Add non-API-important outside the macro
.route("/", get(root));
```

### Additional Notes

The `generate!` macro includes several optional fields that you can customize to fit your needs. One of these is `prefix`, which allows you to set a URL prefix for your API routes. By default, this is an empty string (`""`), but you can change it to something like `"/api"`. Be aware that the prefix should not end with a `/`.

Another customizable option is `files`, which defines the Rust project files that contain the source code for handler functions and dependencies. This can be a single string literal (e.g., `"src"`) or an array of string literals (e.g., `["src/db", "src", "src/error.rs"]`). These paths are used to extract type information for the TypeScript client. The default value is `"src"`, which should be suitable in most scenarios.

Finally, once the router is generated by the macro, along with the API, you can immediately use it to start your server or perform other operations.

## Complete Examples

Below is a complete example demonstrating the use of `gluer` with `axum`. [Here](/tests/api.ts) is the generated output. You can also look into [this bigger project](https://github.com/wrenger/schiller-lib) or [this smaller project](https://github.com/nwrenger/shitboard)  which uses `gluer`.

```rust
use axum::{
    extract::{Path, Query},
    routing::get,
    Json, Router,
};
use gluer::{generate, metadata};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// An example of a simple function with a `Path` and a `Query` extractor
#[metadata]
async fn fetch_root(Query(test): Query<HashMap<String, String>>, Path(p): Path<usize>) -> String {
    test.get(&p.to_string()).unwrap().clone()
}

#[metadata]
#[derive(Deserialize)]
struct QueryOptions {
    id: usize,
    query: String,
}

// And one with a `Query` extractor using a struct
#[metadata]
async fn fetch_other(Query(test): Query<QueryOptions>) -> String {
    format!("{}: {}", test.id, test.query)
}

// Generics are supported, multiple even
// Note: This is not a docstring and won't
// be converted
#[metadata]
#[derive(Serialize, Deserialize, Default)]
pub struct Hello<T: Serialize, S> {
    name: S,
    vec: Vec<T>,
}

/// Might want to look into the `api.ts` file to see the docstring for this struct
#[metadata]
#[derive(Serialize, Deserialize, Default)]
struct Age {
    /// Even supports docstring on fields and optional fields
    #[meta(optional)]
    age: AgeInner,
}

#[metadata]
#[derive(Serialize, Deserialize, Default)]
struct AgeInner {
    // Thats quite big and also supported
    /// This gets converted to a `string` on the TypeScript side
    /// because `numbers` there cannot be greater than 64 bits
    age: u128,
}

#[metadata]
#[derive(Serialize, Deserialize, Default)]
struct Huh<T> {
    huh: T,
}

// Even deep nested generics are supported and tagging default rust types as Custom
/// Docstrings for functions are also supported
#[metadata(custom = [Result])]
async fn add_root(
    Path(_): Path<usize>,
    Json(hello): Json<Result<Hello<Hello<Huh<Huh<Hello<Age, String>>>, String>, String>>>,
) -> Json<Result<String>> {
    Json(Ok(hello.unwrap().name.to_string()))
}

#[metadata]
#[derive(Serialize, Deserialize)]
enum Alphabet {
    A,
    B,
    C,
    // etc
}

// Even tuples are supported
#[metadata]
async fn get_alphabet(Path(r): Path<(Alphabet, S)>) -> Json<(Alphabet, S)> {
    Json(r)
}

/// An example how an api error type could look like
#[metadata]
#[derive(Serialize, Deserialize, Debug)]
enum Error {
    /// Normal 404 error
    NotFound,
    /// Internally something really bad happened
    InternalServerError,
}

// And types?!?
#[metadata]
type Result<T> = std::result::Result<T, Error>;

#[metadata]
type S = String;

#[tokio::main]
async fn main() {
    let _app: Router<()> = generate! {
        routes = { // required
            "/:p" = get(fetch_root).post(add_root),
            "/char/:path/metadata/:path" = get(get_alphabet),
            "/other" = get(fetch_other),
        },
        files = "tests", // Make sure to remove this when copying this example into a normal project
        output = "tests/api.ts", //required
    };

    let _listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    // starts the server, comment in and rename `_app` and `_listener` to run it
    // axum::serve(listener, app).await.unwrap();
}
```
