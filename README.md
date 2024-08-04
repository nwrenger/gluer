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
gluer = "0.6.0"
```

## Features

- Define routing and API generation as outlined in [How to use](#how-to-use).
- Infer input and output types of functions.
- Support `axum`'s types completely.
- Convert Rust structs to TypeScript interfaces.
  - Via the `[#metadata]` attribute macro with the `#[meta(...)]` attribute
- Generate a TypeScript file with:
  - Functions to access the api
  - Supports a custom base URL
  - Structs as Interfaces
  - Enums as Types, enums with values are not supported, because of the lack of that feature in TypeScript
  - Tuples as the TypeScript equivalent, also supports tuples in `axum`'s path 
  - Types as the TypeScript equivalent
  - Supports converting rust specific types as `Result` using the `custom = [Type, *]` attribute as custom ones
  - Generics, even multiple and nested ones, look for that [here](#complete-example)
- Using no extra dependencies in the generated TypeScript file.

## How to use

`gluer` generates an api endpoint `.ts` file. To use it, follow these steps:

### Step 1: Define Structs and Functions

To define your structs, functions and enums, use the `#[metadata]` macro along with the `#[meta(...)] `attribute. This macro enables `gluer` to generate metadata for these structures, functions and enums. It does so by implementing the `metadata` function on structs and enums or by creating a struct that implements both the `metadata` function and the handler-specific function.
```rust
use axum::{
    Json,
    extract::Path,
};
use gluer::metadata;
use serde::{Serialize, Deserialize};

// Define a struct with the metadata macro
#[metadata(custom = [Result])]
#[derive(Serialize, Deserialize)]
struct Book {
    name: String,
    // When you use types as `Result`, `Option` or `Vec` the 
    // macro sees them as a default rust type, meaning when
    // you wanting to use custom ones you have to specify that
    // via the `custom` attribute on `#[metadata]`
    some_result: Result<String>,
    // Sometimes you don't have access to certain data types, 
    // so you can override them using `#[meta(into = Type)]` 
    // or skip them entirely via `#[meta(skip)]`
    #[meta(into = String)]
    user: User,
    #[meta(skip)]
    borrower: User,
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

### Step 2: Add Routes

Use the `Api` wrapper around `axum`'s Router to add routes. Utilize the `extract!` macro to gather all necessary information from the functions. Note that inline functions cannot be used, as the function names of the generated TypeScript file are inferred from the handler function names.

```rust
use axum::{
    routing::get,
    Json,
    extract::Path,
};
use gluer::{Api, extract, metadata};

// without `#[metadata]`, it's non-API-important
async fn root() -> String {
    "Hello, World!".to_string()
}

// done like above
#[metadata]
async fn hello(Path(h): Path<String>) -> Json<String> {
    h.into()
}

let mut app: Api<()> = Api::new()
    // Add non-API-important routes or state by
    // accessing axum's Router directly via inner_router
    .inner_router(|f| f.route("/", get(root)))
    // Add API-important routes with the route function
    .route("/:hello", extract!(get(hello)));
```

### Step 3: Generate API

Generate the API file using the `generate_client` function on the `Api` struct. This generates the TypeScript file. You can specify a different `base` and a path, where the file should be generated to and with what name.

```rust,no_run
use gluer::Api;

let app: Api<()> = Api::new();

app.generate_client("tests/api.ts", "");
```

### Step 4: Use the Wrapped Router

To start your server, get the inner router using the `into_router` function.

```rust,no_run
use gluer::Api;

#[tokio::main]
async fn main() {
    let app: Api<()> = Api::new();

    let listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
            .await
            .unwrap();
    axum::serve(listener, app.into_router()).await.unwrap();
}
```

## Complete Example

Below is a complete example demonstrating the use of `gluer` with `axum`:

```rust
use axum::{
    extract::{Path, Query},
    routing::get,
    Json,
};
use gluer::{extract, metadata, Api};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[metadata]
async fn fetch_root(Query(test): Query<HashMap<String, String>>, Path(p): Path<usize>) -> String {
    test.get(&p.to_string()).unwrap().clone()
}

// Generics are supported, multiple even
#[metadata]
#[derive(Serialize, Deserialize, Default)]
pub struct Hello<T: Serialize, S> {
    name: S,
    vec: Vec<T>,
}

#[metadata]
#[derive(Serialize, Deserialize, Default)]
struct Age {
    age: AgeInner,
}

#[metadata]
#[derive(Serialize, Deserialize, Default)]
struct AgeInner {
    age: u8,
}

#[metadata]
#[derive(Serialize, Deserialize, Default)]
struct Huh<T> {
    huh: T,
}

// Even deep nested generics are supported and tagging default rust types as Custom
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
async fn get_alphabet(Path(r): Path<(Alphabet, String)>) -> Json<(Alphabet, String)> {
    Json(r)
}

#[metadata]
#[derive(Serialize, Deserialize, Debug)]
enum Error {
    NotFound,
    InternalServerError,
}

// And types?!?
#[metadata]
type Result<T> = std::result::Result<T, Error>;

#[tokio::main]
async fn main() {
    let app: Api<()> = Api::new()
        .route("/:p", extract!(get(fetch_root).post(add_root)))
        .route("/char/:path/metadata/:path", extract!(get(get_alphabet)));

    app.generate_client("tests/api.ts", "").unwrap();

    let _listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    // starts the server, comment in and rename `_listener` to run it
    // axum::serve(listener, app.into_router()).await.unwrap();
}
```
