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
gluer = "0.4.2"
```

## Features

Note: This crate is in an early stage and may not work in all cases. Please open an issue if you encounter any problems!

- Define routing and API generation as outlined in [How to use](#how-to-use).
- Infer input and output types of functions.
- Support `axum`'s types completely.
- Convert Rust structs to TypeScript interfaces.
  - Via the `[#metadata]` attribute macro with the `#[meta(...)]` attribute
- Generate a TypeScript file with:
  - Functions
  - Data types as Interfaces
  - Generics, even multiple and nested ones, look for that [here](#complete-example)
- Using no extra dependencies in the generated TypeScript file.

## How to use

`gluer` generates an api endpoint `.ts` file which expects that you build your frontend statically and host it via `axum`'s static file serving. To use it, follow these steps:

### Step 1: Define Structs and Functions

To define your data structures and functions, use the `#[metadata]` macro along with the `#[meta(...)] `attribute. This macro enables `gluer` to generate metadata for these structures and functions. It does so by implementing the `metadata` function on structs or by creating a struct that implements both the `metadata` function and the handler-specific function.
```rust
use axum::{
    Json,
};
use gluer::metadata;

// Define a struct with the metadata macro
#[metadata]
#[derive(Default, serde::Serialize)]
struct Book {
    name: String,
    // Sometimes you don't have access to certain data types, 
    // so you can override them using `#[meta(into = Type)]` 
    // or skip them entirely via `#[meta(skip)]`
    #[meta(into = String)]
    user: User,
    #[meta(skip)]
    borrower: User,
}

#[derive(Default, serde::Serialize)]
struct User {
    name: String,
    password: String,
}

// Define the functions with the metadata macro
#[metadata]
async fn root() -> Json<String> {
    "Hello, World!".to_string().into()
}

#[metadata]
async fn book() -> Json<Book> {
    Book::default().into()
}
```

### Step 2: Add Routes

Use the `Api` wrapper around `axum`'s Router to add routes. Utilize the `extract!` macro to gather all necessary information from the functions. Note that inline functions cannot be used, as the function names of the generated TypeScript file are inferred from the handler function names.

```rust
use axum::{
    routing::get,
    Json,
};
use gluer::{Api, extract, metadata};

// without `#[metadata]`, it's non-API-important
async fn root() -> String {
    "Hello, World!".to_string()
}

// done like above
#[metadata]
async fn hello() -> Json<String> {
    "Hello, World!".to_string().into()
}

let mut app: Api<()> = Api::new()
    // Add non-API-important routes or state by accessing axum's Router directly via inner_router
    .inner_router(|f| f.route("/", get(root)))
    // Add API-important routes with the route function
    .route("/hello-world", extract!(get(hello)));
```

### Step 3: Generate API

Generate the API file using the `generate_client` function on the `Api` struct. This generates the TypeScript file.

```rust,no_run
use gluer::Api;

let app: Api<()> = Api::new();

app.generate_client("tests/api.ts");
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
    // #[meta(into = String)]
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

// Even deep nested generics are supported
#[metadata]
async fn add_root(
    Path(_): Path<usize>,
    Json(hello): Json<Hello<Hello<Huh<Age>, String>, String>>,
) -> Json<String> {
    Json(hello.name.to_string())
}

#[tokio::test]
async fn main_test() {
    let app: Api<()> = Api::new().route("/:p", extract!(get(fetch_root).post(add_root)));

    app.generate_client("tests/api.ts").unwrap();

    let _listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    // starts the server, comment in and rename `_listener` to run it
    // axum::serve(listener, app.into_router()).await.unwrap();
}
```
