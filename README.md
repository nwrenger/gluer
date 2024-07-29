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
light_magic = "0.3.2"
```

## Features

Note: This crate is in an early stage and may not work in all cases. Please open an issue if you encounter any problems!

- Define routing and api generation as outlined in [How to use](#how-to-use).
- Infer input and output types of functions (currently supports only `Json<...>` for inputs).
- Convert Rust structs to TypeScript interfaces.
- Generate a TypeScript file with functions and data types.
- The generated TypeScript file does not depend on any external TypeScript libraries.

## How to use

`gluer` generates an api endpoint `.ts` file which expects that you build your frontend statically and host it via `axum`'s static file serving. To use it, follow these steps:

### Step 1: Define Structs and Functions

Start by using the `#[cached]` macro to define your data structures and functions. This macro gives `gluer` access to the necessary code for type inference and conversion.

```rust
use axum::{
    Json,
};
use gluer::cached;

// Define a struct with the cached macro
#[cached]
#[derive(Default, serde::Serialize)]
struct Book {
    // imagine some fields here
}

// Define the functions with the cached macro
#[cached]
async fn root() -> Json<String> {
    "Hello, World!".to_string().into()
}

#[cached]
async fn book() -> Json<Book> {
    Book::default().into()
}
```

### Step 2: Add Routes

Use the `add_route!` macro to add API-important routes to your router. Note that inline functions cannot generally be used due to Rust's limitations in inferring types in macros.

```rust
use axum::{
    routing::get,
    Router,
    Json,
};
use gluer::{add_route, cached};

// done like above
#[cached]
async fn root() -> String {
    "Hello, World!".to_string()
}

#[cached]
async fn hello() -> Json<String> {
    "Hello, World!".to_string().into()
}

let mut app: Router<()> = Router::new();

// Add non-API-important route without macro
app = app.route("/", get(root));

// Add API-important routes with the add_route macro
add_route!(app, "/hello-world", get(hello));
```

### Step 3: Generate API

Generate the API file using the `api!` macro. This macro generates the TypeScript file at compile time.

```rust
use gluer::api;

// Generate the TypeScript API
api!("tests/api.ts");
```

## Complete Example

Below is a complete example demonstrating the use of gluer with `axum`:

```rust,no_run
use axum::{routing::get, Json, Router};
use gluer::{add_route, api, cached};

#[cached]
async fn fetch_root() -> String {
    String::from("Hello, World!")
}

#[cached]
#[derive(serde::Serialize, serde::Deserialize, Default)]
struct Hello {
    name: String,
}

#[cached]
async fn add_root(Json(hello): Json<Hello>) -> Json<Hello> {
    hello.into()
}

#[tokio::main]
async fn main() {
    let mut app: Router<()> = Router::new();

    add_route!(app, "/", get(fetch_root).post(add_root));

    api!("tests/api.ts");

    let listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    axum::serve(listener, app).await.unwrap();
}
```
