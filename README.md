# gluer

[![crates.io](https://img.shields.io/crates/v/gluer.svg)](https://crates.io/crates/gluer)
[![crates.io](https://img.shields.io/crates/d/gluer.svg)](https://crates.io/crates/gluer)
[![docs.rs](https://docs.rs/gluer/badge.svg)](https://docs.rs/gluer)

A wrapper for rust frameworks which addresses the persistent issue of redundant type and function definitions between the frontend and backend. At present, it exclusively supports the `axum` framework.

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
light_magic = "0.3.0"
```

## Disclaimer

Please be informed that this crate is in a very early state and is expected to work in not every case. Open a Issue if you encounter one! What works is:

- Defining the routing and api generation as outlined in [How to use](#how-to-use)
- Inferring the input and output types of functions (but only `Json<...>` for inputs)
- Converting them to ts types
- Generating the ts file with the functions and data types

## How to use

`gluer` generates an api endpoint `.ts` file which expects that you build your frontend statically and host it via `axum`'s static file serving. To use it, follow these steps:

### Step 1: Define Parameters and Functions

Start by using the `#[param]` and `fun!` macros to define your data structures and functions. These macros give `gluer` access to the necessary code for type inference and conversion.

```rust
use axum::{
    Json,
};
use gluer::{fun, param};

// Define a parameter with the param macro
#[param]
#[derive(Default, serde::Serialize)]
struct Book {
    // imagine some fields here
}

// Define functions with the fun macro
fun! {
    async fn root() -> Json<String> {
        "Hello, World!".to_string().into()
    }
    async fn book() -> Json<Book> {
        Book::default().into()
    }
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
use gluer::{add_route, fun};

// a part of the function above
fun! {
    async fn root() -> String {
        "Hello, World!".to_string()
    }
    async fn hello() -> Json<String> {
        "Hello, World!".to_string().into()
    }
}

let mut app: Router<()> = Router::new();

// Add non-API-important route without macro
app = app.route("/", get(root));

// Add API-important routes with the add_route macro
add_route!(app, "/hello-world", get(hello));
```

### Step 3: Generate API Specification

Generate the API specification file using the `gen_ts!` macro. This macro generates the TypeScript file at compile time.

```rust
use gluer::gen_ts;

// Generate the TypeScript API specification
gen_ts!("tests/api.ts");
```

### Complete Example

Below is a complete example demonstrating the use of gluer with `axum`:

```rust,no_run
use axum::{routing::get, Json, Router};
use gluer::{add_route, fun, gen_ts, param};

#[param]
#[derive(serde::Serialize, serde::Deserialize, Default)]
struct Hello {
    name: String,
}

fun! {
    async fn add_root(Json(hello): Json<Hello>) -> Json<Hello> {
        hello.into()
    }
    async fn fetch_root() -> Json<Hello> {
        Hello::default().into()
    }
}

#[tokio::main]
async fn main() {
    let mut app: Router<()> = Router::new();

    add_route!(app, "/", get(fetch_root).post(add_root));

    gen_ts!("tests/api.ts");

    let listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    axum::serve(listener, app).await.unwrap();
}
```
