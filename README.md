# gluer

[![crates.io](https://img.shields.io/crates/v/gluer.svg)](https://crates.io/crates/gluer)
[![crates.io](https://img.shields.io/crates/d/gluer.svg)](https://crates.io/crates/gluer)
[![docs.rs](https://docs.rs/gluer/badge.svg)](https://docs.rs/gluer)

A wrapper for rust frameworks which addresses the persistent issue of redundant type and function definitions between the frontend and backend. At present, it exclusively supports the `axum` framework.

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
light_magic = "0.2.1"
```

## Disclaimer

Please be informed that this crate is in a very early state and is expected to work in not every case. Open a Issue if you encounter one! What works is:

- Defining the routing and api generation as outlined in [How to use](#how-to-use)
- Inferring the input and output types of functions (but only `Json<...>` for inputs)
- Converting them to ts types
- Generating the ts file with the functions and data types

## How to use

Firstly you have to use the `add_route!` macro when adding api important routes to your router:

```rust
use axum::{
    routing::{get, post},
    Router,
    Json,
};
use gluer::add_route;

async fn root() -> Json<&'static str> {
    "Hello, World!".into()
}

let mut app: Router<()> = Router::new();

// Not api important, so adding without macro
app = app.route("/", get(root));

// You cannot use inline functions because of rust limitations of inferring types in macros
add_route!(app, "/", post(root));
add_route!(app, "/user", post(root).delete(root));
```

Then you only have to use the `gen_spec!` macro which generates after specifying the path the api on comptime:

```rust
use gluer::gen_spec;

gen_spec!("tests/api.ts");
```

### Complete Example

```rust,no_run
use axum::{routing::post, Json, Router};
use gluer::{add_route, gen_spec};

#[derive(serde::Deserialize)]
struct Hello {
    name: String,
}

async fn root(Json(hello): Json<Hello>) -> Json<String> {
    hello.name.into()
}

#[tokio::main]
async fn main() {
    let mut app = Router::new();

    add_route!(app, "/", post(root));

    gen_spec!("tests/api.ts");

    let listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    axum::serve(listener, app).await.unwrap();
}
```
