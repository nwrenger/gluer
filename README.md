# gluer

[![crates.io](https://img.shields.io/crates/v/gluer.svg)](https://crates.io/crates/gluer)
[![crates.io](https://img.shields.io/crates/d/gluer.svg)](https://crates.io/crates/gluer)
[![docs.rs](https://docs.rs/gluer/badge.svg)](https://docs.rs/gluer)

A wrapper for rust frameworks which addresses the persistent issue of redundant type definitions between the frontend and backend. At present, it exclusively supports the `axum` framework.

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
light_magic = "0.1.0"
```

## Disclaimer

Please be informed that this crate is in a very early state and is expected to work in not every case. Open a Issue if you encounter one!

## How to use

Firstly you have to use the `add_route!` macro when adding api important routes to your router:

```rust
use axum::{
    routing::{get, post},
    Router,
};
use gluer::add_route;

async fn root() -> &'static str {
    "Hello, World!"
}

let mut app = Router::new();

// Not api important, so adding without macro
app = app.router(get(root));

// You currently cannot use inline functions, just path to the functions inside the methods (meaning `path(|| async &'static "Hello World!")` won't work!)
add_route!(app, "/", post(root));
add_route!(app, "/user", post(root).delete(root));
```

Then you only have to use the `gen_spec!` which generates after specifying title, version and path the openapi doc on comptime:

```rust
use gluer::gen_spec;

gen_spec!("test", "0.1.0", "tests/test.json");
```

Then use a library like `openapi-typescript` to generate your fitting `TS Client` code!

### Complete Example

```rust
use axum::{
    routing::{get, post},
    Router,
};
use gluer::{add_route, gen_spec};

async fn root() -> &'static str {
    "Hello, World!"
}

#[tokio::main]
async fn main() {
    let mut app = Router::new();

    add_route!(app, "/", get(root).post(root));

    gen_spec!("test", "0.1.0", "tests/test.json");

    let listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    axum::serve(listener, app).await.unwrap();
}
```
