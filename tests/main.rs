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
    #[meta(into = String)]
    age: AgeInner,
}

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
    Json(hello): Json<Hello<Hello<Huh<Huh<Hello<Age, String>>>, String>, String>>,
) -> Json<String> {
    Json(hello.name.to_string())
}

#[metadata]
#[derive(Serialize, Deserialize)]
enum Enum {
    A,
    B,
    C,
}

#[metadata]
async fn get_enum() -> Json<Enum> {
    Json(Enum::A)
}

#[tokio::test]
async fn main_test() {
    let app: Api<()> = Api::new()
        .route("/", extract!(get(get_enum)))
        .route("/:p", extract!(get(fetch_root).post(add_root)));

    app.generate_client("tests/api.ts", "").unwrap();

    let _listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    // starts the server, comment in and rename `_listener` to run it
    // axum::serve(listener, app.into_router()).await.unwrap();
}
