use axum::{
    extract::{Path, Query},
    routing::get,
    Json, Router,
};
use gluer::{generate, metadata, route};
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
async fn get_alphabet(Path(r): Path<(Alphabet, S)>) -> Json<(Alphabet, S)> {
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

#[metadata]
type S = String;

#[tokio::test]
async fn main_test() {
    let mut _app: Router = Router::new();

    route!(_app, "/:p", get(fetch_root).post(add_root));
    route!(_app, "/char/:path/metadata/:path", get(get_alphabet));

    // Make sure to change "tests" to "src" when copying this example
    generate!("tests", "tests/api.ts", "");

    let _listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    // starts the server, comment in and rename `_listener` to run it
    // axum::serve(listener, app).await.unwrap();
}
