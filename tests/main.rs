use axum::{
    extract::{Path, Query},
    Json, Router,
};
use gluer::{generate, metadata};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// An example of a simple function with a `Path` and `Query` extractor
#[metadata]
async fn fetch_root(Query(test): Query<HashMap<String, String>>, Path(p): Path<usize>) -> String {
    test.get(&p.to_string()).unwrap().clone()
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
    /// Even supports docstring on fields
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

#[tokio::test]
async fn main_test() {
    let _app: Router<()> = generate! {
        routes = { // required
            "/:p" = get(fetch_root).post(add_root),
            "/char/:path/metadata/:path" = get(get_alphabet),
        },
        files = "tests", // Make sure to remove this when copying this example into a normal project
        output = "tests/api.ts", //required
    };

    let _listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    // starts the server, comment in and rename `_listener` to run it
    // axum::serve(listener, app).await.unwrap();
}
