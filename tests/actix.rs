use actix_web::{web, App, HttpServer};
use gluer::{generate, metadata};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[metadata]
async fn fetch_root(test: web::Query<HashMap<String, String>>, p: web::Path<usize>) -> String {
    test.get(&p.to_string()).unwrap().clone()
}

#[metadata]
#[derive(Deserialize)]
struct QueryOptions {
    id: usize,
    query: String,
}

#[metadata]
async fn fetch_other(test: web::Query<QueryOptions>) -> String {
    format!("{}: {}", test.id, test.query)
}

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
    /// This gets converted to a `string` on the TypeScript side
    /// because `numbers` there cannot be greater than 64 bits
    age: u128,
}

#[metadata]
#[derive(Serialize, Deserialize, Default)]
struct Huh<T> {
    huh: T,
}

/// Docstrings for functions are also supported
#[metadata(custom = [Result])]
async fn add_root(
    _: web::Path<usize>,
    hello: web::Json<Result<Hello<Hello<Huh<Huh<Hello<Age, String>>>, String>, String>>>,
) -> web::Json<Result<String>> {
    web::Json(Ok(hello.into_inner().unwrap().name.to_string()))
}

#[metadata]
#[derive(Serialize, Deserialize)]
enum Alphabet {
    A,
    B,
    C,
}

#[metadata]
async fn get_alphabet(r: web::Path<(Alphabet, S)>) -> web::Json<(Alphabet, S)> {
    web::Json(r.into_inner())
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

#[metadata]
type Result<T> = std::result::Result<T, Error>;

#[metadata]
type S = String;

#[actix_web::test]
async fn main_test() {
    let _server = HttpServer::new(|| {
        let scope = generate! {
            routes = { // required
                "/{p}" = get(fetch_root).post(add_root),
                "/char/{path}/metadata/{path}" = get(get_alphabet),
                "/other" = get(fetch_other),
            },
            files = "tests", // Make sure to remove this when copying this example into a normal project
            output = "tests/api_actix.ts", // required
        };

        App::new().service(scope)
    })
    .bind(("127.0.0.1", 8080))
    .unwrap();
    // starts the server, comment in and rename `_server` to run it
    // server.run().await.unwrap();
}
