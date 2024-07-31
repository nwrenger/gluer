use axum::{
    extract::{Path, Query},
    routing::get,
    Json,
};
use gluer::{extract, metadata, Api};
use std::collections::HashMap;

#[metadata]
async fn fetch_root(Query(test): Query<HashMap<String, String>>, Path(p): Path<usize>) -> String {
    test.get(&p.to_string()).unwrap().clone()
}

#[metadata]
#[derive(serde::Serialize, serde::Deserialize, Default)]
pub struct Hello {
    name: String,
    #[into(String)]
    age: Age,
    age2: Age,
}

#[metadata]
#[derive(serde::Serialize, serde::Deserialize, Default)]
struct Age {
    age: AgeInner,
}

#[metadata]
#[derive(serde::Serialize, serde::Deserialize, Default)]
struct AgeInner {
    age: u8,
}

#[metadata]
async fn add_root(Path(_): Path<usize>, Json(hello): Json<Hello>) -> Json<Vec<Hello>> {
    vec![hello].into()
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
