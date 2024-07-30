use axum::{routing::get, Json};
use gluer::{extract, metadata, Api};

#[metadata]
async fn fetch_root() -> &'static str {
    "Hello, World!"
}

#[metadata]
#[derive(serde::Serialize, serde::Deserialize, Default)]
pub struct Hello {
    name: String,
}

#[metadata]
async fn add_root(Json(hello): Json<Hello>) -> Json<Vec<Hello>> {
    vec![hello].into()
}

#[tokio::test]
async fn main_test() {
    let app: Api<()> = Api::new().route("/", extract!(get(fetch_root).post(add_root)));

    app.api("tests/api.ts").unwrap();

    let _listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    // starts the server, comment in and rename `_listener` to run it
    // axum::serve(listener, app.into_router()).await.unwrap();
}
