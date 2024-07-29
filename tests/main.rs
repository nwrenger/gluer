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

#[tokio::test]
#[ignore = "everlasting server"]
async fn main_test() {
    let mut app: Router<()> = Router::new();

    add_route!(app, "/", get(fetch_root).post(add_root));

    // at the end of all macros so no issue here
    api!("tests/api.ts");

    let listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    axum::serve(listener, app).await.unwrap();
}
