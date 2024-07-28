use axum::{routing::post, Json, Router};
use gluer::{add_route, gen_spec};

#[derive(serde::Deserialize)]
struct Hello {
    _name: String,
}

async fn root(Json(_hello): Json<Hello>) -> Json<&'static str> {
    "Hello World!".into()
}

#[tokio::test]
#[ignore = "everlasting server"]
async fn main_test() {
    let mut app = Router::new();

    add_route!(app, "/", post(root));

    gen_spec!("tests/api.ts");

    let listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    axum::serve(listener, app).await.unwrap();
}
