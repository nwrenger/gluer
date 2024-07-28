use axum::{routing::get, Router};
use gluer::{add_route, gen_spec};

async fn root() -> &'static str {
    "Hello, World!"
}

#[tokio::test]
#[ignore = "everlasting server"]
async fn main_test() {
    let mut app = Router::new();

    add_route!(app, "/", get(root).post(root));

    gen_spec!("test", "0.1.0", "tests/test.yaml");

    let listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    axum::serve(listener, app).await.unwrap();
}
