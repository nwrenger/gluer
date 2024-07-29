use axum::{routing::get, Json, Router};
use gluer::{add_route, fun, gen_ts, param};

#[param]
#[derive(serde::Serialize, serde::Deserialize, Default)]
struct Hello {
    name: String,
}

fun! {
    async fn add_root(Json(hello): Json<Hello>) -> Json<Hello> {
        hello.into()
    }
    async fn fetch_root() -> Json<Hello> {
        Hello::default().into()
    }
}

#[tokio::test]
#[ignore = "everlasting server"]
async fn main_test() {
    let mut app: Router<()> = Router::new();

    add_route!(app, "/", get(fetch_root).post(add_root));

    gen_ts!("tests/api.ts");

    let listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    axum::serve(listener, app).await.unwrap();
}
