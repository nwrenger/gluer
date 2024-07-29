use axum::{routing::get, Json, Router};
use gluer::{add_route, cached, gen_ts};
use hello::{Hello, CACHED_STRUCT_HELLO};

#[cached]
async fn fetch_root() -> String {
    String::from("Hello, World!")
}

mod hello {
    use gluer::cached;

    #[cached]
    #[derive(serde::Serialize, serde::Deserialize, Default)]
    pub struct Hello {
        name: String,
    }
}

#[cached]
async fn add_root(Json(hello): Json<Hello>) -> Json<Hello> {
    hello.into()
}

#[tokio::test]
async fn main_test() {
    let mut app: Router<()> = Router::new();
    let mut routes = vec![];

    add_route!(routes, app, "/", get(fetch_root).post(add_root));

    gen_ts(
        "tests/api.ts",
        routes,
        &[CACHED_FN_FETCH_ROOT, CACHED_FN_ADD_ROOT],
        &[CACHED_STRUCT_HELLO],
    )
    .unwrap();

    let listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();
    axum::serve(listener, app).await.unwrap(); // prevent everlasting server
}
