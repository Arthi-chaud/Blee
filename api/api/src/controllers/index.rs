use infrastructure::Database;
use rocket::{get, serde::json::Json};
use serde::Serialize;

#[derive(Serialize)]
pub struct IndexResponse {
	pub message: &'static str,
}

#[get("/")]
pub fn index(_db: Database) -> Json<IndexResponse> {
	Json(IndexResponse {
		message: "Welcome to Blee's API. Hit '/swagger' for more!",
	})
}
