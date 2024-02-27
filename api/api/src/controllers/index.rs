use infrastructure::Database;
use rocket::{get, serde::json::Json};
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};
use shared::response_models::IndexResponse;

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
	openapi_get_routes_spec![settings: index]
}

#[openapi(tag = "Index")]
#[get("/")]
async fn index(_db: Database) -> Json<IndexResponse> {
	Json(IndexResponse {
		message: "Hello World".to_owned(),
	})
}
