use crate::dto::extra::NewExtra;
use infrastructure::Database;
use rocket::{post, serde::json::Json};
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};
use shared::response_models::IndexResponse;

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
    openapi_get_routes_spec![settings: new_extra]
}

/// Create a new extra
#[openapi(tag = "Extras")]
#[post("/", format = "json", data = "<data>")]
async fn new_extra(_db: Database, data: Json<NewExtra<'_>>) -> Json<IndexResponse> {
    Json(IndexResponse {
        message: data.0.artist_name.to_string(),
    })
}
