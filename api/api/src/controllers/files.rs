use crate::error_handling::{ApiError, ApiResult};
use crate::services;
use domain::models::file::File;
use infrastructure::Database;
use rocket::serde::json::Json;
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
	openapi_get_routes_spec![settings: get_file]
}

/// Get a Single File
#[openapi(tag = "Files")]
#[get("/<uuid>")]
async fn get_file(db: Database, uuid: Uuid) -> ApiResult<File> {
	db.run(move |conn| services::file::find(&uuid, conn))
		.await
		.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(Json(v)))
}
