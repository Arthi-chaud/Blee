use crate::error_handling::{ApiError, ApiResult};
use crate::responses::package::PackageResponse;
use crate::services;
use infrastructure::Database;
use rocket::serde::json::Json;
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
	openapi_get_routes_spec![settings: get_package]
}

/// Get a Single Package
#[openapi(tag = "Package")]
#[get("/<slug_or_uuid>")]
async fn get_package(db: Database, slug_or_uuid: String) -> ApiResult<PackageResponse> {
	db.run(move |conn| services::package::find(&slug_or_uuid, conn))
		.await
		.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(Json(v)))
}
