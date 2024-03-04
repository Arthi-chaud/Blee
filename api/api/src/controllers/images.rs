use crate::config::Config;
use crate::error_handling::ApiError;
use crate::services;
use infrastructure::Database;
use rocket::fs::NamedFile;
use rocket::serde::uuid::Uuid;
use rocket::State;
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};
use std::path::Path;

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
	openapi_get_routes_spec![settings: serve_image]
}

/// Get an Image
#[openapi(tag = "Images")]
#[get("/<uuid>")]
async fn serve_image(
	uuid: Uuid,
	db: Database,
	config: &State<Config>,
) -> Result<NamedFile, ApiError> {
	let image_row = db
		.run(move |conn| services::image::get(&uuid, conn).map_err(|e| ApiError::from(e)))
		.await?;
	let image_path = Path::new(&config.data_folder)
		.join(image_row.id.to_string())
		.join("image.webp");
	NamedFile::open(&image_path)
		.await
		.map_err(|_| ApiError::ImageServingError)
}
