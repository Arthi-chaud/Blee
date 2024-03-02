use crate::config::Config;
use crate::error_handling::{ApiError, ApiResult};
use crate::responses::artist::ArtistResponse;
use crate::{services, utils};
use diesel::Connection;
use domain::models::image::{Image, ImageType};
use infrastructure::Database;
use rocket::fs::TempFile;
use rocket::serde::json::Json;
use rocket::State;
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
	openapi_get_routes_spec![settings: get_artist, post_artist_image]
}

/// Find an Artist
#[openapi(tag = "Artists")]
#[get("/<slug_or_uuid>")]
async fn get_artist(_db: Database, slug_or_uuid: String) -> ApiResult<ArtistResponse> {
	_db.run(move |conn| services::artist::find(&slug_or_uuid, conn))
		.await
		.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(Json(v)))
}

/// Upload an Artist's image
#[openapi(tag = "Artists")]
#[post("/<slug_or_uuid>/poster", data = "<data>")]
async fn post_artist_image(
	db: Database,
	slug_or_uuid: String,
	data: TempFile<'_>,
	config: &State<Config>,
) -> ApiResult<Image> {
	let bytes = utils::temp_file_to_bytes_vec(data).await?;
	let c: Config = config.inner().clone();
	db.run(move |handle| {
		handle.transaction(move |conn| -> Result<Image, ApiError> {
			let artist =
				services::artist::find(&slug_or_uuid, conn).map_err(|e| ApiError::from(e))?;
			let new_poster = services::image::create(&bytes, ImageType::Poster, conn, &c)?;

			if let Some(image) = artist.poster {
				services::image::delete(&image.id, conn, &c).unwrap();
			}
			services::artist::set_poster(&new_poster.id, &artist.artist.id, conn)
				.map_err(|e| ApiError::from(e))?;
			Ok(new_poster)
		})
	})
	.await
	.map(|v| Json(v))
}
