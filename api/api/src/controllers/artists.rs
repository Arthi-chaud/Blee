use crate::config::Config;
use crate::database::Database;
use crate::dto::artist::ArtistWithPosterResponse;
use crate::dto::image::ImageResponse;
use crate::error_handling::{ApiError, ApiRawResult, ApiResult};
use crate::{services, utils};
use entity::sea_orm_active_enums::ImageTypeEnum;
use rocket::fs::TempFile;
use rocket::response::status;
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
async fn get_artist(db: Database<'_>, slug_or_uuid: String) -> ApiResult<ArtistWithPosterResponse> {
	services::artist::find(&slug_or_uuid, db.into_inner())
		.await
		.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(Json(v)))
}

/// Upload an Artist's image
#[openapi(tag = "Artists")]
#[post("/<slug_or_uuid>/poster", data = "<data>")]
async fn post_artist_image(
	db: Database<'_>,
	slug_or_uuid: String,
	data: TempFile<'_>,
	config: &State<Config>,
) -> ApiRawResult<status::Created<Json<ImageResponse>>> {
	let bytes = utils::temp_file_to_bytes_vec(data).await?;
	let c: Config = config.inner().clone();
	let conn = db.into_inner();

	let artist = services::artist::find(&slug_or_uuid, conn)
		.await
		.map_err(|e| ApiError::from(e))?;
	let new_poster = services::image::create(&bytes, ImageTypeEnum::Poster, conn, &c)
		.await
		.map_err(|e| ApiError::from(e))?;

	if let Some(image) = artist.poster {
		let _ = services::image::delete(&image.id, conn, &c);
	}
	services::artist::set_poster(&new_poster.id, &artist.artist.id, conn)
		.await
		.map_err(|e| ApiError::from(e))?;
	Ok(status::Created::new("").body(Json(ImageResponse::from(new_poster))))
}
