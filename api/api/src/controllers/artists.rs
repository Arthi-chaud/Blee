use crate::config::Config;
use crate::database::Database;
use crate::dto::artist::{ArtistFilter, ArtistSort, ArtistWithPosterResponse};
use crate::dto::image::ImageResponse;
use crate::dto::page::{Page, Pagination};
use crate::dto::sort::{build_sort, SortOrder};
use crate::error_handling::{ApiError, ApiPageResult, ApiRawResult, ApiResult};
use crate::{services, utils};
use entity::artist;
use rocket::fs::TempFile;
use rocket::response::status;
use rocket::serde::json::Json;
use rocket::serde::uuid::Uuid;
use rocket::State;
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
	openapi_get_routes_spec![settings: get_artists, get_artist, post_artist_image]
}

/// Get many artists
#[openapi(tag = "Artists")]
#[get("/?<package>&<sort>&<order>&<pagination..>")]
async fn get_artists(
	db: Database<'_>,
	package: Option<Uuid>,
	sort: Option<ArtistSort>,
	order: Option<SortOrder>,
	pagination: Pagination,
) -> ApiPageResult<ArtistWithPosterResponse> {
	services::artist::find_many(
		&ArtistFilter { package },
		build_sort(sort, order),
		&pagination,
		db.into_inner(),
	)
	.await
	.map(|items| Page::from(items))
	.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(v))
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
	let conn = db.into_inner();

	let artist = services::artist::find(&slug_or_uuid, conn)
		.await
		.map_err(|e| ApiError::from(e))?;
	let new_poster = services::image::save_image(
		&bytes,
		crate::dto::image::ImageType::Poster,
		artist.poster.map(|p| p.id),
		&artist.artist.id,
		artist::Entity,
		artist::Column::Id,
		artist::Column::PosterId,
		conn,
		config,
	)
	.await?;

	Ok(status::Created::new("").body(Json(ImageResponse::from(new_poster))))
}
