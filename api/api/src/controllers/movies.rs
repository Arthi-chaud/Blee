use crate::config::Config;
use crate::database::Database;
use crate::dto::artist::ArtistResponse;
use crate::dto::chapter::ChapterResponseWithThumbnail;
use crate::dto::image::ImageResponse;
use crate::dto::movie::MovieCreationResponse;
use crate::dto::movie::{MovieResponseWithRelations, NewMovie};
use crate::error_handling::{ApiError, ApiRawResult, ApiResult};
use crate::{services, utils};
use entity::movie;
use rocket::fs::TempFile;
use rocket::response::status;
use rocket::serde::uuid::Uuid;
use rocket::State;
use rocket::{post, serde::json::Json};
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};
use sea_orm::{DbErr, TransactionTrait};

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
	openapi_get_routes_spec![settings: new_movie, get_movie, get_movie_chapters, post_movie_thumbnail]
}

/// Create a new movie with its chapters
#[openapi(tag = "Movies")]
#[post("/", format = "json", data = "<data>")]
async fn new_movie(
	db: Database<'_>,
	data: Json<NewMovie>,
) -> ApiRawResult<status::Created<Json<MovieCreationResponse>>> {
	// TODO: This should be validated before the controller is called
	for chapter in data.0.chapters.iter() {
		if chapter.types.is_empty() {
			return Err(ApiError::ValidationError(format!(
				"Chapter '{}' should have at least one type",
				chapter.name
			)));
		}
	}
	db.into_inner()
		.transaction::<_, MovieCreationResponse, DbErr>(|txn| {
			Box::pin(async move {
				let file = services::file::create_or_find(
					&data.file.path,
					data.file.size,
					data.file.quality,
					txn,
				)
				.await?;
				let mut package_artist: Option<ArtistResponse> = None;

				if let Some(pa_name) = &data.package_artist_name {
					package_artist = Some(services::artist::create_or_find(&pa_name, txn).await?);
				}

				let movie_artist = services::artist::create_or_find(&data.artist_name, txn).await?;
				let package = services::package::create_or_find(
					&package_artist,
					&data.package_name,
					data.package_release_date,
					txn,
				)
				.await?;

				let movie = services::movie::create(
					&data.movie_name,
					data.movie_type,
					&movie_artist,
					&package.id,
					&file.id,
					txn,
				)
				.await?;

				let chapter_results =
					services::chapter::create_many(&data.chapters, &movie.id, txn).await?;

				Ok(MovieCreationResponse {
					artist_id: movie_artist.id,
					package_artist_id: package_artist.map(|a| a.id),
					package_id: package.id,
					movie_id: movie.id,
					chapters_id: chapter_results.iter().map(|c| c.id).collect(),
					file_id: file.id,
				})
			})
		})
		.await
		.map_or_else(
			|e| Err(ApiError::from(e)),
			|v| Ok(status::Created::new("").body(Json(v))),
		)
}

/// Get a Single Movie
#[openapi(tag = "Movies")]
#[get("/<slug_or_uuid>")]
async fn get_movie(
	db: Database<'_>,
	slug_or_uuid: String,
) -> ApiResult<MovieResponseWithRelations> {
	services::movie::find(&slug_or_uuid, db.into_inner())
		.await
		.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(Json(v)))
}

/// Get a Movie's Chapters
#[openapi(tag = "Movies")]
#[get("/<uuid>/chapters")]
async fn get_movie_chapters(
	db: Database<'_>,
	uuid: Uuid,
) -> ApiResult<Vec<ChapterResponseWithThumbnail>> {
	services::chapter::find_by_movie(&uuid, db.into_inner())
		.await
		.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(Json(v)))
}

/// Upload a Movie's Thumbnail
#[openapi(tag = "Movies")]
#[post("/<slug_or_uuid>/thumbnail", data = "<data>")]
async fn post_movie_thumbnail(
	db: Database<'_>,
	slug_or_uuid: String,
	data: TempFile<'_>,
	config: &State<Config>,
) -> ApiRawResult<status::Created<Json<ImageResponse>>> {
	let bytes = utils::temp_file_to_bytes_vec(data).await?;
	let conn = db.into_inner();
	let movie = services::movie::find(&slug_or_uuid, conn)
		.await
		.map_err(|e| ApiError::from(e))?;
	let new_poster = services::image::save_image(
		&bytes,
		crate::dto::image::ImageType::Poster,
		movie.poster.map(|p| p.id),
		&movie.movie.id,
		movie::Entity,
		movie::Column::Id,
		movie::Column::PosterId,
		conn,
		config,
	)
	.await?;

	Ok(status::Created::new("").body(Json(ImageResponse::from(new_poster))))
}
