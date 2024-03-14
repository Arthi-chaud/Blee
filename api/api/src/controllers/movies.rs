use crate::database::Database;
use crate::dto::movie::NewMovie;
use crate::error_handling::{ApiError, ApiRawResult, ApiResult};
use crate::dto::chapter::ChapterResponse;
use crate::dto::movie::{MovieCreationResponse, MovieResponse};
use crate::services;
use entity::artist;
use rocket::response::status;
use rocket::serde::uuid::Uuid;
use rocket::{post, serde::json::Json};
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};
use sea_orm::DbErr;

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
	openapi_get_routes_spec![settings: new_movie, get_movie, get_movie_chapters]
}

/// Create a new movie with its chapters
#[openapi(tag = "Movies")]
#[post("/", format = "json", data = "<data>")]
async fn new_movie(
	db: Database<'_>,
	data: Json<NewMovie>,
) -> ApiRawResult<status::Created<Json<MovieCreationResponse>>> {
	db.transaction::<_, (), DbErr>(|txn| {
		Box::pin(async move {
			let file = services::file::create_or_find(
				&data.file.path,
				data.file.size,
				data.file.quality,
				txn,
			)
			.await?;
			let package_artist = data.package_artist_name.clone().map_or(
				Ok::<Option<artist::Model>, DbErr>(None),
				|artist_name| {
					Ok(Some(services::artist::create_or_find(
						&artist_name,
						txn,
					)?))
				},
			)?;
			let movie_artist =
				services::artist::create_or_find(&data.artist_name, txn).await?;
			let package = services::package::create_or_find(
				package_artist.clone(),
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
async fn get_movie(db: Database<'_>, slug_or_uuid: String) -> ApiResult<MovieResponse> {
	services::movie::find(&slug_or_uuid, db.into_inner())
		.await
		.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(Json(v)))
}

/// Get a Movie's Chapters
#[openapi(tag = "Movies")]
#[get("/<uuid>/chapters")]
async fn get_movie_chapters(db: Database<'_>, uuid: Uuid) -> ApiResult<Vec<ChapterResponse>> {
	services::chapter::find_by_movie(&uuid, db.into_inner())
		.await
		.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(Json(v)))
}
