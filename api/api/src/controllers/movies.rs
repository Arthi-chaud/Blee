use crate::dto::movie::NewMovie;
use crate::error_handling::{ApiError, ApiRawResult, ApiResult};
use crate::responses::chapter::ChapterResponse;
use crate::responses::movie::{MovieCreationResponse, MovieResponse};
use crate::services;
use diesel::Connection;
use domain::models::artist::Artist;
use infrastructure::Database;
use rocket::response::status;
use rocket::serde::uuid::Uuid;
use rocket::{post, serde::json::Json};
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
	openapi_get_routes_spec![settings: new_movie, get_movie, get_movie_chapters]
}

/// Create a new movie with its chapters
#[openapi(tag = "Movies")]
#[post("/", format = "json", data = "<data>")]
async fn new_movie(
	_db: Database,
	data: Json<NewMovie>,
) -> ApiRawResult<status::Created<Json<MovieCreationResponse>>> {
	_db.run(move |conn| {
		conn.transaction::<MovieCreationResponse, diesel::result::Error, _>(
			move |connection| -> _ {
				let file = services::file::create_or_find(
					&data.file.path,
					data.file.size,
					data.file.quality,
					connection,
				)?;
				let package_artist = data.package_artist_name.clone().map_or(
					Ok::<Option<Artist>, diesel::result::Error>(None),
					|artist_name| {
						Ok(Some(services::artist::create_or_find(
							&artist_name,
							connection,
						)?))
					},
				)?;
				let movie_artist = services::artist::create_or_find(&data.artist_name, connection)?;
				let package = services::package::create_or_find(
					package_artist.clone(),
					&data.package_name,
					data.package_release_date,
					connection,
				)?;

				let movie = services::movie::create(
					&data.movie_name,
					data.movie_type,
					&movie_artist,
					&package.id,
					&file.id,
					connection,
				)?;

				let chapter_results =
					services::chapter::create_many(&data.chapters, &movie.id, connection)?;

				Ok(MovieCreationResponse {
					artist_id: movie_artist.id,
					package_artist_id: package_artist.map(|a| a.id),
					package_id: package.id,
					movie_id: movie.id,
					chapters_id: chapter_results.iter().map(|c| c.id).collect(),
					file_id: file.id,
				})
			},
		)
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
async fn get_movie(db: Database, slug_or_uuid: String) -> ApiResult<MovieResponse> {
	db.run(move |conn| services::movie::find(&slug_or_uuid, conn))
		.await
		.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(Json(v)))
}

/// Get a Movie's Chapters
#[openapi(tag = "Movies")]
#[get("/<uuid>/chapters")]
async fn get_movie_chapters(db: Database, uuid: Uuid) -> ApiResult<Vec<ChapterResponse>> {
	db.run(move |conn| services::chapters::find_by_movie(&uuid, conn))
		.await
		.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(Json(v)))
}
