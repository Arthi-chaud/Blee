use crate::dto::extra::NewExtra;
use crate::error_handling::{ApiError, ApiRawResult, ApiResult};
use crate::responses::extra::{ExtraCreationResponse, ExtraResponse};
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
	openapi_get_routes_spec![settings: new_extra, get_extra]
}

/// Create a new extra
#[openapi(tag = "Extras")]
#[post("/", format = "json", data = "<data>")]
async fn new_extra(
	_db: Database,
	data: Json<NewExtra>,
) -> ApiRawResult<status::Created<Json<ExtraCreationResponse>>> {
	_db.run(move |conn| {
		conn.transaction::<ExtraCreationResponse, diesel::result::Error, _>(
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
				let extra_artist = services::artist::create_or_find(&data.artist_name, connection)?;
				let package = services::package::create_or_find(
					package_artist.clone(),
					&data.package_name,
					data.package_release_date,
					connection,
				)?;

				let extra = services::extra::create(
					&data.extra_name,
					data.disc_index,
					data.track_index,
					&data.types,
					&package.id,
					&extra_artist.id,
					&file.id,
					connection,
				)?;

				Ok(ExtraCreationResponse {
					artist_id: extra_artist.id,
					package_artist_id: package_artist.map(|a| a.id),
					package_id: package.id,
					extra_id: extra.id,
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

/// Get a Single extra
#[openapi(tag = "Extras")]
#[get("/<uuid>")]
async fn get_extra(db: Database, uuid: Uuid) -> ApiResult<ExtraResponse> {
	db.run(move |conn| services::extra::find(&uuid, conn))
		.await
		.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(Json(v)))
}
