use crate::database::Database;
use crate::dto::artist::ArtistResponse;
use crate::dto::extra::ExtraCreationResponse;
use crate::dto::extra::ExtraResponseWithRelations;
use crate::dto::extra::NewExtra;
use crate::error_handling::{ApiError, ApiRawResult, ApiResult};
use crate::services;
use rocket::response::status;
use rocket::serde::uuid::Uuid;
use rocket::{post, serde::json::Json};
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};
use sea_orm::DbErr;
use sea_orm::TransactionTrait;

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
	openapi_get_routes_spec![settings: new_extra, get_extra]
}

/// Create a new extra
#[openapi(tag = "Extras")]
#[post("/", format = "json", data = "<data>")]
async fn new_extra(
	db: Database<'_>,
	data: Json<NewExtra>,
) -> ApiRawResult<status::Created<Json<ExtraCreationResponse>>> {
	db.into_inner()
		.transaction::<_, ExtraCreationResponse, DbErr>(|txn| {
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
				let extra_artist = services::artist::create_or_find(&data.artist_name, txn).await?;
				let package = services::package::create_or_find(
					&package_artist,
					&data.package_name,
					data.package_release_date,
					txn,
				)
				.await?;

				let extra = services::extra::create(
					&data.extra_name,
					data.disc_index,
					data.track_index,
					&data.types,
					&package.id,
					&extra_artist.id,
					&file.id,
					txn,
				)
				.await?;

				Ok(ExtraCreationResponse {
					artist_id: extra_artist.id,
					package_artist_id: package_artist.map(|a| a.id),
					package_id: package.id,
					extra_id: extra.id,
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

/// Get a Single extra
#[openapi(tag = "Extras")]
#[get("/<uuid>")]
async fn get_extra(db: Database<'_>, uuid: Uuid) -> ApiResult<ExtraResponseWithRelations> {
	services::extra::find(&uuid, db.into_inner())
		.await
		.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(Json(v)))
}
