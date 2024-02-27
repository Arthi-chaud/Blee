use crate::dto::extra::NewExtra;
use crate::error_handling::{ApiError, ApiResult};
use diesel::Connection;
use domain::models::artist::Artist;
use infrastructure::Database;
use rocket::{post, serde::json::Json};
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};
use shared::response_models::ExtraCreationResponse;

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
	openapi_get_routes_spec![settings: new_extra]
}

/// Create a new extra
#[openapi(tag = "Extras")]
#[post("/", format = "json", data = "<data>")]
async fn new_extra(_db: Database, data: Json<NewExtra>) -> ApiResult<ExtraCreationResponse> {
	_db.run(move |conn| {
		conn.transaction::<ExtraCreationResponse, diesel::result::Error, _>(
			move |connection| -> _ {
				let file = application::services::file::create_or_find(
					&data.file.path,
					data.file.size,
					data.file.quality,
					connection,
				)?;
				let package_artist = data.package_artist_name.clone().map_or(
					Ok::<Option<Artist>, diesel::result::Error>(None),
					|artist_name| {
						Ok(Some(application::services::artist::create_or_find(
							&artist_name,
							connection,
						)?))
					},
				)?;
				let extra_artist =
					application::services::artist::create_or_find(&data.artist_name, connection)?;
				let package = application::services::package::create_or_find(
					package_artist.clone(),
					&data.package_name,
					data.package_release_date,
					connection,
				)?;

				let extra = application::services::extra::create(
					&data.extra_name,
					data.disc_index,
					data.track_index,
					&data.type_,
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
				})
			},
		)
	})
	.await
	.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(Json(v)))
}
