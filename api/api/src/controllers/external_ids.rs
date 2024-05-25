use crate::database::Database;
use crate::dto::external_id::{ExternalIdFilter, ExternalIdResponse, NewExternalId};
use crate::dto::page::{Page, Pagination};
use crate::error_handling::{ApiError, ApiPageResult, ApiRawResult};
use crate::guards::MatcherAuthGuard;
use crate::services;
use rocket::response::status;
use rocket::serde::uuid::Uuid;
use rocket::{post, serde::json::Json};
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
	openapi_get_routes_spec![settings: new_external_id, get_external_ids]
}
/// Create a new extra
#[openapi(tag = "External Id")]
#[post("/", format = "json", data = "<data>")]
async fn new_external_id(
	db: Database<'_>,
	data: Json<NewExternalId>,
	_matcher: MatcherAuthGuard,
) -> ApiRawResult<status::Created<Json<ExternalIdResponse>>> {
	if data.package_id.is_some() && data.artist_id.is_some()
		|| (data.package_id.is_none() && data.artist_id.is_none())
	{
		return Err(ApiError::ValidationError(
			"There should be exactly one parent ID set.".to_owned(),
		));
	}
	if let Some(rating) = data.rating {
		if rating < 0 || rating > 100 {
			return Err(ApiError::ValidationError(
				"Rating should be between 0 and 100.".to_owned(),
			));
		}
	}
	services::external_ids::create(&data.0, db.into_inner())
		.await
		.map_or_else(
			|e| Err(ApiError::from(e)),
			|v| Ok(status::Created::new("").body(Json(v.into()))),
		)
}

/// Get many External IDs
#[openapi(tag = "External Id")]
#[get("/?<artist>&<package>&<pagination..>")]
async fn get_external_ids(
	db: Database<'_>,
	artist: Option<Uuid>,
	package: Option<Uuid>,
	pagination: Pagination,
) -> ApiPageResult<ExternalIdResponse> {
	if artist.is_some() && package.is_some() {
		return Err(ApiError::ValidationError(
			"There should be exactly one parent ID set.".to_owned(),
		));
	}
	services::external_ids::find_many(
		&ExternalIdFilter { artist, package },
		&pagination,
		db.into_inner(),
	)
	.await
	.map(|items| Page::from(items))
	.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(v))
}
