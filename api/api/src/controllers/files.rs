use crate::config::Config;
use crate::database::Database;
use crate::dto::file::FileFilter;
use crate::dto::file::FileResponse;
use crate::dto::file::FileSort;
use crate::dto::page::Page;
use crate::dto::page::Pagination;
use crate::dto::sort::build_sort;
use crate::dto::sort::SortOrder;
use crate::error_handling::{ApiError, ApiPageResult, ApiResult};
use crate::guards::ScannerAuthGuard;
use crate::services;
use rocket::serde::json::Json;
use rocket::serde::uuid::Uuid;
use rocket::State;
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
	openapi_get_routes_spec![settings: get_file, delete_file, get_files]
}

/// Get a Single File
#[openapi(tag = "Files")]
#[get("/<uuid>")]
async fn get_file(db: Database<'_>, uuid: Uuid) -> ApiResult<FileResponse> {
	services::file::find(&uuid, db.into_inner())
		.await
		.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(Json(v)))
}

/// Get delete a File, along with the related resources if needed (images,
/// chapers, extra, package, ...)
#[openapi(tag = "Files")]
#[delete("/<uuid>")]
async fn delete_file(
	db: Database<'_>,
	uuid: Uuid,
	config: &State<Config>,
	_scanner: ScannerAuthGuard,
) -> Result<(), ApiError> {
	services::file::delete(&uuid, db.into_inner(), config)
		.await
		.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(v))
}

/// Get many Files
#[openapi(tag = "Files")]
#[get("/?<path>&<sort>&<order>&<pagination..>")]
async fn get_files(
	db: Database<'_>,
	sort: Option<FileSort>,
	order: Option<SortOrder>,
	path: Option<String>,
	pagination: Pagination,
) -> ApiPageResult<FileResponse> {
	services::file::find_many(
		&FileFilter { path },
		build_sort(sort, order),
		&pagination,
		db.into_inner(),
	)
	.await
	.map(|items| Page::from(items))
	.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(v))
}
