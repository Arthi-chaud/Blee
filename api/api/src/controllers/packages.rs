use crate::config::Config;
use crate::database::Database;
use crate::dto::image::ImageResponse;
use crate::dto::package::{PackageFilter, PackageResponseWithRelations};
use crate::dto::page::{Page, Pagination};
use crate::error_handling::{ApiError, ApiPageResult, ApiRawResult, ApiResult};
use crate::{services, utils};
use entity::package;
use rocket::fs::TempFile;
use rocket::response::status;
use rocket::serde::json::Json;
use rocket::State;
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
	openapi_get_routes_spec![settings: get_package, get_packages, post_package_poster]
}

/// Get a Single Package
#[openapi(tag = "Packages")]
#[get("/<slug_or_uuid>")]
async fn get_package(
	db: Database<'_>,
	slug_or_uuid: String,
) -> ApiResult<PackageResponseWithRelations> {
	services::package::find(&slug_or_uuid, db.into_inner())
		.await
		.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(Json(v)))
}

/// Get many packages
#[openapi(tag = "Packages")]
#[get("/?<filters>&<pagination>")]
async fn get_packages(
	db: Database<'_>,
	filters: PackageFilter,
	pagination: Pagination,
) -> ApiPageResult<PackageResponseWithRelations> {
	services::package::find_many(filters, pagination, db.into_inner())
		.await
		.map(|items| Page::from(items))
		.map_or_else(|e| Err(ApiError::from(e)), |v| Ok(v))
}

/// Upload a Package's Poster
#[openapi(tag = "Packages")]
#[post("/<slug_or_uuid>/poster", data = "<data>")]
async fn post_package_poster(
	db: Database<'_>,
	slug_or_uuid: String,
	data: TempFile<'_>,
	config: &State<Config>,
) -> ApiRawResult<status::Created<Json<ImageResponse>>> {
	let bytes = utils::temp_file_to_bytes_vec(data).await?;
	let conn = db.into_inner();
	let package = services::package::find(&slug_or_uuid, conn)
		.await
		.map_err(|e| ApiError::from(e))?;
	let new_poster = services::image::save_image(
		&bytes,
		crate::dto::image::ImageType::Poster,
		package.poster.map(|p| p.id),
		&package.package.id,
		package::Entity,
		package::Column::Id,
		package::Column::PosterId,
		conn,
		config,
	)
	.await?;

	Ok(status::Created::new("").body(Json(ImageResponse::from(new_poster))))
}
