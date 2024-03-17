use crate::config::Config;
use crate::database::Database;
use crate::dto::image::ImageResponse;
use crate::error_handling::{ApiError, ApiRawResult};
use crate::{services, utils};
use entity::chapter;
use rocket::fs::TempFile;
use rocket::response::status;
use rocket::serde::json::Json;
use rocket::serde::uuid::Uuid;
use rocket::State;
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
	openapi_get_routes_spec![settings: post_chapter_thumbnail]
}

/// Upload a Chapter's thumbnail
#[openapi(tag = "Chapters")]
#[post("/<uuid>/thumbnail", data = "<data>")]
async fn post_chapter_thumbnail(
	db: Database<'_>,
	uuid: Uuid,
	data: TempFile<'_>,
	config: &State<Config>,
) -> ApiRawResult<status::Created<Json<ImageResponse>>> {
	let bytes = utils::temp_file_to_bytes_vec(data).await?;
	let conn = db.into_inner();

	let chapter = services::chapter::find(&uuid, conn)
		.await
		.map_err(|e| ApiError::from(e))?;
	let new_poster = services::image::save_image(
		&bytes,
		crate::dto::image::ImageType::Thumbnail,
		chapter.thumbnail.map(|p| p.id),
		&chapter.chapter.id,
		chapter::Entity,
		chapter::Column::Id,
		chapter::Column::ThumbnailId,
		conn,
		config,
	)
	.await?;

	Ok(status::Created::new("").body(Json(ImageResponse::from(new_poster))))
}
