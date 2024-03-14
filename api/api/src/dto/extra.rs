use super::artist::ArtistResponse;
use super::file::NewFile;
use crate::dto::image::ImageResponse;
use crate::swagger_examples::*;
use chrono::NaiveDate;
use entity::{extra, file, image, package};
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(Serialize, JsonSchema)]
pub struct ExtraResponseWithRelations {
	#[serde(flatten)]
	pub extra: ExtraResponse,
	pub thumbnail: Option<ImageResponse>,
	#[schemars(skip)]
	#[serde(skip_serializing_if = "Option::is_none")]
	pub package: Option<package::Model>,
	#[schemars(skip)]
	#[serde(skip_serializing_if = "Option::is_none")]
	pub artist: Option<ArtistResponse>,
	#[schemars(skip)]
	#[serde(skip_serializing_if = "Option::is_none")]
	pub file: Option<file::Model>,
}

#[derive(Serialize, JsonSchema)]
struct ExtraResponse {
	#[schemars(example = "example_uuid")]
	pub id: Uuid,
	pub name: String,
	pub slug: String,
	#[schemars(example = "example_uuid")]
	pub thumbnail_id: Option<Uuid>,
	pub registered_at: Option<NaiveDate>,
	#[schemars(example = "example_uuid")]
	pub package_id: Uuid,
	#[schemars(example = "example_uuid")]
	pub artist_id: Uuid,
	#[schemars(example = "example_uuid")]
	pub file_id: Uuid,
	#[schemars(example = "example_index")]
	pub disc_index: Option<i32>,
	#[schemars(example = "example_index")]
	pub track_index: Option<i32>,
	pub r#type: ExtraType,
}

#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
enum ExtraType {
	AlternateView,
	Backdrops,
	BehindTheScenes,
	Interview,
	MusicVideo,
	Other,
	Performance,
	Trailer,
}

/// Object returned when an extra has been created
#[derive(Serialize, Deserialize, JsonSchema, Debug)]
#[serde(crate = "rocket::serde")]
pub struct ExtraCreationResponse {
	#[schemars(example = "example_uuid")]
	pub artist_id: Uuid,
	#[schemars(example = "example_uuid")]
	pub package_artist_id: Option<Uuid>,
	#[schemars(example = "example_uuid")]
	pub package_id: Uuid,
	#[schemars(example = "example_uuid")]
	pub extra_id: Uuid,
	#[schemars(example = "example_uuid")]
	pub file_id: Uuid,
}

/// DTO to create a new Extra
#[derive(Deserialize, Serialize, JsonSchema)]
#[serde(crate = "rocket::serde")]
pub struct NewExtra {
	#[schemars(example = "example_artist_name")]
	pub artist_name: String,
	#[schemars(example = "example_extra_name")]
	pub extra_name: String,
	#[schemars(example = "example_artist_name")]
	pub package_artist_name: Option<String>,
	#[schemars(example = "example_package_name")]
	pub package_name: String,
	#[schemars(example = "example_package_release_date")]
	pub package_release_date: Option<chrono::NaiveDate>,
	#[schemars(example = "example_index")]
	pub disc_index: Option<i16>,
	#[schemars(example = "example_index")]
	pub track_index: Option<i16>,
	pub types: Vec<ExtraType>,
	pub file: NewFile,
}
