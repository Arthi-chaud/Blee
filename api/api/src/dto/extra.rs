use super::file::NewFile;
use crate::dto::image::ImageResponse;
use crate::swagger_examples::*;
use chrono::NaiveDateTime;
use entity::sea_orm_active_enums::ExtraTypeEnum;
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::{Deserialize, Serialize};
#[derive(Serialize, JsonSchema)]
pub struct ExtraResponseWithRelations {
	#[serde(flatten)]
	pub extra: ExtraResponse,
	pub thumbnail: Option<ImageResponse>,
}

#[derive(Serialize, JsonSchema)]
pub struct ExtraResponse {
	#[schemars(example = "example_uuid")]
	pub id: Uuid,
	pub name: String,
	#[schemars(example = "example_uuid")]
	pub thumbnail_id: Option<Uuid>,
	pub registered_at: NaiveDateTime,
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
	pub artist_name: Option<String>,
	pub package_name: String,
	pub duration: u64,
	pub r#type: Vec<ExtraType>,
}

#[derive(Serialize, Deserialize, JsonSchema, Clone, Copy, FromFormField)]
#[serde(rename_all = "snake_case")]
pub enum ExtraType {
	AlternateView,
	Backdrops,
	BehindTheScenes,
	Interview,
	MusicVideo,
	Other,
	Performance,
	Trailer,
}

impl From<ExtraTypeEnum> for ExtraType {
	fn from(value: ExtraTypeEnum) -> Self {
		match value {
			ExtraTypeEnum::AlternateView => ExtraType::AlternateView,
			ExtraTypeEnum::Backdrops => ExtraType::Backdrops,
			ExtraTypeEnum::BehindTheScenes => ExtraType::BehindTheScenes,
			ExtraTypeEnum::Interview => ExtraType::Interview,
			ExtraTypeEnum::MusicVideo => ExtraType::MusicVideo,
			ExtraTypeEnum::Other => ExtraType::Other,
			ExtraTypeEnum::Performance => ExtraType::Performance,
			ExtraTypeEnum::Trailer => ExtraType::Trailer,
		}
	}
}

impl From<ExtraType> for ExtraTypeEnum {
	fn from(value: ExtraType) -> Self {
		match value {
			ExtraType::AlternateView => ExtraTypeEnum::AlternateView,
			ExtraType::Backdrops => ExtraTypeEnum::Backdrops,
			ExtraType::BehindTheScenes => ExtraTypeEnum::BehindTheScenes,
			ExtraType::Interview => ExtraTypeEnum::Interview,
			ExtraType::MusicVideo => ExtraTypeEnum::MusicVideo,
			ExtraType::Other => ExtraTypeEnum::Other,
			ExtraType::Performance => ExtraTypeEnum::Performance,
			ExtraType::Trailer => ExtraTypeEnum::Trailer,
		}
	}
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
	pub disc_index: Option<i32>,
	#[schemars(example = "example_index")]
	pub track_index: Option<i32>,
	/// Must Not Be Empty
	pub types: Vec<ExtraType>,
	pub file: NewFile,
}

/// Filters for movies
pub struct ExtraFilter {
	/// Filter by Type
	pub r#type: Option<ExtraType>,
	/// Filter by Artist
	pub artist: Option<Uuid>,
	/// Filter by Package
	pub package: Option<Uuid>,
}

// Sorting for Extras
#[derive(Deserialize, FromFormField, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ExtraSort {
	#[field(value = "name")]
	Name,
	#[field(value = "artist_name")]
	ArtistName,
	#[field(value = "package_name")]
	PackageName,
	#[field(value = "add_date")]
	AddDate,
	#[field(value = "release_date")]
	ReleaseDate,
}
