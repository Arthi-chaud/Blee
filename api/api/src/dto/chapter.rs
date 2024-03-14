use crate::swagger_examples::*;
use entity::sea_orm_active_enums::ChapterTypeEnum;
use entity::{chapter, image};
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use super::image::ImageResponse;

/// A ChapterResponse with the ImageResponse of the thumbnail, of there is one
#[derive(Serialize, JsonSchema)]
pub struct ChapterResponseWithThumbnail {
	#[serde(flatten)]
	pub chapter: ChapterResponse,
	pub thumbnail: Option<ImageResponse>,
}

/// A Chapter data type
#[derive(Serialize, JsonSchema)]
pub struct ChapterResponse {
	#[schemars(example = "example_uuid")]
	pub id: Uuid,
	pub name: String,
	#[schemars(example = "example_uuid")]
	pub thumbnail_id: Option<Uuid>,
	#[schemars(example = "example_uuid")]
	pub movie_id: Uuid,
	/// the start timespamp of the chapter, in seconds
	pub start_time: i32,
	/// the end timespamp of the chapter, in seconds
	pub end_time: i32,
	pub r#type: ChapterType,
}

impl From<chapter::Model> for ChapterResponse {
	fn from(value: chapter::Model) -> Self {
		ChapterResponse {
			id: value.id,
			name: value.name,
			thumbnail_id: value.thumbnail_id,
			movie_id: value.movie_id,
			start_time: value.start_time,
			end_time: value.end_time,
			r#type: value.r#type.into(),
		}
	}
}

#[derive(Serialize, JsonSchema, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ChapterType {
	Interview,
	NonMusicalInterview,
	Other,
	Performance,
}

impl From<ChapterTypeEnum> for ChapterType {
	fn from(value: ChapterTypeEnum) -> Self {
		match value {
			ChapterTypeEnum::Interview => ChapterType::Interview,
			ChapterTypeEnum::NonMusicalInterview => ChapterType::NonMusicalInterview,
			ChapterTypeEnum::Performance => ChapterType::Performance,
			ChapterTypeEnum::Other => ChapterType::Other,
		}
	}
}

/// DTO to create a new chapter
#[derive(Deserialize, JsonSchema, Serialize)]
#[serde(crate = "rocket::serde")]
pub struct NewChapter {
	#[schemars(example = "example_chapter_name")]
	pub name: String,
	/// the start timespamp of the chapter, in seconds
	pub start_timestamp: u64,
	/// the end timespamp of the chapter, in seconds
	pub end_timestamp: u64,
	pub types: Vec<ChapterType>,
}
