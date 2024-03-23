use crate::{swagger_examples::*, utils::Identifiable};
use entity::{file, sea_orm_active_enums::VideoQualityEnum};
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// DTO to create a new File
#[derive(Deserialize, Serialize, JsonSchema, Clone)]
#[serde(crate = "rocket::serde")]
pub struct NewFile {
	/// The aboslute path of the file
	#[schemars(example = "example_file_path")]
	pub path: String,
	/// The size (in bytes) of the file
	#[schemars(example = "example_size")]
	pub size: u64,
	#[schemars(example = "example_video_quality")]
	pub quality: VideoQuality,
}

/// A 'File' data type
#[derive(Serialize, JsonSchema)]
pub struct FileResponse {
	#[schemars(example = "example_uuid")]
	pub id: Uuid,
	#[schemars(example = "example_file_path")]
	pub path: String,
	#[schemars(example = "example_size")]
	pub size: i64,
	pub quality: VideoQuality,
	#[schemars(example = "example_uuid")]
	pub scrubber_id: Option<Uuid>,
}

impl Identifiable for FileResponse {
	fn get_id(&self) -> String {
		self.id.to_string()
	}
}

impl From<file::Model> for FileResponse {
	fn from(value: file::Model) -> Self {
		FileResponse {
			id: value.id,
			path: value.path,
			size: value.size,
			quality: VideoQuality::from(value.quality),
			scrubber_id: value.scrubber_id,
		}
	}
}

#[derive(Deserialize, Serialize, JsonSchema, Clone, Copy)]
pub enum VideoQuality {
	#[serde(rename(deserialize = "8k", serialize = "8k"))]
	K8,
	#[serde(rename(deserialize = "4k", serialize = "4k"))]
	K4,
	#[serde(rename(deserialize = "2k", serialize = "2k"))]
	K2,
	#[serde(rename(deserialize = "1080p", serialize = "1080p"))]
	P1080,
	#[serde(rename(deserialize = "720p", serialize = "720p"))]
	P720,
	#[serde(rename(deserialize = "480p", serialize = "480p"))]
	P480,
	#[serde(rename(deserialize = "360p", serialize = "360p"))]
	P360,
	#[serde(rename(deserialize = "240p", serialize = "240p"))]
	P240,
	#[serde(rename(deserialize = "other", serialize = "other"))]
	Other,
}

impl From<VideoQualityEnum> for VideoQuality {
	fn from(value: VideoQualityEnum) -> Self {
		match value {
			VideoQualityEnum::_1080p => VideoQuality::P1080,
			VideoQualityEnum::_240p => VideoQuality::P240,
			VideoQualityEnum::_2k => VideoQuality::K2,
			VideoQualityEnum::_360p => VideoQuality::P360,
			VideoQualityEnum::_480p => VideoQuality::P480,
			VideoQualityEnum::_4k => VideoQuality::K4,
			VideoQualityEnum::_720p => VideoQuality::P720,
			VideoQualityEnum::_8k => VideoQuality::K8,
			VideoQualityEnum::Other => VideoQuality::Other,
		}
	}
}

impl From<VideoQuality> for VideoQualityEnum {
	fn from(value: VideoQuality) -> Self {
		match value {
			VideoQuality::P1080 => VideoQualityEnum::_1080p,
			VideoQuality::P240 => VideoQualityEnum::_240p,
			VideoQuality::K2 => VideoQualityEnum::_2k,
			VideoQuality::P360 => VideoQualityEnum::_360p,
			VideoQuality::P480 => VideoQualityEnum::_480p,
			VideoQuality::K4 => VideoQualityEnum::_4k,
			VideoQuality::P720 => VideoQualityEnum::_720p,
			VideoQuality::K8 => VideoQualityEnum::_8k,
			VideoQuality::Other => VideoQualityEnum::Other,
		}
	}
}

/// Filters for Files
#[derive(Serialize, Deserialize, JsonSchema, FromForm)]
pub struct FileFilter {
	/// Filter by Path (starts with)
	pub path: Option<String>,
}
