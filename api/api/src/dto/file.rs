use crate::swagger_examples::*;
use domain::models::video_quality::VideoQuality;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::Deserialize;

/// DTO to create a new File
#[derive(Deserialize, JsonSchema, Clone)]
#[serde(crate = "rocket::serde")]
pub struct NewFile {
	/// The aboslute path of the file
	#[schemars(example = "example_file_path")]
	pub path: String,
	/// The size (in bytes) of the file
	#[schemars(example = "example_size")]
	pub size: i64,
	#[schemars(example = "example_video_quality")]
	pub quality: VideoQuality,
}
