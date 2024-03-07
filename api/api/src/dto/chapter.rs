use crate::swagger_examples::*;
use domain::models::chapter::ChapterType;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// DTO to create a new chapter
#[derive(Deserialize, JsonSchema, Serialize)]
#[serde(crate = "rocket::serde")]
pub struct NewChapter {
	#[schemars(example = "example_chapter_name")]
	pub name: String,
	/// the start timespamp of the chapter, in seconds
	pub start_timestamp: i16,
	/// the end timespamp of the chapter, in seconds
	pub end_timestamp: i16,
	/// the end timespamp of the chapter, in seconds
	pub types: Vec<ChapterType>,
}
