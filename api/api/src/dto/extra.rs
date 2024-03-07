use crate::swagger_examples::*;
use domain::models::extra::ExtraType;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use super::file::NewFile;

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
