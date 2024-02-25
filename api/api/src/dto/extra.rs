use domain::models::extra::ExtraType;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::Deserialize;
use shared::examples::*;

/// DTO to create a new Extra
#[derive(Deserialize, JsonSchema)]
#[serde(crate = "rocket::serde")]
pub struct NewExtra {
	#[schemars(example = "example_artist_name")]
    pub artist_name: String,
	#[schemars(example = "example_extra_name")]
    pub extra_name: String,
	#[schemars(example = "example_artist_name")]
	pub package_artist_name: String,
	#[schemars(example = "example_package_name")]
    pub package_name: String,
	#[schemars(example = "example_file_path")]
	pub file_path: String,
	#[schemars(example = "example_index")]
	pub disc_index: Option<i16>,
	#[schemars(example = "example_index")]
	pub track_index: Option<i16>,
	pub type_: Vec<ExtraType>,
}
