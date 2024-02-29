use crate::swagger_examples::*;
use domain::models::movie::MovieType;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::Deserialize;

use super::chapter::NewChapter;
use super::file::NewFile;

/// DTO to create a new Movie
#[derive(Deserialize, JsonSchema)]
#[serde(crate = "rocket::serde")]
pub struct NewMovie {
	#[schemars(example = "example_artist_name")]
	pub artist_name: String,
	#[schemars(example = "example_extra_name")]
	pub movie_name: String,
	#[schemars(example = "example_movie_type")]
	pub movie_type: MovieType,
	#[schemars(example = "example_artist_name")]
	pub package_artist_name: Option<String>,
	#[schemars(example = "example_package_name")]
	pub package_name: String,
	#[schemars(example = "example_package_release_date")]
	pub package_release_date: Option<chrono::NaiveDate>,
	pub chapters: Vec<NewChapter>,
	pub file: NewFile,
}
