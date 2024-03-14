use crate::swagger_examples::*;
use chrono::NaiveDateTime;
use rocket::serde;
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use super::artist::ArtistResponse;
use super::chapter::NewChapter;
use super::file::{FileResponse, NewFile};
use super::image::ImageResponse;
use super::package::PackageResponse;

#[derive(Serialize, JsonSchema)]
pub struct MovieResponseWithRelations {
	#[serde(flatten)]
	pub movie: MovieResponse,
	pub poster: Option<ImageResponse>,
	#[schemars(skip)]
	#[serde(skip_serializing_if = "Option::is_none")]
	pub package: Option<PackageResponse>,
	#[schemars(skip)]
	#[serde(skip_serializing_if = "Option::is_none")]
	pub artist: Option<ArtistResponse>,
	#[schemars(skip)]
	#[serde(skip_serializing_if = "Option::is_none")]
	pub file: Option<FileResponse>,
}

#[derive(Serialize, JsonSchema)]
pub struct MovieResponse {
	#[schemars(example = "example_uuid")]
	pub id: Uuid,
	pub name: String,
	pub slug: String,
	#[schemars(example = "example_uuid")]
	pub poster_id: Option<Uuid>,
	pub registered_at: NaiveDateTime,
	#[schemars(example = "example_uuid")]
	pub package_id: Uuid,
	#[schemars(example = "example_uuid")]
	pub artist_id: Uuid,
	#[schemars(example = "example_uuid")]
	pub file_id: Uuid,
	#[schemars(example = "example_movie_type")]
	#[serde(rename = "type")]
	pub type_: MovieType,
}

/// DTO to create a new Movie
#[derive(Deserialize, JsonSchema, Serialize)]
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

#[derive(Serialize, Deserialize, JsonSchema, Debug)]
#[serde(crate = "rocket::serde")]
pub struct MovieCreationResponse {
	#[schemars(example = "example_uuid")]
	pub artist_id: Uuid,
	#[schemars(example = "example_uuid")]
	pub package_artist_id: Option<Uuid>,
	#[schemars(example = "example_uuid")]
	pub package_id: Uuid,
	#[schemars(example = "example_uuid")]
	pub movie_id: Uuid,
	#[schemars(example = "example_uuid")]
	pub file_id: Uuid,
	pub chapters_id: Vec<Uuid>,
}

#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
enum MovieType {
	Concert,
	Documentary,
}
