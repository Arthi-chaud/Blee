use super::chapter::NewChapter;
use super::file::NewFile;
use super::image::ImageResponse;
use crate::swagger_examples::*;
use chrono::NaiveDateTime;
use entity::sea_orm_active_enums::MovieTypeEnum;
use rocket::serde;
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(Serialize, JsonSchema)]
pub struct MovieResponseWithRelations {
	#[serde(flatten)]
	pub movie: MovieResponse,
	pub thumbnail: Option<ImageResponse>,
}

#[derive(Serialize, JsonSchema)]
pub struct MovieResponse {
	#[schemars(example = "example_uuid")]
	pub id: Uuid,
	pub name: String,
	pub slug: String,
	#[schemars(example = "example_uuid")]
	pub thumbnail_id: Option<Uuid>,
	pub registered_at: NaiveDateTime,
	#[schemars(example = "example_uuid")]
	pub package_id: Uuid,
	pub package_name: String,
	#[schemars(example = "example_uuid")]
	pub artist_id: Uuid,
	pub artist_name: String,
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

#[derive(Serialize, Deserialize, JsonSchema, Clone, Copy, FromFormField)]
#[serde(rename_all = "snake_case")]
pub enum MovieType {
	Concert,
	Documentary,
}

impl From<MovieTypeEnum> for MovieType {
	fn from(value: MovieTypeEnum) -> Self {
		match value {
			MovieTypeEnum::Concert => MovieType::Concert,
			MovieTypeEnum::Documentary => MovieType::Documentary,
		}
	}
}

impl From<MovieType> for MovieTypeEnum {
	fn from(value: MovieType) -> Self {
		match value {
			MovieType::Concert => MovieTypeEnum::Concert,
			MovieType::Documentary => MovieTypeEnum::Documentary,
		}
	}
}

/// Filters for movies
pub struct MovieFilter {
	/// Filter by Type
	pub r#type: Option<MovieType>,
	/// Filter by Artist
	pub artist: Option<Uuid>,
	/// Filter by Package
	pub package: Option<Uuid>,
}

// Sorting for Movies
#[derive(Deserialize, FromFormField, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum MovieSort {
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
