use crate::swagger_examples::*;
use chrono::{NaiveDate, NaiveDateTime};
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use super::image::ImageResponse;

/// A PackageResponse with the related entitied
#[derive(Serialize, JsonSchema)]
pub struct PackageResponseWithRelations {
	#[serde(flatten)]
	pub package: PackageResponse,
	#[schemars(skip)]
	pub poster: Option<ImageResponse>,
}

/// A Package Data type
#[derive(Serialize, JsonSchema)]
pub struct PackageResponse {
	#[schemars(example = "example_uuid")]
	pub id: Uuid,
	#[schemars(example = "example_package_name")]
	pub name: String,
	pub slug: String,
	#[schemars(example = "example_package_release_date")]
	pub release_year: Option<NaiveDate>,
	pub registered_at: NaiveDateTime,
	#[schemars(example = "example_uuid")]
	pub artist_id: Option<Uuid>,
	pub artist_name: Option<String>,
	#[schemars(example = "example_uuid")]
	pub poster_id: Option<Uuid>,
}

/// Filters for packages
pub struct PackageFilter {
	/// Filter by Artist
	pub artist: Option<Uuid>,
}

// Sorting for Packages
#[derive(Deserialize, FromFormField, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum PackageSort {
	#[field(value = "name")]
	Name,
	#[field(value = "add_date")]
	AddDate,
	#[field(value = "release_date")]
	ReleaseDate,
	#[field(value = "artist_name")]
	ArtistName,
}
