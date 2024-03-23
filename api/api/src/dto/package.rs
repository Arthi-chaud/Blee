use crate::swagger_examples::*;
use crate::utils::Identifiable;
use chrono::{NaiveDate, NaiveDateTime};
use entity::package;
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use super::artist::ArtistResponse;
use super::image::ImageResponse;

/// A PackageResponse with the related entitied
#[derive(Serialize, JsonSchema)]
pub struct PackageResponseWithRelations {
	#[serde(flatten)]
	pub package: PackageResponse,
	#[schemars(skip)]
	#[serde(skip_serializing_if = "Option::is_none")]
	pub artist: Option<Option<ArtistResponse>>,
	pub poster: Option<ImageResponse>,
}

impl Identifiable for PackageResponseWithRelations {
	fn get_id(&self) -> String {
		self.package.get_id()
	}
}

/// A Package Data type
#[derive(Serialize, JsonSchema)]
pub struct PackageResponse {
	#[schemars(example = "example_uuid")]
	pub id: Uuid,
	#[schemars(example = "example_package_name")]
	pub name: String,
	pub slug: String,
	#[schemars(example = "example_description")]
	pub description: Option<String>,
	#[schemars(example = "example_package_release_date")]
	pub release_year: Option<NaiveDate>,
	pub registered_at: NaiveDateTime,
	#[schemars(example = "example_uuid")]
	pub artist_id: Option<Uuid>,
	#[schemars(example = "example_uuid")]
	pub poster_id: Option<Uuid>,
}

impl Identifiable for PackageResponse {
	fn get_id(&self) -> String {
		self.id.to_string()
	}
}

impl From<package::Model> for PackageResponse {
	fn from(value: package::Model) -> Self {
		PackageResponse {
			id: value.id,
			name: value.name,
			slug: value.slug,
			description: value.description,
			release_year: value.release_year,
			registered_at: value.registered_at.into(),
			artist_id: value.artist_id,
			poster_id: value.poster_id,
		}
	}
}

/// Filters for packages
#[derive(Serialize, Deserialize, JsonSchema, FromForm)]
pub struct PackageFilter {
	/// Filter by Artist
	pub artist: Option<Uuid>,
}
