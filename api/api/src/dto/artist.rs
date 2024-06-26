use super::image::ImageResponse;
use crate::swagger_examples::*;
use chrono::NaiveDateTime;
use entity::{artist, image};
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// An ArtistResponse with the ImageResponse of the related poster, if there is
/// one
#[derive(Serialize, JsonSchema)]
pub struct ArtistWithPosterResponse {
	#[serde(flatten)]
	pub artist: ArtistResponse,
	pub poster: Option<ImageResponse>,
}

impl From<(artist::Model, Option<image::Model>)> for ArtistWithPosterResponse {
	fn from(value: (artist::Model, Option<image::Model>)) -> Self {
		ArtistWithPosterResponse {
			artist: value.0.into(),
			poster: value.1.map(|i| i.into()),
		}
	}
}

/// An Artist data type
#[derive(Serialize, JsonSchema)]
pub struct ArtistResponse {
	#[schemars(example = "example_uuid")]
	pub id: Uuid,
	#[schemars(example = "example_artist_name")]
	pub name: String,
	#[schemars(example = "example_artist_slug")]
	pub slug: String,
	pub registered_at: NaiveDateTime,
	#[schemars(example = "example_uuid")]
	pub poster_id: Option<Uuid>,
}

impl From<artist::Model> for ArtistResponse {
	fn from(value: artist::Model) -> Self {
		ArtistResponse {
			id: value.id,
			name: value.name,
			slug: value.unique_slug,
			registered_at: value.registered_at.into(),
			poster_id: value.poster_id,
		}
	}
}

// Filter for Artist
pub struct ArtistFilter {
	pub package: Option<Uuid>,
}

// Sorting for Artist
#[derive(Deserialize, FromFormField, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ArtistSort {
	#[field(value = "name")]
	Name,
	#[field(value = "add_date")]
	AddDate,
}
