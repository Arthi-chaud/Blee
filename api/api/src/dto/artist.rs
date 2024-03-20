use super::image::ImageResponse;
use crate::swagger_examples::*;
use crate::utils::Identifiable;
use chrono::NaiveDateTime;
use entity::{artist, image};
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::Serialize;

/// An ArtistResponse with the ImageResponse of the related poster, if there is
/// one
#[derive(Serialize, JsonSchema)]
pub struct ArtistWithPosterResponse {
	#[serde(flatten)]
	pub artist: ArtistResponse,
	pub poster: Option<ImageResponse>,
}

impl Identifiable for ArtistWithPosterResponse {
	fn get_id(&self) -> String {
		self.artist.get_id()
	}
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
	#[schemars(example = "example_description")]
	pub description: Option<String>,
	pub registered_at: NaiveDateTime,
	#[schemars(example = "example_uuid")]
	pub poster_id: Option<Uuid>,
}

impl Identifiable for ArtistResponse {
	fn get_id(&self) -> String {
		self.id.to_string()
	}
}

impl From<artist::Model> for ArtistResponse {
	fn from(value: artist::Model) -> Self {
		ArtistResponse {
			id: value.id,
			name: value.name,
			slug: value.slug,
			description: value.description,
			registered_at: value.registered_at.into(),
			poster_id: value.poster_id,
		}
	}
}
