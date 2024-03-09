use crate::swagger_examples::*;
use domain::models::{file::File, image::Image, movie::Movie, package::Package};
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use super::artist::ArtistResponse;

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

#[derive(Serialize, JsonSchema)]
pub struct MovieResponse {
	#[serde(flatten)]
	pub movie: Movie,
	pub poster: Option<Image>,
	#[schemars(skip)]
	#[serde(skip_serializing_if = "Option::is_none")]
	pub package: Option<Package>,
	#[schemars(skip)]
	#[serde(skip_serializing_if = "Option::is_none")]
	pub artist: Option<ArtistResponse>,
	#[schemars(skip)]
	#[serde(skip_serializing_if = "Option::is_none")]
	pub file: Option<File>,
}
