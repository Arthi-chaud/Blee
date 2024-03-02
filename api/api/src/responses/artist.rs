use domain::models::{artist::Artist, image::Image};
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::Serialize;

#[derive(Serialize, JsonSchema)]
pub struct ArtistResponse {
	#[serde(flatten)]
	pub artist: Artist,
	pub poster: Option<Image>,
}
