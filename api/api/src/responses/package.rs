use domain::models::{artist::Artist, image::Image, package::Package};
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::Serialize;

#[derive(Serialize, JsonSchema)]
pub struct PackageResponse {
	#[serde(flatten)]
	pub package: Package,
	pub poster: Option<Image>,
	#[schemars(skip)]
	#[serde(skip_serializing_if = "Option::is_none")]
	pub artist: Option<Option<Artist>>,
}
