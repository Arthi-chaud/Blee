use crate::swagger_examples::*;
use chrono::{NaiveDate, NaiveDateTime};
use entity::{artist, image, package};
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::Serialize;

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
	#[schemars(example = "example_uuid")]
	pub banner_id: Option<Uuid>,
}
