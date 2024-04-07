use crate::swagger_examples::*;
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// An External ID data type
#[derive(Serialize, JsonSchema)]
pub struct ExternalIdResponse {
	#[schemars(example = "example_uuid")]
	pub id: Uuid,
	pub url: String,
	pub value: String,
	#[schemars(example = "example_description")]
	pub description: Option<String>,
	#[schemars(example = "example_rating")]
	pub rating: Option<i16>,
	#[schemars(example = "example_provider_name")]
	pub provider_name: String,
	#[serde(skip_serializing_if = "Option::is_none")]
	pub artist_id: Option<Uuid>,
	#[serde(skip_serializing_if = "Option::is_none")]
	pub package_id: Option<Uuid>,
}

/// DTO to create a new ExternalId
#[derive(Deserialize, Serialize, JsonSchema)]
#[serde(crate = "rocket::serde")]
pub struct NewExternalId {
	pub url: String,
	pub value: String,
	#[schemars(example = "example_description")]
	pub description: Option<String>,
	#[schemars(example = "example_rating")]
	pub rating: Option<i16>,
	#[schemars(example = "example_provider_name")]
	pub provider_name: String,
	pub artist_id: Option<Uuid>,
	pub package_id: Option<Uuid>,
}
