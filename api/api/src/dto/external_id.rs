use crate::swagger_examples::*;
use entity::external_id;
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::{Deserialize, Serialize};

/// An External ID data type
#[derive(Serialize, JsonSchema, FromForm)]
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

impl From<external_id::Model> for ExternalIdResponse {
	fn from(value: external_id::Model) -> Self {
		ExternalIdResponse {
			id: value.id,
			url: value.url,
			value: value.value,
			provider_name: value.provider_name,
			description: value.description,
			rating: value.rating,
			artist_id: value.artist_id,
			package_id: value.package_id,
		}
	}
}

/// DTO to create a new ExternalId
#[derive(Deserialize, Serialize, JsonSchema, Clone)]
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
