use diesel::prelude::*;
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::Serialize;

#[derive(Queryable, Identifiable, Selectable, PartialEq, Serialize, JsonSchema, Clone)]
#[diesel(table_name = crate::schema::images)]
#[diesel(check_for_backend(diesel::pg::Pg))]
/// An Image
pub struct Image {
	pub id: Uuid,
	pub blurhash: String,
	pub colors: Vec<Option<String>>,
	pub aspect_ratio: f64,
	#[serde(rename = "type")]
	pub type_: ImageType,
}

#[derive(diesel_derive_enum::DbEnum, Debug, PartialEq, Serialize, JsonSchema, Clone)]
#[DbValueStyle = "PascalCase"]
#[ExistingTypePath = "crate::schema::sql_types::ImageTypes"]
pub enum ImageType {
	Poster,
	Banner,
	Thumbnail,
}
