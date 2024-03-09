use crate::models::image::Image;
use crate::models::movie::Movie;
use diesel::prelude::*;
use rocket::serde::uuid::Uuid;
use rocket::serde::Deserialize;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::Serialize;

#[derive(
	QueryableByName,
	Queryable,
	Identifiable,
	Selectable,
	Debug,
	PartialEq,
	Associations,
	Insertable,
	Clone,
	JsonSchema,
	Serialize,
)]
#[diesel(table_name = crate::schema::chapters)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(belongs_to(Image, foreign_key = thumbnail_id))]
#[diesel(belongs_to(Movie))]
/// A Chapter from a movie
pub struct Chapter {
	pub id: Uuid,
	pub name: String,
	pub thumbnail_id: Option<Uuid>,
	pub movie_id: Uuid,
	pub start_time: i16,
	pub end_time: i16,
	pub types: Vec<Option<ChapterType>>,
}

#[derive(
	diesel_derive_enum::DbEnum, Debug, PartialEq, JsonSchema, Deserialize, Serialize, Clone,
)]
#[DbValueStyle = "PascalCase"]
#[ExistingTypePath = "crate::schema::sql_types::ChapterTypes"]
pub enum ChapterType {
	Performance,
	Interview,
	NonMusicalInterview,
	Other,
}
