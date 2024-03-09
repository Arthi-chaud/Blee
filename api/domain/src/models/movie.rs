use crate::models::artist::Artist;
use crate::models::file::File;
use crate::models::image::Image;
use crate::models::package::Package;
use chrono::NaiveDateTime;
use diesel::prelude::*;
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(
	Queryable, Identifiable, Selectable, Debug, Associations, PartialEq, JsonSchema, Serialize,
)]
#[diesel(table_name = crate::schema::movies)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(belongs_to(Package))]
#[diesel(belongs_to(Artist))]
#[diesel(belongs_to(Image, foreign_key = poster_id))]
#[diesel(belongs_to(File))]
/// A Movie
pub struct Movie {
	pub id: Uuid,
	pub name: String,
	pub slug: String,
	pub poster_id: Option<Uuid>,
	pub registered_at: NaiveDateTime,
	pub package_id: Uuid,
	pub artist_id: Uuid,
	pub file_id: Uuid,
	#[serde(rename = "type")]
	pub type_: MovieType,
}

#[derive(
	diesel_derive_enum::DbEnum, Debug, PartialEq, JsonSchema, Serialize, Deserialize, Clone, Copy,
)]
#[DbValueStyle = "PascalCase"]
#[ExistingTypePath = "crate::schema::sql_types::MovieTypes"]
pub enum MovieType {
	Concert,
	Documentary,
}
