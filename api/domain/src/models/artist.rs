use crate::models::image::Image;
use chrono::naive::NaiveDateTime;
use diesel::prelude::*;
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::Serialize;

#[derive(
	QueryableByName,
	Queryable,
	JsonSchema,
	Identifiable,
	Selectable,
	Debug,
	PartialEq,
	Associations,
	Insertable,
	Serialize,
	Clone,
)]
#[diesel(table_name = crate::schema::artists)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(belongs_to(Image, foreign_key = poster_id))]
/// An Artist
pub struct Artist {
	pub id: Uuid,
	pub name: String,
	pub slug: String,
	pub description: Option<String>,
	pub registered_at: NaiveDateTime,
	pub poster_id: Option<Uuid>,
}
