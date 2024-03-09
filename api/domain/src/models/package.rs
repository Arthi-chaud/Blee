use crate::models::artist::Artist;
use crate::models::image::Image;
use chrono::{NaiveDate, NaiveDateTime};
use diesel::prelude::*;
use rocket::serde::uuid::Uuid;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::Serialize;

#[derive(Queryable, Identifiable, Selectable, Associations, PartialEq, Serialize, JsonSchema)]
#[diesel(table_name = crate::schema::packages)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(belongs_to(Artist))]
#[diesel(belongs_to(Image, foreign_key = poster_id))]
/// A Package
pub struct Package {
	pub id: Uuid,
	pub name: String,
	pub slug: String,
	pub description: Option<String>,
	pub release_year: Option<NaiveDate>,
	pub registered_at: NaiveDateTime,
	pub artist_id: Option<Uuid>,
	pub poster_id: Option<Uuid>,
	pub banner_id: Option<Uuid>,
}
