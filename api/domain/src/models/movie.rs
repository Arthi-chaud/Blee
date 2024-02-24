use chrono::NaiveDateTime;
use diesel::prelude::*;
use rocket::serde::uuid::Uuid;
use crate::models::package::Package;

#[derive(Queryable, Identifiable, Selectable, Debug, Associations, PartialEq)]
#[diesel(table_name = crate::schema::movies)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(belongs_to(Package))]
/// A Movie
pub struct Movie {
    pub id: Uuid,
	pub name: String,
	pub poster_id: Option<Uuid>,
	pub package_id: Uuid,
	pub artist_id: Uuid,
	pub file_id: Uuid,
	pub disc_index: i16,
	pub track_index: i16,
	pub type_: Vec<Option<MovieType>>,
	pub registered_at: NaiveDateTime,
}


#[derive(diesel_derive_enum::DbEnum, Debug, PartialEq)]
#[DbValueStyle = "PascalCase"]
#[ExistingTypePath = "crate::schema::sql_types::MovieTypes"]
pub enum MovieType {
    Concert, Documentary
}