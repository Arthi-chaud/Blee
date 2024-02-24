use chrono::{NaiveDateTime, NaiveDate};
use diesel::prelude::*;
use rocket::serde::uuid::Uuid;
use crate::models::artist::Artist;
use crate::models::image::Image;

#[derive(Queryable, Identifiable, Selectable, Associations, PartialEq)]
#[diesel(table_name = crate::schema::packages)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(belongs_to(Artist))]
#[diesel(belongs_to(Image, foreign_key = poster_id))]
//TODO: add relation with banner
/// A Package
pub struct Package {
    pub id: Uuid,
	pub name: String,
	pub description: Option<String>,
	pub release_year: Option<NaiveDate>,
	pub artist_id: Option<Uuid>,
	pub poster_id: Option<Uuid>,
	pub banner_id: Option<Uuid>,
	pub registered_at: NaiveDateTime,
}
