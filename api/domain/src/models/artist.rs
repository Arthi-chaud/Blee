use chrono::naive::NaiveDateTime;
use diesel::prelude::*;
use rocket::serde::uuid::Uuid;
use crate::models::image::Image;

#[derive(QueryableByName, Queryable, Identifiable, Selectable, Debug, PartialEq, Associations, Insertable, Clone)]
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
