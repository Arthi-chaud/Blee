use diesel::prelude::*;
use rocket::serde::uuid::Uuid;

#[derive(Queryable, Identifiable, Selectable, Debug, PartialEq)]
#[diesel(table_name = crate::schema::artists)]
#[diesel(check_for_backend(diesel::pg::Pg))]
/// An Artist
pub struct Artist {
    pub id: Uuid,
    pub name: String,
    pub slug: String,
}
