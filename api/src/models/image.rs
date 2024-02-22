use diesel::prelude::*;
use rocket::serde::uuid::Uuid;

#[derive(Queryable, Identifiable, Selectable, PartialEq)]
#[diesel(table_name = crate::schema::images)]
#[diesel(check_for_backend(diesel::pg::Pg))]
/// An Image
pub struct Image {
    pub id: Uuid,
    pub blurhash: String,
    pub colors: Vec<Option<String>>,
    pub type_: ImageType
}

#[derive(diesel_derive_enum::DbEnum, Debug, PartialEq)]
#[ExistingTypePath = "crate::schema::sql_types::ImageTypes"]
pub enum ImageType {
    Poster, Banner, Thumbnail
}