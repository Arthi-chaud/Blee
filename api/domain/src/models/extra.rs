use diesel::prelude::*;
use rocket::serde::uuid::Uuid;
use crate::models::package::Package;
use crate::models::artist::Artist;

#[derive(Queryable, Identifiable, Selectable, Debug, Associations, PartialEq)]
#[diesel(table_name = crate::schema::extras)]
#[diesel(check_for_backend(diesel::pg::Pg))]
#[diesel(belongs_to(Package))]
#[diesel(belongs_to(Artist))]
/// An Extra Video file
pub struct Extra {
    pub id: Uuid,
	pub name: String,
	pub thumbnail_id: Option<Uuid>,
	pub package_id: Uuid,
	pub artist_id: Uuid,
	pub file_id: Uuid,
	pub disc_index: Option<i16>,
	pub track_index: Option<i16>,
	pub type_: Vec<Option<ExtraType>>,
}

#[derive(diesel_derive_enum::DbEnum, Debug, PartialEq)]
#[DbValueStyle = "PascalCase"]
#[ExistingTypePath = "crate::schema::sql_types::ExtraTypes"]
pub enum ExtraType {
    Trailer,
	Interview,
	BehindTheScenes,
	MusicVideo,
	AlternateView,
	Backdrops,
	Performance,
	Other
}