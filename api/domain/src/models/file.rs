use diesel::prelude::*;
use rocket::serde::uuid::Uuid;

use super::video_quality::VideoQuality;

#[derive(Queryable, Identifiable, Selectable, Debug, PartialEq)]
#[diesel(table_name = crate::schema::files)]
#[diesel(check_for_backend(diesel::pg::Pg))]
/// A File
pub struct File {
	pub id: Uuid,
	pub size: i64,
	pub path: String,
	pub quality: VideoQuality,
	pub scrubber_id: Option<Uuid>,
}
