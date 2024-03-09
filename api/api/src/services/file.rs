use diesel::prelude::*;
use domain::models::{file::File, video_quality::VideoQuality};
use rocket::serde::uuid::Uuid;

pub fn create_or_find<'s>(
	file_path: &'s str,
	file_size: i64,
	video_quality: VideoQuality,
	connection: &mut PgConnection,
) -> Result<File, diesel::result::Error> {
	use domain::schema::files::dsl::*;

	#[derive(Insertable)]
	#[diesel(table_name = domain::schema::files)]
	struct NewFile<'s> {
		path: &'s str,
		size: i64,
		quality: VideoQuality,
	}
	let creation_dto = NewFile {
		path: file_path,
		size: file_size,
		quality: video_quality,
	};

	diesel::insert_into(files)
		.values(&creation_dto)
		.get_result(connection)
}

pub fn find(uuid: &Uuid, connection: &mut PgConnection) -> Result<File, diesel::result::Error> {
	use domain::schema::files::dsl::*;

	files.filter(id.eq(uuid)).first(connection)
}
