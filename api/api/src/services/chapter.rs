use diesel::{prelude::*, PgConnection};
use domain::models::chapter::{Chapter, ChapterType};
use rocket::serde::uuid::Uuid;

use crate::dto::chapter::NewChapter;

#[derive(Insertable)]
#[diesel(table_name = domain::schema::chapters)]
struct CreateChapter<'s> {
	name: &'s String,
	start_time: i16,
	end_time: i16,
	movie_id: &'s Uuid,
	types: Vec<Option<ChapterType>>,
}

pub fn create_many<'s>(
	chapters: &Vec<NewChapter>,
	movie_uuid: &Uuid,
	connection: &mut PgConnection,
) -> Result<Vec<Chapter>, diesel::result::Error> {
	use domain::schema::chapters::dsl;
	let creation_dtos: Vec<CreateChapter> = chapters
		.iter()
		.map(|chapter| CreateChapter {
			name: &chapter.name,
			start_time: chapter.start_timestamp,
			end_time: chapter.end_timestamp,
			movie_id: movie_uuid,
			types: chapter.types.iter().map(|t| Some(t.clone())).collect(),
		})
		.collect();

	diesel::insert_into(dsl::chapters)
		.values(creation_dtos)
		.get_results::<Chapter>(connection)
}
