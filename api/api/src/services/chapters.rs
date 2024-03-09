use diesel::prelude::*;
use domain::models::{chapter::Chapter, image::Image};
use rocket::serde::uuid::Uuid;

use crate::responses::chapter::ChapterResponse;

pub fn find_by_movie(
	movie_uuid: &Uuid,
	connection: &mut PgConnection,
) -> Result<Vec<ChapterResponse>, diesel::result::Error> {
	use domain::schema::chapters::dsl::*;
	use domain::schema::images::dsl::*;

	let movie_chapters: Vec<(Chapter, Option<Image>)> = chapters
		.filter(movie_id.eq(movie_uuid))
		.left_join(images)
		.order(start_time.asc())
		.get_results(connection)?;

	Ok(movie_chapters
		.iter()
		.map(move |(chapter, thumbnail)| -> ChapterResponse {
			ChapterResponse {
				chapter: chapter.clone(),
				thumbnail: thumbnail.clone(),
			}
		})
		.collect())
}
