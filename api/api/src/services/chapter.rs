use crate::dto::chapter::{ChapterResponseWithThumbnail, NewChapter};
use entity::{chapter, image};
use rocket::serde::uuid::Uuid;
use sea_orm::{ColumnTrait, DbConn, DbErr, EntityTrait, QueryFilter, QueryOrder, Set};

pub async fn create_many<'s>(
	chapters: &Vec<NewChapter>,
	movie_uuid: &Uuid,
	connection: &DbConn,
) -> Result<Vec<chapter::Model>, DbErr> {
	let creation_dtos: Vec<chapter::ActiveModel> = chapters
		.iter()
		.map(|c| chapter::ActiveModel {
			name: Set(c.name.clone()),
			start_time: Set(c.start_timestamp as i32),
			end_time: Set(c.end_timestamp as i32),
			movie_id: Set(movie_uuid.clone()),
			r#type: Set((*(c.types.first().unwrap())).into()), // TODO
			..Default::default()
		})
		.collect();

	// This is horrendous, but we cannot use insert_many with exec_with_returning
	let mut created_chapters: Vec<chapter::Model> = vec![];
	for chapter in creation_dtos {
		created_chapters.push(
			chapter::Entity::insert(chapter)
				.exec_with_returning(connection)
				.await?,
		)
	}

	Ok(created_chapters)
}

pub async fn find_by_movie(
	movie_uuid: &Uuid,
	connection: &DbConn,
) -> Result<Vec<ChapterResponseWithThumbnail>, DbErr> {
	let movie_chapters: Vec<(chapter::Model, Option<image::Model>)> = chapter::Entity::find()
		.find_also_related(image::Entity)
		.filter(chapter::Column::MovieId.eq(movie_uuid.clone()))
		.order_by_asc(chapter::Column::StartTime)
		.all(connection)
		.await?;

	Ok(movie_chapters
		.iter()
		.map(
			move |(chapter, thumbnail)| -> ChapterResponseWithThumbnail {
				ChapterResponseWithThumbnail {
					chapter: (*chapter).into(),
					thumbnail: (*thumbnail).into(),
				}
			},
		)
		.collect())
}
