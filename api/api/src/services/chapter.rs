use crate::dto::chapter::{ChapterResponseWithThumbnail, NewChapter};
use entity::{chapter, image, sea_orm_active_enums::ChapterTypeEnum};
use rocket::serde::uuid::Uuid;
use sea_orm::{ColumnTrait, ConnectionTrait, DbErr, EntityTrait, QueryFilter, QueryOrder, Set};

pub async fn create_many<'s, 'a, C>(
	chapters: &Vec<NewChapter>,
	movie_uuid: &Uuid,
	connection: &'a C,
) -> Result<Vec<chapter::Model>, DbErr>
where
	C: ConnectionTrait,
{
	let creation_dtos: Vec<chapter::ActiveModel> = chapters
		.iter()
		.map(|c| chapter::ActiveModel {
			name: Set(c.name.clone()),
			start_time: Set(c.start_timestamp as i32),
			end_time: Set(c.end_timestamp as i32),
			movie_id: Set(movie_uuid.clone()),
			r#type: Set(c.types.iter().map(|e| ChapterTypeEnum::from(*e)).collect()),
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

pub async fn find_by_movie<'a, C>(
	movie_uuid: &Uuid,
	connection: &'a C,
) -> Result<Vec<ChapterResponseWithThumbnail>, DbErr>
where
	C: ConnectionTrait,
{
	let movie_chapters: Vec<(chapter::Model, Option<image::Model>)> = chapter::Entity::find()
		.find_also_related(image::Entity)
		.filter(chapter::Column::MovieId.eq(movie_uuid.clone()))
		.order_by_asc(chapter::Column::StartTime)
		.all(connection)
		.await?;

	Ok(movie_chapters
		.iter()
		.map(|(chapter, thumbnail)| -> ChapterResponseWithThumbnail {
			ChapterResponseWithThumbnail {
				chapter: chapter.clone().into(),
				thumbnail: thumbnail.clone().map(|x| x.into()),
			}
		})
		.collect())
}

pub async fn find<'a, C>(
	uuid: &Uuid,
	connection: &'a C,
) -> Result<ChapterResponseWithThumbnail, DbErr>
where
	C: ConnectionTrait,
{
	let (chapter, thumbnail) = chapter::Entity::find_by_id(*uuid)
		.find_also_related(image::Entity)
		.one(connection)
		.await?
		.map_or(Err(DbErr::RecordNotFound("Chapter".to_string())), |r| Ok(r))?;

	Ok(ChapterResponseWithThumbnail {
		chapter: chapter.clone().into(),
		thumbnail: thumbnail.clone().map(|x| x.into()),
	})
}
