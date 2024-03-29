use entity::file;
use rocket::serde::uuid::Uuid;
use sea_orm::{ColumnTrait, ConnectionTrait, DbErr, EntityTrait, QueryFilter, QueryTrait, Set};

use crate::dto::{
	file::{FileFilter, FileResponse, VideoQuality},
	page::Pagination,
};

pub async fn create_or_find<'s, 'a, C>(
	file_path: &'s str,
	file_size: u64,
	video_quality: VideoQuality,
	connection: &'a C,
) -> Result<file::Model, DbErr>
where
	C: ConnectionTrait,
{
	file::Entity::insert(file::ActiveModel {
		path: Set(file_path.to_string()),
		size: Set(file_size as i64),
		quality: Set(video_quality.into()),
		..Default::default()
	})
	.exec_with_returning(connection)
	.await
}

pub async fn find<'a, C>(uuid: &Uuid, connection: &'a C) -> Result<FileResponse, DbErr>
where
	C: ConnectionTrait,
{
	file::Entity::find_by_id(uuid.clone())
		.one(connection)
		.await?
		.map_or(Err(DbErr::RecordNotFound("File".to_string())), |r| {
			Ok(r.into())
		})
}

pub async fn find_many<'a, C>(
	filter: &FileFilter,
	pagination: &Pagination,
	connection: &'a C,
) -> Result<Vec<FileResponse>, DbErr>
where
	C: ConnectionTrait,
{
	let query = file::Entity::find().apply_if(filter.path.clone(), |q, path| {
		q.filter(file::Column::Path.starts_with(path))
	});

	let mut cursor = query.cursor_by(file::Column::Id);

	if let Some(after_id) = pagination.after_id {
		cursor.after(after_id);
	}
	cursor
		.first(pagination.page_size)
		.all(connection)
		.await
		.map(|items| items.into_iter().map(|file| file.into()).collect())
}
