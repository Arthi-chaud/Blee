use entity::{chapter, extra, file, movie};
use rocket::serde::uuid::Uuid;
use sea_orm::{
	ColumnTrait, ConnectionTrait, DbErr, EntityTrait, QueryFilter, QueryOrder, QuerySelect,
	QueryTrait, Set,
};

use crate::{
	config::Config,
	dto::{
		file::{FileFilter, FileResponse, FileSort, VideoQuality},
		page::Pagination,
		sort::Sort,
	},
};

use super::{housekeeping, image};

pub async fn create_or_find<'s, 'a, C>(
	file_path: &'s str,
	file_size: u64,
	file_duration: u64,
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
		duration: Set(file_duration as i64),
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

pub async fn find_by_path<'a, C>(path: &str, connection: &'a C) -> Result<FileResponse, DbErr>
where
	C: ConnectionTrait,
{
	file::Entity::find()
		.filter(file::Column::Path.eq(path))
		.one(connection)
		.await?
		.map_or(Err(DbErr::RecordNotFound("File".to_string())), |r| {
			Ok(r.into())
		})
}

pub async fn find_many<'a, C>(
	filter: &FileFilter,
	sort: Option<Sort<FileSort>>,
	pagination: &Pagination,
	connection: &'a C,
) -> Result<Vec<FileResponse>, DbErr>
where
	C: ConnectionTrait,
{
	let mut query = file::Entity::find()
		.apply_if(filter.path.clone(), |q, path| {
			q.filter(file::Column::Path.starts_with(path))
		})
		.offset(pagination.skip)
		.limit(pagination.take);

	if let Some(s) = sort {
		query = match s.sort_by {
			FileSort::Path => query.order_by(file::Column::Path, s.order.into()),
			FileSort::AddDate => query.order_by(file::Column::RegisteredAt, s.order.into()),
			FileSort::Size => query.order_by(file::Column::Size, s.order.into()),
		}
	}
	query
		.all(connection)
		.await
		.map(|items| items.into_iter().map(|file| file.into()).collect())
}

pub async fn delete<'a, C>(uuid: &Uuid, connection: &'a C, config: &Config) -> Result<(), DbErr>
where
	C: ConnectionTrait,
{
	let file_to_delete = find(uuid, connection).await?;

	let related_extras = extra::Entity::find()
		.filter(extra::Column::FileId.eq(file_to_delete.id))
		.all(connection)
		.await?;
	if let Some(related_extra) = related_extras.first() {
		if let Some(extra_poster) = related_extra.thumbnail_id {
			image::delete(&extra_poster, connection, config).await?;
		}
		extra::Entity::delete_by_id(related_extra.id)
			.exec(connection)
			.await?;
	} else {
		let related_movies = movie::Entity::find()
			.filter(movie::Column::FileId.eq(file_to_delete.id))
			.all(connection)
			.await?;
		if let Some(related_movie) = related_movies.first() {
			if let Some(movie_poster) = related_movie.thumbnail_id {
				image::delete(&movie_poster, connection, config).await?;
			}
			chapter::Entity::delete_many()
				.filter(chapter::Column::MovieId.eq(related_movie.id))
				.exec(connection)
				.await?;
			movie::Entity::delete_by_id(related_movie.id)
				.exec(connection)
				.await?;
		}
	}
	if let Some(scrubber_id) = file_to_delete.scrubber_id {
		image::delete(&scrubber_id, connection, config).await?;
	}
	file::Entity::delete_by_id(file_to_delete.id)
		.exec(connection)
		.await?;
	housekeeping::housekeeping(connection).await
}
