use entity::{file, sea_orm_active_enums::VideoQualityEnum};
use rocket::serde::uuid::Uuid;
use sea_orm::{DbConn, DbErr, EntityTrait, Set};

use crate::dto::file::FileResponse;

pub async fn create_or_find<'s>(
	file_path: &'s str,
	file_size: u64,
	video_quality: VideoQualityEnum,
	connection: &DbConn,
) -> Result<file::Model, DbErr> {
	file::Entity::insert(file::ActiveModel {
		path: Set(file_path.to_string()),
		size: Set(file_size as i64),
		quality: Set(video_quality),
		..Default::default()
	})
	.exec_with_returning(connection)
	.await
}

pub async fn find(uuid: &Uuid, connection: &DbConn) -> Result<FileResponse, DbErr> {
	file::Entity::find_by_id(uuid.clone())
		.one(connection)
		.await?
		.map_or(Err(DbErr::RecordNotFound("File".to_string())), |r| {
			Ok(r.into())
		})
}
