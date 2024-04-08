use entity::external_id;
use sea_orm::{ConnectionTrait, DbErr, EntityTrait, Set};

use crate::dto::external_id::NewExternalId;

use super::{artist, package};

pub async fn create<'s, 'a, C>(
	dto: &NewExternalId,
	connection: &'a C,
) -> Result<external_id::Model, DbErr>
where
	C: ConnectionTrait,
{
	if let Some(artist_id) = dto.artist_id {
		artist::find(&artist_id.to_string(), connection).await?;
	} else if let Some(package_id) = dto.package_id {
		package::find(&package_id.to_string(), connection).await?;
	}
	external_id::Entity::insert(external_id::ActiveModel {
		description: Set(dto.description.clone()),
		rating: Set(dto.rating),
		provider_name: Set(dto.provider_name.clone()),
		value: Set(dto.value.clone()),
		url: Set(dto.url.clone()),
		package_id: Set(dto.package_id),
		artist_id: Set(dto.artist_id),
		..Default::default()
	})
	.exec_with_returning(connection)
	.await
}
