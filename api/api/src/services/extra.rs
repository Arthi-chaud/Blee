use crate::dto::extra::ExtraResponseWithRelations;
use entity::{extra, image, sea_orm_active_enums::ExtraTypeEnum};
use rocket::serde::uuid::Uuid;
use sea_orm::{DbConn, DbErr, EntityTrait, Set};
use slug::slugify;

pub async fn create<'s>(
	extra_name: &'s str,
	disc: Option<i32>,
	track: Option<i32>,
	types: &Vec<ExtraTypeEnum>,
	package_uuid: &Uuid,
	artist_uuid: &Uuid,
	file_uuid: &Uuid,
	connection: &DbConn,
) -> Result<extra::Model, DbErr> {
	let creation_dto = extra::ActiveModel {
		name: Set(extra_name.to_string()),
		slug: Set(slugify(extra_name)),
		package_id: Set(*package_uuid),
		artist_id: Set(*artist_uuid),
		file_id: Set(*file_uuid),
		disc_index: Set(disc),
		track_index: Set(track),
		r#type: Set(types.first().unwrap().clone()), //TODO
		..Default::default()
	};

	extra::Entity::insert(creation_dto)
		.exec_with_returning(connection)
		.await
}

pub async fn find(uuid: &Uuid, connection: &DbConn) -> Result<ExtraResponseWithRelations, DbErr> {
	let (extra, image) = extra::Entity::find_by_id(*uuid)
		.find_also_related(image::Entity)
		.one(connection)
		.await?
		.map_or(Err(DbErr::RecordNotFound("Extra".to_string())), |r| Ok(r))?;

	Ok(ExtraResponseWithRelations {
		extra: extra.into(),
		thumbnail: image.map(|x| x.into()),
		artist: None,
		package: None,
		file: None,
	})
}
