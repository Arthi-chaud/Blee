use crate::dto::artist::ArtistWithPosterResponse;
use entity::{artist, image};
use migration::Expr;
use rocket::serde::uuid::Uuid;
use sea_orm::{sea_query, ActiveValue::Set, ColumnTrait, DbConn, DbErr, EntityTrait, QueryFilter};
use slug::slugify;

pub async fn create_or_find<'s>(
	artist_name: &'s str,
	connection: &DbConn,
) -> Result<artist::Model, DbErr> {
	let artist_slug = slugify(artist_name.to_owned());
	let new_artist = artist::ActiveModel {
		name: Set(artist_name.to_owned()),
		slug: Set(artist_slug.clone()),
		..Default::default()
	};

	artist::Entity::insert(new_artist.clone())
		.on_conflict(
			sea_query::OnConflict::column(artist::Column::Slug)
				.do_nothing()
				.to_owned(),
		)
		.exec(connection)
		.await;

	artist::Entity::find()
		.filter(artist::Column::Slug.eq(artist_slug))
		.one(connection)
		.await?
		.map_or(Err(DbErr::RecordNotFound("".to_string())), |r| Ok(r))
}

pub async fn find<'s>(
	slug_or_uuid: &'s str,
	connection: &DbConn,
) -> Result<ArtistWithPosterResponse, DbErr> {
	let uuid_parse_result = Uuid::parse_str(slug_or_uuid);
	let mut query = artist::Entity::find();

	if let Ok(uuid) = uuid_parse_result {
		query = query.filter(artist::Column::Id.eq(uuid));
	} else {
		query = query.filter(artist::Column::Slug.eq(slug_or_uuid));
	}
	let (artist, image) = query
		.find_also_related(image::Entity)
		.one(connection)
		.await?
		.map_or(Err(DbErr::RecordNotFound("Artist".to_string())), |r| Ok(r))?;

	Ok(ArtistWithPosterResponse {
		artist: artist.into(),
		poster: image.map(|i| i.into()),
	})
}

pub async fn set_poster<'s>(
	poster_uuid: &Uuid,
	artist_uuid: &Uuid,
	connection: &DbConn,
) -> Result<(), DbErr> {
	artist::Entity::update_many()
		.col_expr(artist::Column::PosterId, Expr::value(*poster_uuid))
		.filter(artist::Column::Id.eq(*artist_uuid))
		.exec(connection)
		.await
		.map(|_| ())
}
