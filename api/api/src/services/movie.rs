use crate::dto::movie::MovieResponse;
use entity::{artist, image, movie, sea_orm_active_enums::MovieTypeEnum};
use rocket::serde::uuid::Uuid;
use sea_orm::{ColumnTrait, DbConn, DbErr, EntityTrait, QueryFilter, Set};
use slug::slugify;

pub async fn create<'s>(
	movie_name: &'s str,
	movie_type: MovieTypeEnum,
	artist: &artist::Model,
	package_uuid: &Uuid,
	file_uuid: &Uuid,
	connection: &DbConn,
) -> Result<movie::Model, DbErr> {
	let new_movie = movie::ActiveModel {
		name: Set(movie_name.to_string()),
		slug: Set(slugify(format!("{} {}", artist.name, movie_name))),
		package_id: Set(*package_uuid),
		r#type: Set(movie_type),
		artist_id: Set(artist.id),
		file_id: Set(*file_uuid),
		..Default::default()
	};

	movie::Entity::insert(new_movie.clone())
		.exec_with_returning(connection)
		.await
}

pub async fn find(slug_or_uuid: &String, connection: &DbConn) -> Result<MovieResponse, DbErr> {
	let uuid_parse_result = Uuid::parse_str(slug_or_uuid);
	let mut query = movie::Entity::find();

	if let Ok(uuid) = uuid_parse_result {
		query = query.filter(movie::Column::Id.eq(uuid));
	} else {
		query = query.filter(movie::Column::Slug.eq(slug_or_uuid));
	}

	let (movie, poster) = query
		.find_also_related(image::Entity)
		.one(connection)
		.await?
		.map_or(Err(DbErr::RecordNotFound("Movie".to_string())), |r| Ok(r))?;

	Ok(MovieResponse {
		movie,
		poster,
		artist: None,
		package: None,
		file: None,
	})
}
