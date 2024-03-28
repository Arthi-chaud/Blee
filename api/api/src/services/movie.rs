use crate::dto::{
	artist::ArtistResponse,
	movie::{MovieFilter, MovieResponseWithRelations, MovieType},
	page::Pagination,
};
use entity::{image, movie, sea_orm_active_enums::MovieTypeEnum};
use rocket::serde::uuid::Uuid;
use sea_orm::{
	ColumnTrait, ConnectionTrait, DbErr, EntityTrait, QueryFilter, QuerySelect, QueryTrait, Set,
};
use slug::slugify;

pub async fn create<'s, 'a, C>(
	movie_name: &'s str,
	movie_type: MovieType,
	artist: &ArtistResponse,
	package_uuid: &Uuid,
	file_uuid: &Uuid,
	connection: &'a C,
) -> Result<movie::Model, DbErr>
where
	C: ConnectionTrait,
{
	let new_movie = movie::ActiveModel {
		name: Set(movie_name.to_string()),
		slug: Set(slugify(format!("{} {}", artist.name, movie_name))),
		package_id: Set(*package_uuid),
		r#type: Set(movie_type.into()),
		artist_id: Set(artist.id),
		file_id: Set(*file_uuid),
		..Default::default()
	};

	movie::Entity::insert(new_movie.clone())
		.exec_with_returning(connection)
		.await
}

pub async fn find<'a, C>(
	slug_or_uuid: &String,
	connection: &'a C,
) -> Result<MovieResponseWithRelations, DbErr>
where
	C: ConnectionTrait,
{
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

	Ok(MovieResponseWithRelations {
		movie: movie.into(),
		poster: poster.map(|x| x.into()),
		artist: None,
		package: None,
		file: None,
	})
}

pub async fn find_many<'a, C>(
	filters: &MovieFilter,
	pagination: &Pagination,
	connection: &'a C,
) -> Result<Vec<MovieResponseWithRelations>, DbErr>
where
	C: ConnectionTrait,
{
	let query = movie::Entity::find()
		.apply_if(filters.r#type, |q, r#type| {
			q.filter(movie::Column::Type.eq(MovieTypeEnum::from(r#type)))
		})
		.apply_if(filters.artist, |q, artist_uuid| {
			q.filter(movie::Column::ArtistId.eq(artist_uuid))
		})
		.apply_if(filters.package, |q, package_uuid| {
			q.filter(movie::Column::PackageId.eq(package_uuid))
		})
		.offset(pagination.skip)
		.limit(pagination.take);

	let mut joint_query = query
		.find_also_related(image::Entity)
		.cursor_by(movie::Column::Id);
	joint_query.all(connection).await.map(|items| {
		items
			.into_iter()
			.map(|(movie, poster)| MovieResponseWithRelations {
				movie: movie.into(),
				poster: poster.map(|x| x.into()),
				artist: None,
				package: None,
				file: None,
			})
			.collect()
	})
}
