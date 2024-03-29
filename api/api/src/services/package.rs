use crate::dto::{
	artist::ArtistResponse,
	package::{PackageFilter, PackageResponseWithRelations},
	page::Pagination,
};
use ::slug::slugify;
use entity::{image, package};
use rocket::serde::uuid::Uuid;
use sea_orm::{
	sea_query, ColumnTrait, ConnectionTrait, DbErr, EntityTrait, QueryFilter, QuerySelect,
	QueryTrait, Set,
};

pub async fn create_or_find<'s, 'a, C>(
	artist: &Option<ArtistResponse>,
	package_name: &'s str,
	release_date: Option<chrono::NaiveDate>,
	connection: &'a C,
) -> Result<package::Model, DbErr>
where
	C: ConnectionTrait,
{
	let artist_name = artist
		.as_ref()
		.map_or(String::from("Various Artist"), |a| a.name.clone());
	let unique_slug = slugify(format!("{} {}", artist_name, package_name));
	let new_package = package::ActiveModel {
		name: Set(package_name.to_string()),
		unique_slug: Set(unique_slug.to_owned()),
		name_slug: Set(slugify(package_name)),
		release_year: Set(release_date),
		artist_id: Set(artist.as_ref().map(|a| a.id)),
		..Default::default()
	};

	let _ = package::Entity::insert(new_package.clone())
		.on_conflict(
			sea_query::OnConflict::column(package::Column::UniqueSlug)
				.do_nothing()
				.to_owned(),
		)
		.exec(connection)
		.await;

	package::Entity::find()
		.filter(package::Column::UniqueSlug.eq(unique_slug))
		.one(connection)
		.await?
		.map_or(Err(DbErr::RecordNotFound("Package".to_string())), |r| Ok(r))
}

pub async fn find<'a, C>(
	slug_or_uuid: &String,
	connection: &'a C,
) -> Result<PackageResponseWithRelations, DbErr>
where
	C: ConnectionTrait,
{
	let uuid_parse_result = Uuid::parse_str(slug_or_uuid);
	let mut query = package::Entity::find();

	if let Ok(uuid) = uuid_parse_result {
		query = query.filter(package::Column::Id.eq(uuid));
	} else {
		query = query.filter(package::Column::UniqueSlug.eq(slug_or_uuid));
	}

	let (package, poster) = query
		.find_also_related(image::Entity)
		.one(connection)
		.await?
		.map_or(Err(DbErr::RecordNotFound("Package".to_string())), |r| Ok(r))?;

	Ok(PackageResponseWithRelations {
		package: package.into(),
		poster: poster.map(|x| x.into()),
		artist: None,
	})
}

pub async fn find_many<'a, C>(
	filters: &PackageFilter,
	pagination: &Pagination,
	connection: &'a C,
) -> Result<Vec<PackageResponseWithRelations>, DbErr>
where
	C: ConnectionTrait,
{
	let query = package::Entity::find()
		.apply_if(filters.artist, |q, artist_uuid| {
			q.filter(package::Column::ArtistId.eq(artist_uuid))
		})
		.offset(pagination.skip)
		.limit(pagination.take);

	let mut joint_query = query
		.find_also_related(image::Entity)
		.cursor_by(package::Column::Id);

	joint_query.all(connection).await.map(|items| {
		items
			.into_iter()
			.map(|(package, poster)| PackageResponseWithRelations {
				package: package.into(),
				poster: poster.map(|x| x.into()),
				artist: None,
			})
			.collect()
	})
}
