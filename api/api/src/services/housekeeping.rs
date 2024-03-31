use entity::{artist, extra, image, movie, package};
use sea_orm::{
	sea_query::*, ColumnTrait, ConnectionTrait, DbErr, EntityTrait, QueryFilter, QuerySelect,
	RelationTrait,
};

// Cleanse the database from 'empty' resources (packages, artist, etc.)
pub async fn housekeeping<'a, C>(connection: &'a C) -> Result<(), DbErr>
where
	C: ConnectionTrait,
{
	let empty_packages = package::Entity::find()
		.join(JoinType::LeftJoin, package::Relation::Extra.def())
		.join(JoinType::LeftJoin, package::Relation::Movie.def())
		.column_as(extra::Column::PackageId.count(), "extra_count")
		.column_as(movie::Column::PackageId.count(), "movie_count")
		.group_by(package::Column::Id)
		.having(
			extra::Column::Id
				.count()
				.add(movie::Column::Id.count())
				.eq(0),
		)
		.all(connection)
		.await?;
	image::Entity::delete_many()
		.filter(image::Column::Id.is_in(empty_packages.iter().map(|p| p.poster_id)))
		.exec(connection)
		.await?;
	package::Entity::delete_many()
		.filter(package::Column::Id.is_in(empty_packages.iter().map(|p| p.id)))
		.exec(connection)
		.await?;

	let empty_artists = artist::Entity::find()
		.join(JoinType::LeftJoin, artist::Relation::Extra.def())
		.join(JoinType::LeftJoin, artist::Relation::Package.def())
		.join(JoinType::LeftJoin, artist::Relation::Movie.def())
		.column_as(extra::Column::ArtistId.count(), "extra_count")
		.column_as(movie::Column::ArtistId.count(), "movie_count")
		.column_as(package::Column::ArtistId.count(), "package_count")
		.group_by(artist::Column::Id)
		.having(
			extra::Column::Id
				.count()
				.add(package::Column::Id.count())
				.add(movie::Column::Id.count())
				.eq(0),
		)
		.all(connection)
		.await?;

	image::Entity::delete_many()
		.filter(image::Column::Id.is_in(empty_artists.iter().map(|a| a.poster_id)))
		.exec(connection)
		.await?;
	artist::Entity::delete_many()
		.filter(artist::Column::Id.is_in(empty_artists.iter().map(|a| a.id)))
		.exec(connection)
		.await?;
	Ok(())
}
