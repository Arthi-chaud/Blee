use crate::dto::{
	artist::ArtistResponse,
	movie::{MovieFilter, MovieResponse, MovieResponseWithRelations, MovieSort, MovieType},
	page::Pagination,
	sort::Sort,
};
use chrono::NaiveDate;
use entity::{artist, image, movie, package, sea_orm_active_enums::MovieTypeEnum};
use rocket::serde::uuid::Uuid;
use sea_orm::{
	sea_query::*, ColumnTrait, ConnectionTrait, DbErr, EntityTrait, FromQueryResult, JoinType,
	QueryFilter, QueryOrder, QuerySelect, QueryTrait, RelationTrait, Set,
};
use slug::slugify;

#[derive(FromQueryResult)]
pub struct MovieModel {
	pub id: Uuid,
	pub name: String,
	pub unique_slug: String,
	pub thumbnail_id: Option<Uuid>,
	pub package_id: Uuid,
	pub artist_id: Uuid,
	pub artist_name: String,
	pub package_name: String,
	pub file_id: Uuid,
	pub registered_at: NaiveDate,
	pub r#type: MovieTypeEnum,
}

impl From<MovieModel> for MovieResponse {
	fn from(value: MovieModel) -> Self {
		MovieResponse {
			id: value.id,
			name: value.name,
			slug: value.unique_slug,
			thumbnail_id: value.thumbnail_id,
			registered_at: value.registered_at.into(),
			package_id: value.package_id,
			artist_id: value.artist_id,
			file_id: value.file_id,
			artist_name: value.artist_name,
			package_name: value.package_name,
			type_: value.r#type.into(),
		}
	}
}

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
		unique_slug: Set(slugify(format!("{} {}", artist.name, movie_name))),
		name_slug: Set(slugify(movie_name)),
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
	let mut query = movie::Entity::find()
		.column_as(artist::Column::Name, "artist_name")
		.join(JoinType::InnerJoin, movie::Relation::Artist.def())
		.column_as(package::Column::Name, "package_name")
		.join(JoinType::InnerJoin, movie::Relation::Package.def());

	if let Ok(uuid) = uuid_parse_result {
		query = query.filter(movie::Column::Id.eq(uuid));
	} else {
		query = query.filter(movie::Column::UniqueSlug.eq(slug_or_uuid));
	}

	let (movie, thumbnail) = query
		.find_also_related(image::Entity)
		.into_model::<MovieModel, image::Model>()
		.one(connection)
		.await?
		.map_or(Err(DbErr::RecordNotFound("Movie".to_string())), |r| Ok(r))?;

	Ok(MovieResponseWithRelations {
		movie: movie.into(),
		thumbnail: thumbnail.map(|x| x.into()),
	})
}

pub async fn find_many<'a, C>(
	filters: &MovieFilter,
	sort: Option<Sort<MovieSort>>,
	pagination: &Pagination,
	connection: &'a C,
) -> Result<Vec<MovieResponseWithRelations>, DbErr>
where
	C: ConnectionTrait,
{
	let artist_alias = Alias::new("artist");
	let package_alias = Alias::new("package");
	let mut query = movie::Entity::find()
		.join_as(
			JoinType::LeftJoin,
			movie::Relation::Artist.def(),
			artist_alias.clone(),
		)
		.join_as(
			JoinType::LeftJoin,
			movie::Relation::Package.def(),
			package_alias.clone(),
		)
		.column_as(
			Expr::col((artist_alias.clone(), artist::Column::Name)),
			"artist_name",
		)
		.column_as(
			Expr::col((package_alias.clone(), package::Column::Name)),
			"package_name",
		)
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

	if let Some(s) = sort {
		query = match s.sort_by {
			MovieSort::Name => query.order_by(movie::Column::NameSlug, s.order.into()),
			MovieSort::AddDate => query.order_by(movie::Column::RegisteredAt, s.order.into()),
			MovieSort::ArtistName => query
				.order_by(
					Expr::col((artist_alias, movie::Column::UniqueSlug)),
					s.order.into(),
				)
				.order_by(movie::Column::NameSlug, sea_orm::Order::Asc),
			MovieSort::PackageName => query.order_by(
				Expr::col((package_alias, package::Column::NameSlug)),
				s.order.into(),
			),
			MovieSort::ReleaseDate => query.order_by(
				Expr::col((package_alias, package::Column::ReleaseYear)),
				s.order.into(),
			),
		}
	}

	query
		.find_also_related(image::Entity)
		.into_model::<MovieModel, image::Model>()
		.all(connection)
		.await
		.map(|items| {
			items
				.into_iter()
				.map(|(movie, thumbnail)| MovieResponseWithRelations {
					movie: movie.into(),
					thumbnail: thumbnail.map(|x| x.into()),
				})
				.collect()
		})
}
