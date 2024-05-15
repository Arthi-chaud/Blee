use crate::dto::{
	artist::ArtistResponse,
	package::{
		PackageFilter, PackageResponse, PackageResponseWithRelations, PackageSort, UpdatePackage,
	},
	page::Pagination,
	sort::Sort,
};
use ::slug::slugify;
use chrono::NaiveDate;
use entity::{
	artist, image,
	package::{self},
};
use rocket::serde::uuid::Uuid;
use sea_orm::ActiveModelTrait;
use sea_orm::{
	sea_query::{self, *},
	ColumnTrait, ConnectionTrait, DbErr, EntityTrait, FromQueryResult, QueryFilter, QueryOrder,
	QuerySelect, QueryTrait, RelationTrait, Set,
};

#[derive(FromQueryResult)]
struct PackageModel {
	pub id: Uuid,
	pub name: String,
	pub unique_slug: String,
	pub release_year: Option<NaiveDate>,
	pub registered_at: NaiveDate,
	pub artist_id: Option<Uuid>,
	pub poster_id: Option<Uuid>,
	pub artist_name: Option<String>,
}

impl From<PackageModel> for PackageResponse {
	fn from(value: PackageModel) -> Self {
		PackageResponse {
			id: value.id,
			name: value.name,
			artist_name: value.artist_name,
			slug: value.unique_slug,
			release_year: value.release_year,
			registered_at: value.registered_at.into(),
			artist_id: value.artist_id,
			poster_id: value.poster_id,
		}
	}
}

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
	let mut query = package::Entity::find()
		.column_as(artist::Column::Name, "artist_name")
		.join(JoinType::InnerJoin, package::Relation::Artist.def());

	if let Ok(uuid) = uuid_parse_result {
		query = query.filter(package::Column::Id.eq(uuid));
	} else {
		query = query.filter(package::Column::UniqueSlug.eq(slug_or_uuid));
	}

	let (package, poster) = query
		.find_also_related(image::Entity)
		.into_model::<PackageModel, image::Model>()
		.one(connection)
		.await?
		.map_or(Err(DbErr::RecordNotFound("Package".to_string())), |r| Ok(r))?;

	Ok(PackageResponseWithRelations {
		package: package.into(),
		poster: poster.map(|x| x.into()),
	})
}

pub async fn find_many<'a, C>(
	filters: &PackageFilter,
	sort: Option<Sort<PackageSort>>,
	pagination: &Pagination,
	connection: &'a C,
) -> Result<Vec<PackageResponseWithRelations>, DbErr>
where
	C: ConnectionTrait,
{
	let mut query = package::Entity::find()
		.join_as(
			JoinType::LeftJoin,
			package::Relation::Artist.def(),
			Alias::new("artist"),
		)
		.column_as(
			Expr::col((Alias::new("artist"), artist::Column::Name)),
			"artist_name",
		)
		.apply_if(filters.artist, |q, artist_uuid| {
			q.filter(package::Column::ArtistId.eq(artist_uuid))
		})
		.offset(pagination.skip)
		.limit(pagination.take);

	if let Some(s) = sort {
		query = match s.sort_by {
			PackageSort::Name => query.order_by(package::Column::NameSlug, s.order.into()),
			PackageSort::AddDate => query.order_by(package::Column::RegisteredAt, s.order.into()),
			PackageSort::ReleaseDate => {
				//TODO: Handle nulls
				query.order_by(package::Column::ReleaseYear, s.order.into())
			}
			PackageSort::ArtistName => query
				.order_by(
					Expr::col((Alias::new("artist"), artist::Column::UniqueSlug)),
					s.order.into(),
				)
				.order_by(package::Column::NameSlug, sea_orm::Order::Asc),
		}
	}

	let joint_query = query
		.find_also_related(image::Entity)
		.into_model::<PackageModel, image::Model>();

	joint_query.all(connection).await.map(|items| {
		items
			.into_iter()
			.map(|(package, poster)| PackageResponseWithRelations {
				package: package.into(),
				poster: poster.map(|x| x.into()),
			})
			.collect()
	})
}

pub async fn update<'s, 'a, C>(
	package_uuid: &Uuid,
	update_dto: &UpdatePackage,
	connection: &'a C,
) -> Result<(), DbErr>
where
	C: ConnectionTrait,
{
	let package: package::Model = package::Entity::find_by_id(*package_uuid)
		.one(connection)
		.await?
		.map_or(Err(DbErr::RecordNotFound("Package".to_string())), |r| Ok(r))?;
	let mut package: package::ActiveModel = package.into();
	package.release_year = Set(update_dto.release_date);
	package.update(connection).await?;
	Ok(())
}
