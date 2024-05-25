use crate::dto::{
	artist::ArtistResponse,
	image::{ImageResponse, ImageType},
	package::{
		PackageFilter, PackageResponse, PackageResponseWithRelations, PackageSort, UpdatePackage,
	},
	page::Pagination,
	sort::Sort,
};
use ::slug::slugify;
use chrono::NaiveDate;
use entity::{artist, image, package};
use rocket::serde::uuid::Uuid;
use sea_orm::{
	sea_query::{self, *},
	ColumnTrait, ConnectionTrait, DbErr, EntityTrait, FromQueryResult, Iterable, QueryFilter,
	QueryOrder, QuerySelect, QueryTrait, RelationTrait, Set,
};
use sea_orm::{ActiveModelTrait, Select};

#[derive(FromQueryResult)]
struct PackageModel {
	pub id: Uuid,
	pub name: String,
	pub unique_slug: String,
	pub release_year: Option<NaiveDate>,
	pub registered_at: NaiveDate,
	pub artist_id: Option<Uuid>,
	pub artist_name: Option<String>,
	pub banner_id: Option<Uuid>,
	pub banner_blurhash: Option<String>,
	pub banner_aspect_ratio: Option<f32>,
	pub banner_colors: Option<Vec<String>>,
	pub poster_id: Option<Uuid>,
	pub poster_blurhash: Option<String>,
	pub poster_aspect_ratio: Option<f32>,
	pub poster_colors: Option<Vec<String>>,
}

impl From<PackageModel> for PackageResponseWithRelations {
	fn from(value: PackageModel) -> Self {
		PackageResponseWithRelations {
			package: PackageResponse {
				id: value.id,
				name: value.name,
				artist_name: value.artist_name,
				slug: value.unique_slug,
				release_year: value.release_year,
				registered_at: value.registered_at.into(),
				artist_id: value.artist_id,
				poster_id: value.poster_id,
				banner_id: value.banner_id,
			},
			poster: if let Some(p_id) = value.poster_id {
				Some(ImageResponse {
					id: p_id,
					blurhash: value.poster_blurhash.unwrap(),
					aspect_ratio: value.poster_aspect_ratio.unwrap(),
					r#type: ImageType::Poster,
					colors: value.poster_colors.unwrap(),
				})
			} else {
				None
			},
			banner: if let Some(b_id) = value.banner_id {
				Some(ImageResponse {
					id: b_id,
					blurhash: value.banner_blurhash.unwrap(),
					aspect_ratio: value.banner_aspect_ratio.unwrap(),
					// Note: Deserializing Enum does not work (?)
					// So we do not select it, and determine it by hand
					r#type: ImageType::Banner,
					colors: value.banner_colors.unwrap(),
				})
			} else {
				None
			},
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

	let model = join_and_select_related_image(&query)
		.into_model::<PackageModel>()
		.one(connection)
		.await?
		.map_or(Err(DbErr::RecordNotFound("Package".to_string())), |r| Ok(r))?;

	Ok(model.into())
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

	let joint_query = join_and_select_related_image(&query).into_model::<PackageModel>();

	joint_query
		.all(connection)
		.await
		.map(|items| items.into_iter().map(|model| model.into()).collect())
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

fn join_and_select_related_image(query: &Select<package::Entity>) -> Select<package::Entity> {
	let poster_alias = (Alias::new("poster"), package::Relation::Image1);
	let banner_alias = (Alias::new("banner"), package::Relation::Image2);
	let mut new_query = query.clone();
	for (alias, col) in vec![poster_alias, banner_alias] {
		new_query = new_query.join_as(JoinType::LeftJoin, col.def(), alias.clone());
		for column in
			image::Column::iter().filter(|x| x.to_string().ne("id") && x.to_string().ne("type"))
		{
			new_query = new_query.column_as(
				Expr::col((alias.clone(), column)),
				format!("{}_{}", alias.to_string(), column.to_string()),
			);
		}
	}
	new_query
}
