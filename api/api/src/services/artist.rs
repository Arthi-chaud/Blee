use crate::dto::{
	artist::{ArtistFilter, ArtistResponse, ArtistSort, ArtistWithPosterResponse},
	page::Pagination,
	sort::Sort,
};
use entity::{artist, extra, image, movie, package};
use rocket::serde::uuid::Uuid;
use sea_orm::{
	sea_query, ActiveValue::Set, ColumnTrait, Condition, ConnectionTrait, DbErr, EntityTrait,
	QueryFilter, QueryOrder, QuerySelect, QueryTrait, RelationTrait,
};
use slug::slugify;

pub async fn create_or_find<'s, 'a, C>(
	artist_name: &'s str,
	connection: &'a C,
) -> Result<ArtistResponse, DbErr>
where
	C: ConnectionTrait,
{
	let artist_slug = slugify(artist_name.to_owned());
	let new_artist = artist::ActiveModel {
		name: Set(artist_name.to_owned()),
		unique_slug: Set(artist_slug.clone()),
		..Default::default()
	};

	let _ = artist::Entity::insert(new_artist.clone())
		.on_conflict(
			sea_query::OnConflict::column(artist::Column::UniqueSlug)
				.do_nothing()
				.to_owned(),
		)
		.exec(connection)
		.await;

	artist::Entity::find()
		.filter(artist::Column::UniqueSlug.eq(artist_slug))
		.one(connection)
		.await?
		.map_or(Err(DbErr::RecordNotFound("".to_string())), |r| Ok(r.into()))
}

pub async fn find<'s, 'a, C>(
	slug_or_uuid: &'s str,
	connection: &'a C,
) -> Result<ArtistWithPosterResponse, DbErr>
where
	C: ConnectionTrait,
{
	let uuid_parse_result = Uuid::parse_str(slug_or_uuid);
	let mut query = artist::Entity::find();

	if let Ok(uuid) = uuid_parse_result {
		query = query.filter(artist::Column::Id.eq(uuid));
	} else {
		query = query.filter(artist::Column::UniqueSlug.eq(slug_or_uuid));
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

pub async fn find_many<'a, C>(
	filters: &ArtistFilter,
	sort: Option<Sort<ArtistSort>>,
	pagination: &Pagination,
	connection: &'a C,
) -> Result<Vec<ArtistWithPosterResponse>, DbErr>
where
	C: ConnectionTrait,
{
	let mut query = artist::Entity::find()
		.apply_if(filters.package, |q, package_uuid| {
			q.join(sea_orm::JoinType::RightJoin, artist::Relation::Extra.def())
				.join(
					sea_orm::JoinType::RightJoin,
					artist::Relation::Package.def(),
				)
				.join(sea_orm::JoinType::RightJoin, artist::Relation::Movie.def())
				.filter(
					Condition::any()
						.add(extra::Column::PackageId.eq(package_uuid))
						.add(movie::Column::PackageId.eq(package_uuid))
						.add(package::Column::Id.eq(package_uuid)),
				)
				.distinct()
		})
		.offset(pagination.skip)
		.limit(pagination.take)
		.find_also_related(image::Entity);

	if let Some(s) = sort {
		query = match s.sort_by {
			ArtistSort::Name => query.order_by(artist::Column::UniqueSlug, s.order.into()),
			ArtistSort::AddDate => query.order_by(artist::Column::RegisteredAt, s.order.into()),
		}
	}

	query.all(connection).await.map(|items| {
		items
			.into_iter()
			.map(|(artist, image)| ArtistWithPosterResponse {
				artist: artist.into(),
				poster: image.map(|i| i.into()),
			})
			.collect()
	})
}
