use crate::dto::{
	extra::{ExtraFilter, ExtraResponseWithRelations, ExtraSort, ExtraType},
	page::Pagination,
	sort::Sort,
};
use entity::{artist, extra, image, package, sea_orm_active_enums::ExtraTypeEnum};
use rocket::serde::uuid::Uuid;
use sea_orm::{
	sea_query::*, ColumnTrait, ConnectionTrait, DbErr, EntityTrait, JoinType, QueryFilter,
	QueryOrder, QuerySelect, QueryTrait, RelationTrait, Set,
};
use slug::slugify;

pub async fn create<'s, 'a, C>(
	extra_name: &'s str,
	disc: Option<i32>,
	track: Option<i32>,
	types: &Vec<ExtraType>,
	package_uuid: &Uuid,
	artist_uuid: &Uuid,
	file_uuid: &Uuid,
	connection: &'a C,
) -> Result<extra::Model, DbErr>
where
	C: ConnectionTrait,
{
	let creation_dto = extra::ActiveModel {
		name: Set(extra_name.to_string()),
		name_slug: Set(slugify(extra_name)),
		package_id: Set(*package_uuid),
		artist_id: Set(*artist_uuid),
		file_id: Set(*file_uuid),
		disc_index: Set(disc),
		track_index: Set(track),
		r#type: Set(types.iter().map(|e| ExtraTypeEnum::from(*e)).collect()),
		..Default::default()
	};

	extra::Entity::insert(creation_dto)
		.exec_with_returning(connection)
		.await
}

pub async fn find<'a, C>(
	uuid: &Uuid,
	connection: &'a C,
) -> Result<ExtraResponseWithRelations, DbErr>
where
	C: ConnectionTrait,
{
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

pub async fn find_many<'a, C>(
	filters: &ExtraFilter,
	sort: Option<Sort<ExtraSort>>,
	pagination: &Pagination,
	connection: &'a C,
) -> Result<Vec<ExtraResponseWithRelations>, DbErr>
where
	C: ConnectionTrait,
{
	let mut query = extra::Entity::find()
		.join_as(
			JoinType::LeftJoin,
			extra::Relation::Artist.def(),
			Alias::new("artist"),
		)
		.join_as(
			JoinType::LeftJoin,
			extra::Relation::Package.def(),
			Alias::new("package"),
		)
		.apply_if(filters.r#type, |q, r#type| {
			//TODO
			q.filter(extra::Column::Type.eq(vec![ExtraTypeEnum::from(r#type)]))
		})
		.apply_if(filters.artist, |q, artist_uuid| {
			q.filter(extra::Column::ArtistId.eq(artist_uuid))
		})
		.apply_if(filters.package, |q, package_uuid| {
			q.filter(extra::Column::PackageId.eq(package_uuid))
		})
		.offset(pagination.skip)
		.limit(pagination.take);

	if let Some(s) = sort {
		query = match s.sort_by {
			ExtraSort::Name => query.order_by(extra::Column::NameSlug, s.order.into()),
			ExtraSort::AddDate => query.order_by(extra::Column::RegisteredAt, s.order.into()),
			ExtraSort::ArtistName => query
				.order_by(
					Expr::col((Alias::new("artist"), artist::Column::UniqueSlug)),
					s.order.into(),
				)
				.order_by(extra::Column::NameSlug, sea_orm::Order::Asc),
			ExtraSort::PackageName => query
				.order_by(
					Expr::col((Alias::new("package"), package::Column::NameSlug)),
					s.order.into(),
				)
				.order_by(extra::Column::DiscIndex, sea_orm::Order::Asc)
				.order_by(extra::Column::TrackIndex, sea_orm::Order::Asc),
		}
	}
	query
		.find_also_related(image::Entity)
		.all(connection)
		.await
		.map(|items| {
			items
				.into_iter()
				.map(|(extra, image)| ExtraResponseWithRelations {
					extra: extra.into(),
					thumbnail: image.map(|i| i.into()),
					package: None,
					artist: None,
					file: None,
				})
				.collect()
		})
}
