use diesel::prelude::*;
use diesel::PgConnection;
use domain::models::artist::Artist;
use rocket::serde::uuid::Uuid;
use slug::slugify;

pub fn create_or_find<'s>(
	artist_name: &'s str,
	connection: &mut PgConnection,
) -> Result<Artist, diesel::result::Error> {
	use domain::schema::artists::dsl::*;
	let artist_slug = slugify(artist_name.to_owned());

	diesel::insert_into(artists)
		.values((name.eq(artist_name), slug.eq(&artist_slug)))
		.on_conflict(slug)
		.do_nothing()
		.execute(connection)?;

	artists
		.filter(slug.eq(&artist_slug))
		.select(Artist::as_select())
		.first(connection)
}

pub fn find<'s>(
	slug_or_uuid: &'s str,
	connection: &mut PgConnection,
) -> Result<Artist, diesel::result::Error> {
	use domain::schema::artists::dsl::*;
	let uuid_parse_result = Uuid::parse_str(slug_or_uuid);

	match uuid_parse_result {
		Ok(uuid) => artists
			.filter(id.eq(uuid))
			.select(Artist::as_select())
			.first(connection),
		_ => artists
			.filter(slug.eq(slug_or_uuid))
			.select(Artist::as_select())
			.first(connection),
	}
}

pub fn find_by_uuid<'s>(
	uuid: &Uuid,
	connection: &mut PgConnection,
) -> Result<Artist, diesel::result::Error> {
	use domain::schema::artists::dsl::*;

	artists
		.filter(id.eq(uuid))
		.select(Artist::as_select())
		.first(connection)
}
