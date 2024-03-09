use diesel::prelude::*;
use diesel::update;
use diesel::PgConnection;
use domain::models::artist::Artist;
use domain::models::image::Image;
use rocket::serde::uuid::Uuid;
use slug::slugify;

use crate::responses::artist::ArtistResponse;

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
) -> Result<ArtistResponse, diesel::result::Error> {
	use domain::schema::artists::dsl::*;
	use domain::schema::images::dsl::images;
	let uuid_parse_result = Uuid::parse_str(slug_or_uuid);
	let mut query = artists.left_join(images).into_boxed();

	if let Ok(uuid) = uuid_parse_result {
		query = query.filter(id.eq(uuid));
	} else {
		query = query.filter(slug.eq(slug_or_uuid))
	}
	let (artist, image) = query
		.select((Artist::as_select(), Option::<Image>::as_select()))
		.first(connection)?;

	Ok(ArtistResponse {
		artist,
		poster: image,
	})
}

pub fn set_poster<'s>(
	poster_uuid: &Uuid,
	artist_uuid: &Uuid,
	connection: &mut PgConnection,
) -> Result<(), diesel::result::Error> {
	use domain::schema::artists::dsl::*;

	update(artists)
		.filter(id.eq(artist_uuid))
		.set(poster_id.eq(poster_uuid))
		.execute(connection)
		.map(|_| ())
}
