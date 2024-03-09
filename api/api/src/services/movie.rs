use diesel::{prelude::*, PgConnection};
use domain::models::{
	artist::Artist,
	image::Image,
	movie::{Movie, MovieType},
};
use rocket::serde::uuid::Uuid;
use slug::slugify;

use crate::responses::movie::MovieResponse;

pub fn create<'s>(
	movie_name: &'s str,
	movie_type: MovieType,
	artist: &Artist,
	package_uuid: &Uuid,
	file_uuid: &Uuid,
	connection: &mut PgConnection,
) -> Result<Movie, diesel::result::Error> {
	use domain::schema::movies::dsl::*;
	let creation_dto = (
		name.eq(movie_name),
		slug.eq(slugify(format!("{} {}", artist.name, movie_name))),
		package_id.eq(package_uuid),
		type_.eq(movie_type),
		artist_id.eq(artist.id),
		file_id.eq(file_uuid),
	);

	diesel::insert_into(movies)
		.values(&creation_dto)
		// .select(Extra::as_select())
		.get_result::<Movie>(connection)
}

pub fn find(
	slug_or_uuid: &String,
	connection: &mut PgConnection,
) -> Result<MovieResponse, diesel::result::Error> {
	use domain::schema::images::dsl::images;
	use domain::schema::movies::dsl::*;
	let uuid_parse_result = Uuid::parse_str(slug_or_uuid);
	let mut query = movies.left_join(images).into_boxed();

	if let Ok(uuid) = uuid_parse_result {
		query = query.filter(id.eq(uuid));
	} else {
		query = query.filter(slug.eq(slug_or_uuid))
	}

	let (movie, poster) = query
		.select((Movie::as_select(), Option::<Image>::as_select()))
		.first(connection)?;

	Ok(MovieResponse {
		movie,
		poster,
		artist: None,
		package: None,
		file: None,
	})
}
