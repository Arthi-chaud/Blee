use diesel::{prelude::*, PgConnection};
use domain::models::{
	artist::Artist,
	movie::{Movie, MovieType},
};
use rocket::serde::uuid::Uuid;
use slug::slugify;

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
