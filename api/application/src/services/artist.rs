use diesel::prelude::*;
use diesel::PgConnection;
use domain::models::artist::Artist;
use slug::slugify;

pub fn create_or_find<'s>(
    artist_name: &'s str,
    connection: &mut PgConnection,
) -> Result<Artist, diesel::result::Error> {
    use domain::schema::artists::dsl::*;

    let _ = diesel::insert_into(artists)
        .values((name.eq(artist_name), slug.eq(artist_name)))
        .on_conflict(slug)
        .do_nothing()
        .execute(connection);

    artists
        .filter(slug.eq(slugify(artist_name.to_owned())))
        .select(Artist::as_select())
        .first(connection)
}
