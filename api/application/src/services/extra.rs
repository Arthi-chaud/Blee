use diesel::{prelude::*, PgConnection};
use domain::models::extra::{Extra, ExtraType};
use rocket::{serde::uuid::Uuid};
use slug::slugify;

pub fn create<'s>(
    extra_name: &'s str,
    disc: Option<i16>,
    track: Option<i16>,
    types: &Vec<ExtraType>,
    package_uuid: &Uuid,
    artist_uuid: &Uuid,
    file_uuid: &Uuid,
    connection: &mut PgConnection,
) -> Result<Extra, diesel::result::Error> {
    use domain::schema::extras::dsl::*;
    let option_types: Vec<Option<ExtraType>> = types.iter().map(|e| Some(e.clone())).collect();

    let creation_dto = (
        name.eq(extra_name),
        slug.eq(slugify(extra_name)),
        package_id.eq(package_uuid),
        artist_id.eq(artist_uuid),
        file_id.eq(file_uuid),
        disc_index.eq(disc),
        track_index.eq(track),
        type_.eq(option_types)
	);

    diesel::insert_into(extras)
        .values(&creation_dto)
		// .select(Extra::as_select())
        .get_result::<Extra>(connection)
}
