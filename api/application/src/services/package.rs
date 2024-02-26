use rocket::serde::uuid::Uuid;
use ::slug::slugify;
use diesel::{prelude::*, ExpressionMethods, PgConnection};
use domain::models::{artist::Artist, package::Package};

pub fn create_or_find<'s>(
    artist: Option<Artist>,
    package_name: &'s str,
    release_date: Option<chrono::NaiveDate>,
    connection: &mut PgConnection,
) -> Result<Package, diesel::result::Error> {
    use domain::schema::packages::dsl::*;
    #[derive(Insertable)]
    #[diesel(table_name = domain::schema::packages)]
    struct NewPackage<'s> {
        name: &'s str,
        slug: &'s str,
        release_year: Option<chrono::NaiveDate>,
        artist_id: Option<Uuid>,
    }

    let artist_name = artist.clone()
        .map_or(String::from("Various Artist"), |a| a.name.clone());
    let package_slug = slugify(format!("{} {}", artist_name, package_name));
    let creation_dto = NewPackage {
        name: package_name,
        slug: &package_slug,
        release_year: release_date,
        artist_id: artist.map(|a| a.id)
    };

    diesel::insert_into(packages)
        .values(&creation_dto)
        .on_conflict(slug)
        .do_nothing()
        .execute(connection)?;


    packages
        .filter(slug.eq(package_slug))
        .select(Package::as_select())
        .first(connection)
}
