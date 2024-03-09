use ::slug::slugify;
use diesel::{prelude::*, ExpressionMethods, PgConnection};
use domain::models::{artist::Artist, image::Image, package::Package};
use rocket::serde::uuid::Uuid;

use crate::responses::package::PackageResponse;

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

	let artist_name = artist
		.clone()
		.map_or(String::from("Various Artist"), |a| a.name.clone());
	let package_slug = slugify(format!("{} {}", artist_name, package_name));
	let creation_dto = NewPackage {
		name: package_name,
		slug: &package_slug,
		release_year: release_date,
		artist_id: artist.map(|a| a.id),
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

pub fn find(
	slug_or_uuid: &String,
	connection: &mut PgConnection,
) -> Result<PackageResponse, diesel::result::Error> {
	use domain::schema::images::dsl as image_dsl;
	use domain::schema::packages::dsl::*;
	let uuid_parse_result = Uuid::parse_str(slug_or_uuid);
	let mut query = packages
		.left_join(image_dsl::images.on(image_dsl::id.nullable().eq(poster_id)))
		.into_boxed();
	if let Ok(uuid) = uuid_parse_result {
		query = query.filter(id.eq(uuid));
	} else {
		query = query.filter(slug.eq(slug_or_uuid))
	}

	let (package, poster) = query
		.select((Package::as_select(), Option::<Image>::as_select()))
		.first(connection)?;

	Ok(PackageResponse {
		package,
		poster,
		artist: None,
	})
}
