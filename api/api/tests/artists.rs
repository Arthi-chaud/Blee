#[macro_use]
mod common;

#[cfg(test)]
mod test_artist {
	use api::database::Db;
	use entity::artist;
	use rocket::http::Status;
	use sea_orm::{ColumnTrait, EntityTrait, QueryFilter, Set};
	use sea_orm_rocket::Database;

	use crate::common::*;

	#[test]
	/// Test `/artists/:slug_or_id`
	fn test_artist() {
		let client = test_client().lock().unwrap();

		let response_using_slug = client.get("/artists/madonna").dispatch();
		assert_eq!(response_using_slug.status(), Status::Ok);
		let value = response_json_value(response_using_slug);

		let name = value.get("name").unwrap().as_str();
		let slug = value.get("slug").unwrap().as_str();
		let poster_id = value.get("poster_id").unwrap().as_str();
		let poster = value.get("poster").unwrap().as_object();
		let id = value.get("id").unwrap().as_str();

		// Check retuened values
		assert_eq!(name, Some("Madonna"));
		assert_eq!(slug, Some("madonna"));
		assert_eq!(poster_id, None);
		assert_eq!(poster, None);
		assert_ne!(id, None);

		let response_using_uuid = client.get("/artists/".to_owned() + id.unwrap()).dispatch();
		assert_eq!(value, response_json_value(response_using_uuid));
	}

	#[test]
	/// Expect `/artists/:slug_or_id` to fail
	fn test_artist_failure() {
		let client = test_client().lock().unwrap();

		let bad_slug = client.get("/artists/madonn").dispatch();
		let bad_uuid = client.get("/artists/00000000").dispatch();
		assert_eq!(bad_slug.status(), Status::NotFound);
		assert_eq!(bad_uuid.status(), Status::NotFound);
	}

	#[test]
	/// Test `/artists/:slug_or_id/poster` failures
	fn test_artist_poster_failures() {
		let client = test_client().lock().unwrap();

		let unknown_artist = client.post("/artists/madonn/poster").dispatch();
		assert_eq!(unknown_artist.status(), Status::NotFound);
	}

	#[test]
	/// Test `/artists` Pagination
	fn test_artist_pagination() {
		let client = test_client().lock().unwrap();
		let conn = &Db::fetch(client.rocket()).unwrap().conn;
		let _ = aw!(
			artist::Entity::insert_many((1..10).map(|i| artist::ActiveModel {
				slug: Set(format!("artist-{}", i)),
				name: Set(format!("Artist {}", i)),
				..Default::default()
			}))
			.exec_without_returning(conn)
		);

		let response_first_page = client.get("/artists?page_size=5").dispatch();
		assert_eq!(response_first_page.status(), Status::Ok);
		let value = response_json_value(response_first_page);
		let items = value.get("items").unwrap().as_array().unwrap();
		let next = value
			.get("metadata")
			.unwrap()
			.as_object()
			.unwrap()
			.get("next")
			.unwrap()
			.as_str()
			.unwrap();
		assert_eq!(items.len(), 5);
		let last_item = items.last().unwrap().as_object().unwrap();
		let last_item_id = last_item.get("id").unwrap().as_str().unwrap();
		assert_eq!(
			next,
			format!("/artists?page_size=5&after_id={}", last_item_id)
		);

		//teardown
		aw!(artist::Entity::delete_many()
			.filter(artist::Column::Slug.starts_with("artist-"))
			.exec(conn));
	}

	#[test]
	// Test /artists?package=
	fn test_artist_filtered_by_package() {
		let client = test_client().lock().unwrap();

		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let filtering_package = &data.as_ref().unwrap().package_a.package.id;
				let expected_artist = &data.as_ref().unwrap().package_a.package.artist_id.unwrap();
				let response = client
					.get(format!("/artists?package={}", filtering_package))
					.dispatch();
				assert_eq!(response.status(), Status::Ok);
				let value = response_json_value(response);
				println!("{:?}", value);
				let items = value
					.as_object()
					.unwrap()
					.get("items")
					.unwrap()
					.as_array()
					.unwrap();
				assert_eq!(items.len(), 1);
				let item = items.first().unwrap().as_object().unwrap();
				assert_eq!(
					item.get("id").unwrap().as_str().unwrap(),
					expected_artist.to_string()
				);
			})
			.is_ok());
	}
}
