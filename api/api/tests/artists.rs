mod common;

#[cfg(test)]
mod test_artist {
	use std::{
		fs::{self, remove_dir_all},
		path::PathBuf,
	};

	use regex::Regex;
	use rocket::http::Status;

	use crate::common::*;

	#[test]
	/// Test `/artists/:slug_or_id`
	fn test_artist() {
		let client = test_client().lock().unwrap();

		let response_using_slug = client.get("/artists/aretha-franklin").dispatch();
		assert_eq!(response_using_slug.status(), Status::Ok);
		let value = response_json_value(response_using_slug);

		let name = value.get("name").unwrap().as_str();
		let slug = value.get("slug").unwrap().as_str();
		let poster_id = value.get("poster_id").unwrap().as_str();
		let poster = value.get("poster").unwrap().as_object();
		let id = value.get("id").unwrap().as_str();

		// Check retuened values
		assert_eq!(name, Some("Aretha Franklin"));
		assert_eq!(slug, Some("aretha-franklin"));
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

		let bad_slug = client.get("/artists/aretha-frankli").dispatch();
		let bad_uuid = client.get("/artists/00000000").dispatch();
		assert_eq!(bad_slug.status(), Status::NotFound);
		assert_eq!(bad_uuid.status(), Status::NotFound);
	}

	#[test]
	/// Test `/artists/:slug_or_id/poster`
	fn test_artist_poster() {
		let client = test_client().lock().unwrap();

		let response = client
			.post("/artists/aretha-franklin/poster")
			.body(fs::read("./tests/assets/poster.png").unwrap())
			.dispatch();
		assert_eq!(response.status(), Status::Created);
		let poster = response_json_value(response);
		let poster_id = poster.get("id").unwrap().as_str().unwrap();
		let poster_colors = poster.get("colors").unwrap().as_array().unwrap();
		let poster_blurhash = poster.get("blurhash").unwrap().as_str().unwrap();
		let poster_ratio = poster.get("aspect_ratio").unwrap().as_f64().unwrap();

		assert_eq!(poster_blurhash, "T02Yt$WBx]x]ofITRiay4mt8axWA");
		assert_ne!(poster_colors.len(), 0);
		assert_eq!(poster_ratio, 1 as f64);
		for color in poster_colors {
			println!("{}", color.as_str().unwrap());
			assert!(Regex::new(r"#(\d|[a-z]){6}")
				.unwrap()
				.is_match(color.as_str().unwrap()))
		}

		let poster_dir_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
			.as_path()
			.join(&std::env::var("CONFIG_DIR").unwrap().to_string())
			.join(poster_id);
		println!("{}", poster_dir_path.to_str().unwrap());

		assert!(poster_dir_path.join("image.webp").exists());

		// Teardown
		let _ = remove_dir_all(poster_dir_path);
	}
	#[test]
	/// Test `/artists/:slug_or_id/poster` failures
	fn test_artist_poster_failures() {
		let client = test_client().lock().unwrap();

		let unknown_artist = client.post("/artists/aretha-frankli/poster").dispatch();
		assert_eq!(unknown_artist.status(), Status::NotFound);

		let missing_attached_file = client.post("/artists/aretha-franklin/poster").dispatch();
		assert_eq!(missing_attached_file.status(), Status::BadRequest);
		let not_an_image = client
			.post("/artists/aretha-franklin/poster")
			.body(
				fs::read(
					PathBuf::from(env!("CARGO_MANIFEST_DIR"))
						.as_path()
						.join("Cargo.toml"),
				)
				.unwrap(),
			)
			.dispatch();
		assert_eq!(not_an_image.status(), Status::BadRequest);
	}
}
