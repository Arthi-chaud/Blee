#[macro_use]
mod common;

#[cfg(test)]
mod test_images {
	use std::{
		fs::{self, remove_dir_all},
		path::PathBuf,
	};

	use regex::Regex;
	use rocket::http::Status;

	use crate::common::*;
	#[test]
	/// Test Image Upload through `/artists/:slug_or_id/poster`
	fn test_image_upload() {
		let client = test_client().lock().unwrap();

		let response = client
			.post("/artists/madonna/poster")
			.body(fs::read("./tests/assets/poster.png").unwrap())
			.dispatch();
		assert_eq!(response.status(), Status::Created);
		let poster = response_json_value(response);
		let _ = poster.get("id").unwrap().as_str().unwrap();
		let poster_colors = poster.get("colors").unwrap().as_array().unwrap();
		let poster_blurhash = poster.get("blurhash").unwrap().as_str().unwrap();
		let poster_ratio = poster.get("aspect_ratio").unwrap().as_f64().unwrap();

		assert_eq!(poster_blurhash, "T02Yt$WBx]x]ofITRiay4mt8axWA");
		assert_ne!(poster_colors.len(), 0);
		assert_eq!(poster_ratio, 1 as f64);
		for color in poster_colors {
			assert!(Regex::new(r"#(\d|[a-z]){6}")
				.unwrap()
				.is_match(color.as_str().unwrap()))
		}
	}

	#[test]
	/// Test Image Upload through `/package/:slug_or_id/banner`
	fn test_image_upload_banner() {
		let client = test_client().lock().unwrap();

		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let package_id = &data.as_ref().unwrap().package_b.package.id.to_string();
				let response = client
					.post(format!("/packages/{}/banner", package_id))
					.body(fs::read("./tests/assets/poster.png").unwrap())
					.dispatch();
				assert_eq!(response.status(), Status::Created);

				let response = client.get(format!("/packages/{}", package_id)).dispatch();
				assert_eq!(response.status(), Status::Ok);
				let package = response_json_value(response);
				let _ = package.get("poster").unwrap().as_null().unwrap();
				let poster = package.get("banner").unwrap().as_object().unwrap();
				let poster_colors = poster.get("colors").unwrap().as_array().unwrap();
				let poster_blurhash = poster.get("blurhash").unwrap().as_str().unwrap();
				let poster_ratio = poster.get("aspect_ratio").unwrap().as_f64().unwrap();

				assert_eq!(poster_blurhash, "T02Yt$WBx]x]ofITRiay4mt8axWA");
				assert_ne!(poster_colors.len(), 0);
				assert_eq!(poster_ratio, 1 as f64);
				for color in poster_colors {
					assert!(Regex::new(r"#(\d|[a-z]){6}")
						.unwrap()
						.is_match(color.as_str().unwrap()))
				}
			})
			.is_ok());
	}

	/// Test image overwrite
	#[test]
	fn test_image_upload_2() {
		let client = test_client().lock().unwrap();

		let first_response = client.get("/artists/madonna").dispatch();
		assert_eq!(first_response.status(), Status::Ok);
		let previousposter = response_json_value(first_response);
		let previousposter_id = previousposter.get("poster_id").unwrap().as_str().unwrap();
		let previousposter_dir_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
			.as_path()
			.join(&std::env::var("CONFIG_DIR").unwrap().to_string())
			.join(previousposter_id);

		let response = client
			.post("/artists/madonna/poster")
			.body(fs::read("./tests/assets/poster2.jpg").unwrap())
			.dispatch();
		assert_eq!(response.status(), Status::Created);
		let poster = response_json_value(response);
		let poster_id = poster.get("id").unwrap().as_str().unwrap();
		let poster_blurhash = poster.get("blurhash").unwrap().as_str().unwrap();
		let poster_ratio = poster.get("aspect_ratio").unwrap().as_f64().unwrap();

		assert_eq!(poster_blurhash, "T5SPX_of-;-;ofD%ayWB00%Mj[Rj");
		assert_eq!(poster_ratio, 1 as f64);

		let poster_dir_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
			.as_path()
			.join(&std::env::var("CONFIG_DIR").unwrap().to_string())
			.join(poster_id);
		assert!(poster_dir_path.join("image.webp").exists());
		assert_eq!(previousposter_dir_path.is_dir(), false);

		// Teardown
		let _ = remove_dir_all(poster_dir_path);
	}

	#[test]
	/// Test image upload failures through `/artists/:slug_or_id/poster`
	fn test_image_upload_failures() {
		let client = test_client().lock().unwrap();

		let missing_attached_file = client.post("/artists/madonna/poster").dispatch();
		assert_eq!(missing_attached_file.status(), Status::BadRequest);
		let not_an_image = client
			.post("/artists/madonna/poster")
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
