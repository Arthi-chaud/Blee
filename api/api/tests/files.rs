#[macro_use]
mod common;

#[cfg(test)]
mod test_files {

	use std::env;

	use crate::common::*;
	use api::dto::file::UpdateFile;
	use rocket::http::{ContentType, Header, Status};
	use urlencoding::encode;

	#[test]
	// Test /files?path=
	// Expect all files in a package
	fn test_files_path_filter() {
		let client = test_client().lock().unwrap();

		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let parent_package = &data.as_ref().unwrap().package_a;
				let response = client
					.get(format!(
						"/files?path={}",
						encode("/videos/Madonna/The Video Collection 93:99")
					))
					.dispatch();
				assert_eq!(response.status(), Status::Ok);
				let value = response_json_value(response);
				let items = value
					.as_object()
					.unwrap()
					.get("items")
					.unwrap()
					.as_array()
					.unwrap();
				assert_eq!(items.len(), parent_package.extras.len());
				for ExtraDummy(_, file) in &parent_package.extras {
					assert!(file
						.path
						.starts_with("/videos/Madonna/The Video Collection 93:99"));
				}
			})
			.is_ok());
	}

	#[test]
	// Test /files?path=
	// Expect a single file
	fn test_files_path_filter2() {
		let client = test_client().lock().unwrap();

		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let parent_package = &data.as_ref().unwrap().package_b;
				let expected_file = &parent_package.movies.first().unwrap().2;
				let response = client
					.get(format!(
						"/files?path={}",
						encode("/videos/MIKA/Live in Cartoon Motion/Live in Cartoon Motion.mkv")
					))
					.dispatch();
				assert_eq!(response.status(), Status::Ok);
				let value = response_json_value(response);
				let items = value
					.as_object()
					.unwrap()
					.get("items")
					.unwrap()
					.as_array()
					.unwrap();
				assert_eq!(items.len(), 1);
				assert_eq!(
					items
						.first()
						.unwrap()
						.as_object()
						.unwrap()
						.get("id")
						.unwrap()
						.as_str()
						.unwrap(),
					expected_file.id.to_string()
				);
			})
			.is_ok());
	}

	#[test]
	// Test /files/<path>
	// Expect a single file
	fn test_file_by_path() {
		let client = test_client().lock().unwrap();

		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let parent_package = &data.as_ref().unwrap().package_b;
				let expected_file = &parent_package.movies.first().unwrap().2;
				let response = client
					.get(format!(
						"/files/{}",
						encode("/videos/MIKA/Live in Cartoon Motion/Live in Cartoon Motion.mkv")
					))
					.dispatch();
				assert_eq!(response.status(), Status::Ok);
				let value = response_json_value(response);
				assert_eq!(
					value
						.as_object()
						.unwrap()
						.get("id")
						.unwrap()
						.as_str()
						.unwrap(),
					expected_file.id.to_string()
				);
			})
			.is_ok());
	}

	#[test]
	fn test_files_path_filter_and_sort() {
		let client = test_client().lock().unwrap();

		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let expected_tracks = &data.as_ref().unwrap().package_a.extras;
				let response = client
					.get(format!(
						"/files?path={}&sort=path",
						encode("/videos/Madonna/The Video Collection 93:99")
					))
					.dispatch();
				assert_eq!(response.status(), Status::Ok);
				let value = response_json_value(response);
				let items = value
					.as_object()
					.unwrap()
					.get("items")
					.unwrap()
					.as_array()
					.unwrap();
				assert_eq!(items.len(), expected_tracks.len());
				for (pos, _) in expected_tracks.iter().enumerate() {
					assert!(items
						.get(pos)
						.unwrap()
						.as_object()
						.unwrap()
						.get("path")
						.unwrap()
						.as_str()
						.unwrap()
						.starts_with(
							format!("/videos/Madonna/The Video Collection 93:99/{:02} ", pos + 1)
								.as_str()
						));
				}
			})
			.is_ok());
	}

	#[test]
	// Test /files/<path>
	// Expect a single file
	fn test_file_update_path() {
		let client = test_client().lock().unwrap();

		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let parent_package = &data.as_ref().unwrap().package_b;
				let new_path = "/videos/MIKA - Live in Cartoon Motion.mkv";
				let expected_file = &parent_package.movies.first().unwrap().2;
				let dto = UpdateFile {
					path: Some(new_path.to_string()),
				};
				let response = client
					.post(format!("/files/{}", expected_file.id))
					.header(Header::new(
						"X-API-Key",
						env::var("SCANNER_API_KEY").unwrap(),
					))
					.header(ContentType::JSON)
					.body(serde_json::to_value(dto).unwrap().to_string())
					.dispatch();
				assert_eq!(response.status(), Status::Ok);
				let value = response_json_value(response);
				assert_eq!(
					value
						.as_object()
						.unwrap()
						.get("id")
						.unwrap()
						.as_str()
						.unwrap(),
					expected_file.id.to_string()
				);
				assert_eq!(
					value
						.as_object()
						.unwrap()
						.get("path")
						.unwrap()
						.as_str()
						.unwrap(),
					new_path
				);
			})
			.is_ok());
		// teardown
		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let parent_package = &data.as_ref().unwrap().package_b;
				let old_path = "/videos/MIKA/Live in Cartoon Motion/Live in Cartoon Motion.mkv";
				let expected_file = &parent_package.movies.first().unwrap().2;
				let dto = UpdateFile {
					path: Some(old_path.to_string()),
				};
				let response = client
					.post(format!("/files/{}", expected_file.id))
					.header(Header::new(
						"X-API-Key",
						env::var("SCANNER_API_KEY").unwrap(),
					))
					.header(ContentType::JSON)
					.body(serde_json::to_value(dto).unwrap().to_string())
					.dispatch();
				assert_eq!(response.status(), Status::Ok);
			})
			.is_ok());
	}
}
