mod common;

#[cfg(test)]
mod test_packages {

	use crate::common::*;
	use rocket::http::Status;

	#[test]
	// Test /packages?artist=
	fn test_package_filter() {
		let client = test_client().lock().unwrap();

		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let filtering_artist = &data.as_ref().unwrap().package_a.artist.as_ref().unwrap();
				let response = client
					.get(format!("/packages?artist={}", filtering_artist.id))
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
				assert_eq!(items.len(), 2);
				for item in items {
					assert_eq!(
						item.as_object()
							.unwrap()
							.get("artist_id")
							.unwrap()
							.as_str()
							.unwrap(),
						filtering_artist.id.to_string()
					);
				}
			})
			.is_ok());
	}

	#[test]
	// Test /packages?sort=name
	fn test_package_sort_name() {
		let client = test_client().lock().unwrap();

		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let response = client.get(format!("/packages?sort=name")).dispatch();
				let first_expected_package = &data.as_ref().unwrap().package_c.package.id;
				let second_expected_package = &data.as_ref().unwrap().package_b.package.id;
				let third_expected_package = &data.as_ref().unwrap().package_a.package.id;
				assert_eq!(response.status(), Status::Ok);
				let value = response_json_value(response);
				let items = value
					.as_object()
					.unwrap()
					.get("items")
					.unwrap()
					.as_array()
					.unwrap();
				let item = items.first().unwrap().as_object().unwrap();
				assert_eq!(
					item.get("id").unwrap().as_str().unwrap(),
					first_expected_package.to_string()
				);
				let item = items.get(1).unwrap().as_object().unwrap();
				assert_eq!(
					item.get("id").unwrap().as_str().unwrap(),
					second_expected_package.to_string()
				);
				let item = items.get(2).unwrap().as_object().unwrap();
				assert_eq!(
					item.get("id").unwrap().as_str().unwrap(),
					third_expected_package.to_string()
				);
			})
			.is_ok());
	}

	#[test]
	// Test /packages?sort=release_date
	fn test_package_sort_release_date() {
		let client = test_client().lock().unwrap();

		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let response = client
					.get(format!("/packages?sort=release_date&order=desc"))
					.dispatch();
				let first_expected_package = &data.as_ref().unwrap().package_b.package.id;
				let second_expected_package = &data.as_ref().unwrap().package_c.package.id;
				let third_expected_package = &data.as_ref().unwrap().package_a.package.id;
				assert_eq!(response.status(), Status::Ok);
				let value = response_json_value(response);
				let items = value
					.as_object()
					.unwrap()
					.get("items")
					.unwrap()
					.as_array()
					.unwrap();
				let item = items.first().unwrap().as_object().unwrap();
				assert_eq!(
					item.get("id").unwrap().as_str().unwrap(),
					first_expected_package.to_string()
				);
				let item = items.get(1).unwrap().as_object().unwrap();
				assert_eq!(
					item.get("id").unwrap().as_str().unwrap(),
					second_expected_package.to_string()
				);
				let item = items.get(2).unwrap().as_object().unwrap();
				assert_eq!(
					item.get("id").unwrap().as_str().unwrap(),
					third_expected_package.to_string()
				);
			})
			.is_ok());
	}

	#[test]
	// Test /packages?sort=artist_name
	fn test_package_sort_artist_name() {
		let client = test_client().lock().unwrap();

		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let response = client.get(format!("/packages?sort=artist_name")).dispatch();
				let first_expected_package = &data.as_ref().unwrap().package_c.package.id;
				let second_expected_package = &data.as_ref().unwrap().package_a.package.id;
				let third_expected_package = &data.as_ref().unwrap().package_b.package.id;
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
				let item = items.first().unwrap().as_object().unwrap();
				assert_eq!(
					item.get("id").unwrap().as_str().unwrap(),
					first_expected_package.to_string()
				);
				let item = items.get(1).unwrap().as_object().unwrap();
				assert_eq!(
					item.get("id").unwrap().as_str().unwrap(),
					second_expected_package.to_string()
				);
				let item = items.get(2).unwrap().as_object().unwrap();
				assert_eq!(
					item.get("id").unwrap().as_str().unwrap(),
					third_expected_package.to_string()
				);
			})
			.is_ok());
	}
}
