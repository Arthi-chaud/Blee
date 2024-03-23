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
				assert_eq!(items.len(), 1);
				let item = items.first().unwrap().as_object().unwrap();
				assert_eq!(
					item.get("artist_id").unwrap().as_str().unwrap(),
					filtering_artist.id.to_string()
				);
			})
			.is_ok());
	}
}
