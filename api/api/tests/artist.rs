mod common;

#[cfg(test)]
mod test_artist {
	use rocket::http::Status;

	use crate::common::*;
	#[test]
	/// Test `/artist/:slug`
	fn test_artist_slug() {
		let client = test_client().lock().unwrap();

		let response = client.get("/artists/aretha-franklin").dispatch();
		assert_eq!(response.status(), Status::Ok);
		let value = response_json_value(response);

		let name = value.get("name").unwrap().as_str();
		let slug = value.get("slug").unwrap().as_str();
		let poster_id = value.get("poster_id").unwrap().as_str();
		let poster = value.get("poster").unwrap().as_object();

		assert_eq!(name, Some("Aretha Franklin"));
		assert_eq!(slug, Some("aretha-franklin"));
		assert_eq!(poster_id, None);
		assert_eq!(poster, None);
	}
}
