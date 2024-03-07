mod common;

#[cfg(test)]
mod test_extra {

	use crate::common::*;
	use api::dto::{extra::NewExtra, file::NewFile};
	use chrono::NaiveDate;
	use domain::models::extra::ExtraType;
	use rocket::http::{ContentType, Status};

	#[test]
	/// Test POST `/extras`
	fn test_extras() {
		let client = test_client().lock().unwrap();
		let dto = NewExtra {
			artist_name: "Madonna".to_owned(),
			extra_name: "Secret (Music Video)".to_owned(),
			package_artist_name: Some("Madonna".to_owned()),
			package_name: "The Video Collection 93:99".to_owned(),
			disc_index: Some(1),
			track_index: Some(8),
			package_release_date: NaiveDate::from_ymd_opt(1999, 01, 02),
			types: vec![ExtraType::MusicVideo],
			file: NewFile {
				path: "/data/Madonna/The Video Collection 93_99/1-08 Secret (Music Video).mp4"
					.to_owned(),
				size: 160000,
				quality: domain::models::video_quality::VideoQuality::P420,
			},
		};
		let response = client
			.post("/extras")
			.header(ContentType::JSON)
			.body(serde_json::to_value(dto).unwrap().to_string())
			.dispatch();
		assert_eq!(response.status(), Status::Created);
		let value_response = response_json_value(response);
		let package_id = value_response.get("package_id").unwrap().as_str().unwrap();
		let package_artist_id = value_response
			.get("package_artist_id")
			.unwrap()
			.as_str()
			.unwrap();
		let artist_id = value_response.get("artist_id").unwrap().as_str().unwrap();
		let file_id = value_response.get("file_id").unwrap().as_str().unwrap();
		let extra_id = value_response.get("extra_id").unwrap().as_str().unwrap();

		assert_eq!(package_artist_id, artist_id);

		// TODO
		// Check Extra properties

		// Check Artist Exists
		let artist_response = client.get(format!("/artists/{}", artist_id)).dispatch();
		assert_eq!(artist_response.status(), Status::Ok);
		let artist_value = response_json_value(artist_response);
		let name = artist_value.get("name").unwrap().as_str().unwrap();
		assert_eq!(name, "Madonna");

		// Check Package Exists
		// Check Package Artist Exists
		// Check File
	}
}
