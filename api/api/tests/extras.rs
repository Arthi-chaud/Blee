mod common;

#[cfg(test)]
mod test_extra {

	use std::env;

	use crate::common::*;
	use api::dto::{
		extra::{ExtraType, NewExtra},
		file::{NewFile, VideoQuality},
	};
	use chrono::NaiveDate;
	use rocket::http::{ContentType, Header, Status};

	#[test]
	/// Test POST `/extras`
	fn test_new_extra() {
		let client = test_client().lock().unwrap();
		let dto = NewExtra {
			artist_name: "Depeche Mode".to_owned(),
			extra_name: "Enjoy the Silence (Music Video)".to_owned(),
			package_artist_name: Some("Depeche Mode".to_owned()),
			package_name: "The Best Of".to_owned(),
			disc_index: Some(1),
			track_index: Some(8),
			package_release_date: NaiveDate::from_ymd_opt(2007, 01, 02),
			types: vec![ExtraType::MusicVideo],
			file: NewFile {
				path: "/data/Depeche Mode/The Best Of/1-08 Enjoy the Silence (Music Video).mp4"
					.to_owned(),
				size: 160000,
				quality: VideoQuality::P480,
			},
		};
		let response = client
			.post("/extras")
			.header(Header::new(
				"X-API-Key",
				env::var("SCANNER_API_KEY").unwrap(),
			))
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

		// Check Extra
		let extra_response = client.get(format!("/extras/{}", extra_id)).dispatch();
		assert_eq!(extra_response.status(), Status::Ok);
		let extra_value = response_json_value(extra_response);
		let name = extra_value.get("name").unwrap().as_str().unwrap();
		assert_eq!(name, "Enjoy the Silence (Music Video)");
		let disc_index = extra_value.get("disc_index").unwrap().as_i64().unwrap();
		assert_eq!(disc_index, 1);
		let track_index = extra_value.get("track_index").unwrap().as_i64().unwrap();
		assert_eq!(track_index, 8);
		let extra_artist_id = extra_value.get("artist_id").unwrap().as_str().unwrap();
		assert_eq!(extra_artist_id, artist_id);
		let extra_file_id = extra_value.get("file_id").unwrap().as_str().unwrap();
		assert_eq!(extra_file_id, file_id);

		// Check Artist Exists
		let artist_response = client.get(format!("/artists/{}", artist_id)).dispatch();
		assert_eq!(artist_response.status(), Status::Ok);
		let artist_value = response_json_value(artist_response);
		let name = artist_value.get("name").unwrap().as_str().unwrap();
		assert_eq!(name, "Depeche Mode");

		// Check Package Exists
		let package_response = client.get(format!("/packages/{}", package_id)).dispatch();
		assert_eq!(package_response.status(), Status::Ok);
		let package_value = response_json_value(package_response);
		let name = package_value.get("name").unwrap().as_str().unwrap();
		assert_eq!(name, "The Best Of");
		let package_artist_id = package_value.get("artist_id").unwrap().as_str().unwrap();
		assert_eq!(package_artist_id, artist_id);

		// Check File
		let file_response = client.get(format!("/files/{}", file_id)).dispatch();
		assert_eq!(file_response.status(), Status::Ok);
		let file_value = response_json_value(file_response);
		let path = file_value.get("path").unwrap().as_str().unwrap();
		assert_eq!(
			path,
			"/data/Depeche Mode/The Best Of/1-08 Enjoy the Silence (Music Video).mp4"
		);
		let quality = file_value.get("quality").unwrap().as_str().unwrap();
		assert_eq!(quality, "480p");
	}

	#[test]
	fn test_new_extra_failure_already_exists() {
		let client = test_client().lock().unwrap();
		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let source_package = &data.as_ref().unwrap().package_a;
				let dto = NewExtra {
					artist_name: source_package.artist.as_ref().unwrap().name.clone(),
					extra_name: source_package.extras.first().unwrap().0.name.clone(),
					package_artist_name: Some("Madonna".to_owned()),
					package_name: "The Video Collection 93:99".to_owned(),
					disc_index: Some(1),
					track_index: Some(8),
					package_release_date: NaiveDate::from_ymd_opt(1999, 01, 02),
					types: vec![ExtraType::MusicVideo],
					file: NewFile {
						path: source_package
							.extras
							.first()
							.unwrap()
							.1
							.path
							.clone()
							.to_owned(),
						size: 160000,
						quality: VideoQuality::P480,
					},
				};
				let response = client
					.post("/extras")
					.header(Header::new(
						"X-API-Key",
						env::var("SCANNER_API_KEY").unwrap(),
					))
					.header(ContentType::JSON)
					.body(serde_json::to_value(dto).unwrap().to_string())
					.dispatch();
				assert_eq!(response.status(), Status::Conflict);
			})
			.is_ok());
	}

	#[test]
	fn test_new_extra_failure_empty_type_list() {
		let client = test_client().lock().unwrap();
		let dto = NewExtra {
			artist_name: "Madonna".to_owned(),
			extra_name: "Secret (Music Video)".to_owned(),
			package_artist_name: Some("Madonna".to_owned()),
			package_name: "The Video Collection 93:99".to_owned(),
			disc_index: Some(1),
			track_index: Some(8),
			package_release_date: NaiveDate::from_ymd_opt(1999, 01, 02),
			types: vec![],
			file: NewFile {
				path: "Secret (Music Video).mp4".to_owned(),
				size: 160000,
				quality: VideoQuality::P480,
			},
		};
		let response = client
			.post("/extras")
			.header(Header::new(
				"X-API-Key",
				env::var("SCANNER_API_KEY").unwrap(),
			))
			.header(ContentType::JSON)
			.body(serde_json::to_value(dto).unwrap().to_string())
			.dispatch();
		assert_eq!(response.status(), Status::BadRequest);
	}
}
