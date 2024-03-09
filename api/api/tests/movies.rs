mod common;

#[cfg(test)]
mod test_movie {

	use std::vec;

	use crate::common::*;
	use api::dto::{chapter::NewChapter, file::NewFile, movie::NewMovie};
	use chrono::NaiveDate;
	use domain::models::{chapter::Chapter, movie::MovieType};
	use rocket::http::{ContentType, Status};

	#[test]
	/// Test POST `/movies`
	fn test_movies() {
		let client = test_client().lock().unwrap();
		let dto = NewMovie {
			artist_name: "Taylor Swift".to_owned(),
			movie_name: "Miss Americana".to_owned(),
			package_artist_name: Some("Taylor Swift".to_owned()),
			package_name: "Miss Americana".to_owned(),
			package_release_date: NaiveDate::from_ymd_opt(2019, 02, 01),
			movie_type: MovieType::Documentary,
			file: NewFile {
				path: "/data/Taylor Swift/Miss Americana.mp4".to_owned(),
				size: 160000,
				quality: domain::models::video_quality::VideoQuality::P1080,
			},
			chapters: vec![
				NewChapter {
					name: "Part 1".to_owned(),
					start_timestamp: 60,
					end_timestamp: 120,
					types: vec![],
				},
				NewChapter {
					name: "Part 2".to_owned(),
					start_timestamp: 120,
					end_timestamp: 180,
					types: vec![],
				},
			],
		};
		let response = client
			.post("/movies")
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
		let movie_id = value_response.get("movie_id").unwrap().as_str().unwrap();

		assert_eq!(package_artist_id, artist_id);

		// Check Movie properties
		let movie_response = client.get(format!("/movies/{}", movie_id)).dispatch();
		assert_eq!(movie_response.status(), Status::Ok);
		let movie_value = response_json_value(movie_response);
		let name = movie_value.get("name").unwrap().as_str().unwrap();
		assert_eq!(name, "Miss Americana");

		// TODO
		// Check Chapters properties

		// Check Artist Exists
		let artist_response = client.get(format!("/artists/{}", artist_id)).dispatch();
		assert_eq!(artist_response.status(), Status::Ok);
		let artist_value = response_json_value(artist_response);
		let name = artist_value.get("name").unwrap().as_str().unwrap();
		assert_eq!(name, "Taylor Swift");

		// Check Package Exists
		let package_response = client.get(format!("/packages/{}", package_id)).dispatch();
		assert_eq!(package_response.status(), Status::Ok);
		let package_value = response_json_value(package_response);
		let name = package_value.get("name").unwrap().as_str().unwrap();
		assert_eq!(name, "Miss Americana");
		let package_artist_id = package_value.get("artist_id").unwrap().as_str().unwrap();
		assert_eq!(package_artist_id, artist_id);

		// Check File
		let file_response = client.get(format!("/files/{}", file_id)).dispatch();
		assert_eq!(file_response.status(), Status::Ok);
		let file_value = response_json_value(file_response);
		let path = file_value.get("path").unwrap().as_str().unwrap();
		assert_eq!(path, "/data/Taylor Swift/Miss Americana.mp4");
		let quality = file_value.get("quality").unwrap().as_str().unwrap();
		assert_eq!(quality, "1080p");
	}
}
