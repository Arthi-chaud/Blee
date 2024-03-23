#[macro_use]
mod common;

#[cfg(test)]
mod test_movie {

	use std::{env, vec};

	use crate::common::*;
	use api::dto::{
		chapter::{ChapterType, NewChapter},
		file::{NewFile, VideoQuality},
		movie::{MovieType, NewMovie},
	};
	use chrono::NaiveDate;
	use rocket::http::{ContentType, Header, Status};

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
				quality: VideoQuality::P1080,
			},
			chapters: vec![
				NewChapter {
					name: "Part 1".to_owned(),
					start_timestamp: 60,
					end_timestamp: 120,
					types: vec![ChapterType::Other],
				},
				NewChapter {
					name: "Part 2".to_owned(),
					start_timestamp: 120,
					end_timestamp: 180,
					types: vec![ChapterType::Other],
				},
			],
		};
		let response = client
			.post("/movies")
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
		let movie_id = value_response.get("movie_id").unwrap().as_str().unwrap();

		assert_eq!(package_artist_id, artist_id);

		// Check Movie properties
		let movie_response = client.get(format!("/movies/{}", movie_id)).dispatch();
		assert_eq!(movie_response.status(), Status::Ok);
		let movie_value = response_json_value(movie_response);
		let name = movie_value.get("name").unwrap().as_str().unwrap();
		assert_eq!(name, "Miss Americana");

		// Check Chapters properties
		let chapter_response = client
			.get(format!("/movies/{}/chapters", movie_id))
			.dispatch();
		assert_eq!(chapter_response.status(), Status::Ok);
		let chapter_value = response_json_value(chapter_response);
		let chapters = chapter_value.as_array().unwrap();
		assert_eq!(chapters.len(), 2);
		let chapter_one = chapters.first().unwrap();
		let chapter_name = chapter_one.get("name").unwrap().as_str().unwrap();
		assert_eq!(chapter_name, "Part 1");
		let chapter_start = chapter_one.get("start_time").unwrap().as_i64().unwrap();
		assert_eq!(chapter_start, 60);
		let chapter_end = chapter_one.get("end_time").unwrap().as_i64().unwrap();
		assert_eq!(chapter_end, 120);
		let chapter_two = chapters.last().unwrap();
		let chapter_name = chapter_two.get("name").unwrap().as_str().unwrap();
		assert_eq!(chapter_name, "Part 2");
		let chapter_start = chapter_two.get("start_time").unwrap().as_i64().unwrap();
		assert_eq!(chapter_start, 120);
		let chapter_end = chapter_two.get("end_time").unwrap().as_i64().unwrap();
		assert_eq!(chapter_end, 180);

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

	#[test]
	fn test_movies_failure_empty_chapter_type() {
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
				quality: VideoQuality::P1080,
			},
			chapters: vec![NewChapter {
				name: "Part 1".to_owned(),
				start_timestamp: 60,
				end_timestamp: 120,
				types: vec![],
			}],
		};
		let response = client
			.post("/movies")
			.header(Header::new(
				"X-API-Key",
				env::var("SCANNER_API_KEY").unwrap(),
			))
			.header(ContentType::JSON)
			.body(serde_json::to_value(dto).unwrap().to_string())
			.dispatch();
		assert_eq!(response.status(), Status::BadRequest);
	}

	#[test]
	/// Test `/movies` Pagination
	/// Check Next is null on last page
	fn test_movies_pagination() {
		let client = test_client().lock().unwrap();

		let response_first_page = client.get("/movies?page_size=2").dispatch();
		assert_eq!(response_first_page.status(), Status::Ok);
		let value = response_json_value(response_first_page);
		let items = value.get("items").unwrap().as_array().unwrap();
		let last_item_id = items.last().unwrap().as_object().unwrap().get("id").unwrap().as_str().unwrap();
		assert_eq!(items.len(), 2);
		let next = value
			.get("metadata")
			.unwrap()
			.as_object()
			.unwrap()
			.get("next")
			.unwrap().as_str().unwrap();
		assert_eq!(next, format!("/movies?page_size=2&after_id={}", last_item_id));
		let response_second_page = client.get(next).dispatch();
		assert_eq!(response_second_page.status(), Status::Ok);
		let value = response_json_value(response_second_page);
		let items = value.get("items").unwrap().as_array().unwrap();
		assert_eq!(items.len(), 1);
	}

	#[test]
	// Test /movies?type=
	fn test_movies_filter_by_type() {
		let client = test_client().lock().unwrap();

		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let expected_movie = &data.as_ref().unwrap().package_b.movies.first().unwrap().0;
				let response = client
					.get("/movies?type=concert")
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
					item.get("id").unwrap().as_str().unwrap(),
					expected_movie.id.to_string()
				);
			})
			.is_ok());
	}

	#[test]
	// Test /movies?artist=
	fn test_movies_filter_by_artist() {
		let client = test_client().lock().unwrap();

		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let filtering_artist = data.as_ref().unwrap().package_c.artist.as_ref().unwrap();
				let expected_movie = &data.as_ref().unwrap().package_c.movies.first().unwrap().0;
				let response = client
					.get(format!("/movies?artist={}", filtering_artist.id))
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
					item.get("id").unwrap().as_str().unwrap(),
					expected_movie.id.to_string()
				);
			})
			.is_ok());
	}
}
