// This files compiles all the exmaple values for DTOs for the Swagger

use chrono::NaiveDate;

pub fn example_package_name() -> &'static str {
	"My Video Collection"
}

pub fn example_package_release_date() -> chrono::NaiveDate {
	NaiveDate::parse_from_str("2000-01-01", "%Y-%m-%d").unwrap()
}

pub fn example_artist_name() -> &'static str {
	"My Artist"
}

pub fn example_chapter_name() -> &'static str {
	"I Love New York..."
}

pub fn example_uuid() -> &'static str {
	"550e8400-e29b-41d4-a716-446655440000"
}

pub fn example_index() -> i16 {
	1
}

pub fn example_size() -> i64 {
	100000000
}

pub fn example_video_quality() -> &'static str {
	"480p"
}

pub fn example_file_path() -> &'static str {
	"/video/My Artist/My Video Collection/1-01 My Extra.mp4"
}

pub fn example_extra_name() -> &'static str {
	"My Extra"
}

pub fn example_movie_type() -> &'static str {
	"Concert"
}
