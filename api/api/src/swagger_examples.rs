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

pub fn example_artist_slug() -> &'static str {
	"my-artist"
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

pub fn example_description() -> &'static str {
	"This is a short description of some kind of content."
}

pub fn example_image_blurhash() -> &'static str {
	"L00000fQfQfQfQfQfQfQfQfQfQfQ"
}

pub fn example_image_ratio() -> f32 {
	4 as f32 / 3 as f32
}

pub fn example_image_colors() -> Vec<&'static str> {
	vec!["#ffffff", "#000000", "#ff0000"]
}

pub fn example_rating() -> i16 {
	50
}

pub fn example_provider_name() -> &'static str {
	"TMDB"
}
