use domain::models::chapter::Chapter;
use domain::models::image::Image;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::Serialize;

#[derive(Serialize, JsonSchema)]
pub struct ChapterResponse {
	#[serde(flatten)]
	pub chapter: Chapter,
	pub thumbnail: Option<Image>,
}
