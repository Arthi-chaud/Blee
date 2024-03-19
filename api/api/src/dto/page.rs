use rocket::http::ContentType;
use rocket::response::Responder;
use rocket::{form::FromForm, serde::uuid::Uuid};
use rocket::{Request, Response};
use rocket_okapi::gen::OpenApiGenerator;
use rocket_okapi::okapi::openapi3::Responses;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use rocket_okapi::response::OpenApiResponderInner;
use rocket_okapi::util::add_schema_response;
use serde::{Deserialize, Serialize};
use std::io::Cursor;

use crate::utils::Identifiable;

const DEFAULT_PAGE_SIZE: u64 = 20;

fn get_default_page_size() -> u64 {
	DEFAULT_PAGE_SIZE
}

/// Parameters for pagination
#[derive(Serialize, Deserialize, JsonSchema, FromForm)]
pub struct Pagination {
	/// The maximum number of items per page
	#[schemars(range(min = 2))]
	#[serde(default = "get_default_page_size")]
	#[field(validate = range(2..).or_else(msg!("Page size should be at least 2.")), default = DEFAULT_PAGE_SIZE)]
	pub page_size: u64,
	/// If specified, page will start with the item following the item with the
	/// provided id
	pub after_id: Option<Uuid>,
}

/// Builds a page from a list of items.
/// The related response will take care of page metadata
pub struct Page<T>(Vec<T>);

impl<T> Page<T> {
	pub fn from(items: Vec<T>) -> Page<T> {
		Page(items)
	}
}

impl<T: Identifiable + JsonSchema> OpenApiResponderInner for Page<T> {
	fn responses(gen: &mut OpenApiGenerator) -> rocket_okapi::Result<Responses> {
		let mut responses = Responses::default();
		let schema = gen.json_schema::<PageResponse<T>>();
		add_schema_response(&mut responses, 200, "application/json", schema)?;
		Ok(responses)
	}
}

impl<'r, T: Identifiable + Serialize> Responder<'r, 'r> for Page<T> {
	fn respond_to(self, r: &'r Request<'_>) -> rocket::response::Result<'r> {
		let requested_page_size = r
			.query_value::<u64>("page_size")
			.map_or_else(|| DEFAULT_PAGE_SIZE, |r| r.unwrap());
		let is_last_page = self.0.len() != (requested_page_size as usize);
		let next_route = if is_last_page {
			None
		} else {
			let path = r.uri().path();
			let query_params: Vec<String> = r
				.query_fields()
				.map(|field| match field.name.as_name().as_str() {
					"after_id" => format!("after_id={}", self.0.last().unwrap().get_id().as_str()),
					_ => format!("{}={}", field.name.as_name().as_str(), field.value),
				})
				.collect();
			let mut full_next_route = path.to_string();
			full_next_route.push('?');
			full_next_route.push_str(query_params.join("?").as_str());
			Some(full_next_route)
		};

		let json_object = PageResponse {
			metadata: PageMetadata {
				count: self.0.len(),
				next: next_route,
			},
			items: self.0,
		};
		let serialized = serde_json::to_string(&json_object).unwrap();
		Response::build()
			.header(ContentType::JSON)
			.sized_body(serialized.len(), Cursor::new(serialized))
			.ok()
	}
}

#[derive(JsonSchema, Serialize)]
struct PageResponse<T> {
	items: Vec<T>,
	metadata: PageMetadata,
}

#[derive(JsonSchema, Serialize)]
struct PageMetadata {
	/// The number of items in the page
	count: usize,
	/// The URL to the next page.
	/// If it is null, the current page is the last
	#[schemars(example = "example_next")]
	next: Option<String>,
}

fn example_next() -> &'static str {
	"/route?param1=a&param2=b&after_id=[FINAL ITEM'S UUID]"
}
