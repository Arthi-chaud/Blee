use rocket::form::FromForm;
use rocket::http::ContentType;
use rocket::response::Responder;
use rocket::{Request, Response};
use rocket_okapi::gen::OpenApiGenerator;
use rocket_okapi::okapi::openapi3::Responses;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use rocket_okapi::response::OpenApiResponderInner;
use rocket_okapi::util::add_schema_response;
use serde::{Deserialize, Serialize};
use std::io::Cursor;

const DEFAULT_PAGE_SIZE: u64 = 20;

fn get_default_page_size() -> u64 {
	DEFAULT_PAGE_SIZE
}

fn get_default_skip() -> u64 {
	0
}

/// Parameters for pagination
#[derive(Serialize, Deserialize, JsonSchema, FromForm)]
pub struct Pagination {
	/// The maximum number of items per page
	#[schemars(range(min = 1))]
	#[serde(default = "get_default_page_size")]
	#[field(default = DEFAULT_PAGE_SIZE, validate = range(1..).or_else(msg!("Page size should be at least 1.")))]
	pub take: u64,
	/// The number of items to skip, in the current order
	#[schemars(range(min = 0))]
	#[serde(default = "get_default_skip")]
	#[field(validate = range(0..).or_else(msg!("Skip should not be negative.")), default = 0)]
	pub skip: u64,
}

/// Builds a page from a list of items.
/// The related response will take care of page metadata
pub struct Page<T>(Vec<T>);

impl<T> Page<T> {
	pub fn from(items: Vec<T>) -> Page<T> {
		Page(items)
	}
}

impl<T: JsonSchema> OpenApiResponderInner for Page<T> {
	fn responses(gen: &mut OpenApiGenerator) -> rocket_okapi::Result<Responses> {
		let mut responses = Responses::default();
		let schema = gen.json_schema::<PageResponse<T>>();
		add_schema_response(&mut responses, 200, "application/json", schema)?;
		Ok(responses)
	}
}

impl<'r, T: Serialize> Responder<'r, 'r> for Page<T> {
	fn respond_to(self, r: &'r Request<'_>) -> rocket::response::Result<'r> {
		let requested_page_size = r
			.query_value::<u64>("take")
			.map_or_else(|| DEFAULT_PAGE_SIZE, |r| r.unwrap());
		let skipped = r
			.query_value::<usize>("skip")
			.map_or_else(|| 0, |r| r.unwrap());
		let is_last_page = self.0.len() != (requested_page_size as usize);
		let next_route = if is_last_page {
			None
		} else {
			let path = r.uri().path();
			let mut query_params: Vec<String> = r
				.query_fields()
				.filter(|field| field.name.as_name().ne("skip"))
				.map(|field| format!("{}={}", field.name.as_name().as_str(), field.value))
				.collect();
			let next_page_skip = skipped + self.0.len();
			if next_page_skip != 0 {
				query_params.push(format!("skip={}", next_page_skip));
			}
			let mut full_next_route = path.to_string();
			full_next_route.push('?');
			full_next_route.push_str(query_params.join("&").as_str());
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
			.header(ContentType::parse_flexible("application/json; charset=utf-8").unwrap())
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
	"/route?param1=a&param2=b&take=&skip="
}
