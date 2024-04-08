use rocket::http::Status;
use rocket::request::{FromRequest, Outcome, Request};
use rocket_okapi::gen::OpenApiGenerator;
use rocket_okapi::okapi;
use rocket_okapi::okapi::openapi3::{
	Object, RefOr, Response, Responses, SecurityRequirement, SecurityScheme, SecuritySchemeData,
};
use rocket_okapi::request::{OpenApiFromRequest, RequestHeaderInput};

use crate::config::Config;

/// The Request Guard for API Key, used by the Scanner to authenticate itself
pub struct ScannerAuthGuard;

/// The Request Guard for API Key, used by the Matcher to authenticate itself
pub struct MatcherAuthGuard;

#[derive(Debug)]
pub enum ApiKeyError {
	Missing,
	Invalid,
}

#[rocket::async_trait]
impl<'r> FromRequest<'r> for ScannerAuthGuard {
	type Error = ApiKeyError;

	async fn from_request(req: &'r Request<'_>) -> Outcome<Self, Self::Error> {
		let scanner_api_key = req
			.rocket()
			.state::<Config>()
			.unwrap()
			.scanner_api_key
			.clone();

		match req.headers().get_one("x-api-key") {
			None => Outcome::Error((Status::Unauthorized, ApiKeyError::Missing)),
			Some(key) if key == scanner_api_key => Outcome::Success(ScannerAuthGuard),
			Some(_) => Outcome::Error((Status::BadRequest, ApiKeyError::Invalid)),
		}
	}
}

#[rocket::async_trait]
impl<'r> FromRequest<'r> for MatcherAuthGuard {
	type Error = ApiKeyError;

	async fn from_request(req: &'r Request<'_>) -> Outcome<Self, Self::Error> {
		let matcher_api_key = req
			.rocket()
			.state::<Config>()
			.unwrap()
			.matcher_api_key
			.clone();

		match req.headers().get_one("x-api-key") {
			None => Outcome::Error((Status::Unauthorized, ApiKeyError::Missing)),
			Some(key) if key == matcher_api_key => Outcome::Success(MatcherAuthGuard),
			Some(_) => Outcome::Error((Status::BadRequest, ApiKeyError::Invalid)),
		}
	}
}

impl<'r> OpenApiFromRequest<'r> for ScannerAuthGuard {
	fn from_request_input(
		_gen: &mut OpenApiGenerator,
		_name: String,
		_required: bool,
	) -> rocket_okapi::Result<rocket_okapi::request::RequestHeaderInput> {
		// SRC: https://github.com/GREsau/okapi/blob/1608ff7b92e3daca8cf05aa4594e1cf163e584a9/examples/secure_request_guard/src/api_key.rs
		let security_scheme = SecurityScheme {
			description: Some("Requires the Scanner's API key to access.".to_owned()),
			data: SecuritySchemeData::ApiKey {
				name: "x-api-key".to_owned(),
				location: "header".to_owned(),
			},
			extensions: Object::default(),
		};
		// Add the requirement for this route/endpoint
		// This can change between routes.
		let mut security_req = SecurityRequirement::new();
		// Each security requirement needs to be met before access is allowed.
		security_req.insert("ApiKeyAuth".to_owned(), Vec::new());
		// These vvvvvvv-----^^^^^^^^^^ values need to match exactly!
		Ok(RequestHeaderInput::Security(
			"ApiKeyAuth".to_owned(),
			security_scheme,
			security_req,
		))
	}

	fn get_responses(_gen: &mut OpenApiGenerator) -> rocket_okapi::Result<Responses> {
		Ok(Responses {
			// Recommended and most strait forward.
			// And easy to add or remove new responses.
			responses: okapi::map! {
				"400".to_owned() => RefOr::Object(Response {
					description: "The Provided API Key or form is wrong or bad."
					.to_string(),
					..Default::default()
				}),
				"401".to_owned() => RefOr::Object(Response {
					description: "An API Key is missing."
					.to_string(),
					..Default::default()
				}),
			},
			..Default::default()
		})
	}
}

impl<'r> OpenApiFromRequest<'r> for MatcherAuthGuard {
	fn from_request_input(
		_gen: &mut OpenApiGenerator,
		_name: String,
		_required: bool,
	) -> rocket_okapi::Result<rocket_okapi::request::RequestHeaderInput> {
		// SRC: https://github.com/GREsau/okapi/blob/1608ff7b92e3daca8cf05aa4594e1cf163e584a9/examples/secure_request_guard/src/api_key.rs
		let security_scheme = SecurityScheme {
			description: Some("Requires the Matcher's API key to access.".to_owned()),
			data: SecuritySchemeData::ApiKey {
				name: "x-api-key".to_owned(),
				location: "header".to_owned(),
			},
			extensions: Object::default(),
		};
		// Add the requirement for this route/endpoint
		// This can change between routes.
		let mut security_req = SecurityRequirement::new();
		// Each security requirement needs to be met before access is allowed.
		security_req.insert("ApiKeyAuth".to_owned(), Vec::new());
		// These vvvvvvv-----^^^^^^^^^^ values need to match exactly!
		Ok(RequestHeaderInput::Security(
			"ApiKeyAuth".to_owned(),
			security_scheme,
			security_req,
		))
	}

	fn get_responses(_gen: &mut OpenApiGenerator) -> rocket_okapi::Result<Responses> {
		Ok(Responses {
			// Recommended and most strait forward.
			// And easy to add or remove new responses.
			responses: okapi::map! {
				"400".to_owned() => RefOr::Object(Response {
					description: "The Provided API Key or form is wrong or bad."
					.to_string(),
					..Default::default()
				}),
				"401".to_owned() => RefOr::Object(Response {
					description: "An API Key is missing."
					.to_string(),
					..Default::default()
				}),
			},
			..Default::default()
		})
	}
}
