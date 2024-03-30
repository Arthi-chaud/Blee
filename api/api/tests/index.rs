use common::*;
use rocket::{form::validate::Contains, http::Status};
#[macro_use]
mod common;

#[test]
/// Test `/`
fn test_index() {
	let client = test_client().lock().unwrap();

	let response = client.get("/").dispatch();

	assert_eq!(response.status(), Status::Ok);

	let body = response_json_value(response);
	let message = body
		.get("message")
		.expect("must have an 'message' field")
		.as_str();

	assert!(message.contains("/swagger"));
}
