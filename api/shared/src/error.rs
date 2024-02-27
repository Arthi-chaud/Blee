use rocket::http::{ContentType, Status};
use rocket::request::Request;
use rocket::response::{self, Responder, Response};
use serde::Serialize;
use std::io::Cursor;

/// Enumeration of errors that can occur.
pub enum Error {
	NotFound { type_: String, identifier: String },
	Conflict { type_: String, identifier: String },
}

// Not Found
// Conflict/Already Exists
// Unknown Error

#[derive(Serialize)]
#[serde(crate = "rocket::serde")]
struct ErrorResponse {
	error_code: Status,
	message: String,
}

impl<'r> Responder<'r, 'static> for Error {
	fn respond_to(self, _: &'r Request<'_>) -> response::Result<'static> {
		let response_body: ErrorResponse = match self {
			Error::NotFound { type_: t, identifier: id } => ErrorResponse {
				error_code: Status::NotFound,
				message: format!("No {} with identifier '{}' found", t, id),
			},
			Error::Conflict { type_: t, identifier: id } => ErrorResponse {
				error_code: Status::NotFound,
				message: format!("A {} with identifier '{}' already exists", t, id),
			},
			// _ => {
			// 	ErrorResponse {
			// 		error_code: Status::InternalServerError,
			// 		message: String::from("An unknown error occured"),
			// 	}
			// }
		};
		//TODO: Log when 500
		let serialized_body = serde_json::to_string(&response_body).unwrap();
		Response::build()
			.header(ContentType::JSON)
			.status(response_body.error_code)
			.sized_body(serialized_body.len(), Cursor::new(serialized_body))
			.ok()
	}
}
