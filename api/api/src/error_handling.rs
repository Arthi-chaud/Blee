use std::io::Cursor;

use rocket::http::ContentType;
use rocket::http::Status;
use rocket::response::{self, Responder, Response};
use rocket::serde::json::Json;
use rocket::Request;
use rocket_okapi::gen::OpenApiGenerator;
use rocket_okapi::okapi::openapi3::Responses;
use rocket_okapi::okapi::Map;
use rocket_okapi::response::OpenApiResponderInner;
use rocket_okapi::OpenApiError;
use sea_orm::error::DbErr;
use sea_orm::SqlErr;
use serde::Serialize;

#[derive(Debug)]
pub enum ApiError {
	DatabaseError(DbErr),
	SqlError(SqlErr),
	ImageProcessingError,
	ImageServingError,
}

#[derive(Serialize)]
pub struct ErrorResponse<'s> {
	pub status_code: Status,
	pub message: &'s str,
}

pub type ApiResult<T> = Result<Json<T>, ApiError>;

pub type ApiRawResult<T> = Result<T, ApiError>;

impl<'r> Responder<'r, 'static> for ApiError {
	fn respond_to(self, _: &'r Request<'_>) -> response::Result<'static> {
		let response_body: ErrorResponse = match self {
			ApiError::ImageProcessingError => ErrorResponse {
				status_code: Status::BadRequest,
				message: "Could not process received image.",
			},
			ApiError::ImageServingError => ErrorResponse {
				status_code: Status::NotFound,
				message: "Could not serve requested image.",
			},
			ApiError::DatabaseError(DbErr::RecordNotFound(_)) => ErrorResponse {
				status_code: Status::NotFound,
				message: "Resource not found.",
			},
			ApiError::SqlError(SqlErr::UniqueConstraintViolation(_)) => ErrorResponse {
				status_code: Status::Conflict,
				message: "Resource already exists.",
			},
			_ => {
				// TODO: Log
				ErrorResponse {
					status_code: Status::InternalServerError,
					message: "An unknown error occured",
				}
			}
		};
		let serialized_body = serde_json::to_string(&response_body).unwrap();
		Response::build()
			.header(ContentType::JSON)
			.status(response_body.status_code)
			.sized_body(serialized_body.len(), Cursor::new(serialized_body))
			.ok()
	}
}

impl From<DbErr> for ApiError {
	fn from(error: DbErr) -> ApiError {
		ApiError::DatabaseError(error)
	}
}

impl From<SqlErr> for ApiError {
	fn from(error: SqlErr) -> ApiError {
		ApiError::SqlError(error)
	}
}

impl OpenApiResponderInner for ApiError {
	fn responses(_generator: &mut OpenApiGenerator) -> Result<Responses, OpenApiError> {
		use rocket_okapi::okapi::openapi3::{RefOr, Response as OpenApiReponse};

		let mut responses = Map::new();
		responses.insert(
			"404".to_string(),
			RefOr::Object(OpenApiReponse {
				description: "\
				This response is given when you request a page that does not exists.\
                "
				.to_string(),
				..Default::default()
			}),
		);
		responses.insert(
			"500".to_string(),
			RefOr::Object(OpenApiReponse {
				description: "\
                This response is given when something wend wrong on the server. \
                "
				.to_string(),
				..Default::default()
			}),
		);
		Ok(Responses {
			responses,
			..Default::default()
		})
	}
}

#[catch(404)]
pub fn not_found() -> Json<ErrorResponse<'static>> {
	Json(ErrorResponse {
		status_code: Status::NotFound,
		message: "Route not found.",
	})
}
