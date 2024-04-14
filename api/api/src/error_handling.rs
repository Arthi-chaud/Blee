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
use sea_orm::RuntimeErr;
use sea_orm::SqlErr;
use sea_orm::TransactionError;
use serde::Serialize;

use crate::dto::page::Page;

#[derive(Debug)]
pub enum ApiError {
	DatabaseError(DbErr),
	SqlError(SqlErr),
	ValidationError(String),
	ImageProcessingError,
	ImageServingError,
}

#[derive(Serialize)]
pub struct ErrorResponse {
	pub status_code: Status,
	pub message: String,
}

pub type ApiResult<T> = Result<Json<T>, ApiError>;

pub type ApiPageResult<T> = Result<Page<T>, ApiError>;

pub type ApiRawResult<T> = Result<T, ApiError>;

impl<'r> Responder<'r, 'static> for ApiError {
	fn respond_to(self, _: &'r Request<'_>) -> response::Result<'static> {
		let response_body: ErrorResponse = match self {
			ApiError::ValidationError(s) => ErrorResponse {
				status_code: Status::BadRequest,
				message: s,
			},
			ApiError::ImageProcessingError => ErrorResponse {
				status_code: Status::BadRequest,
				message: "Could not process received image.".to_owned(),
			},
			ApiError::ImageServingError => ErrorResponse {
				status_code: Status::NotFound,
				message: "Could not serve requested image.".to_owned(),
			},
			ApiError::DatabaseError(DbErr::RecordNotFound(_)) => ErrorResponse {
				status_code: Status::NotFound,
				message: "Resource not found.".to_owned(),
			},
			ApiError::DatabaseError(DbErr::Query(RuntimeErr::SqlxError(
				sqlx::error::Error::Database(pg_error),
			))) => match pg_error.code().unwrap() {
				std::borrow::Cow::Borrowed("23505") => ErrorResponse {
					status_code: Status::Conflict,
					message: "Resource already exists.".to_owned(),
				},
				x => {
					// TODO: Log
					println!("{:?}", x);
					ErrorResponse {
						status_code: Status::InternalServerError,
						message: "Unhandled Database Error".to_owned(),
					}
				}
			},
			ApiError::SqlError(SqlErr::UniqueConstraintViolation(_)) => ErrorResponse {
				status_code: Status::Conflict,
				message: "Resource already exists.".to_owned(),
			},
			x => {
				// TODO: Log
				println!("{:?}", x);
				ErrorResponse {
					status_code: Status::InternalServerError,
					message: "An unknown error occured".to_owned(),
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

impl From<TransactionError<DbErr>> for ApiError {
	fn from(error: TransactionError<DbErr>) -> ApiError {
		match error {
			TransactionError::Connection(c) => ApiError::from(c),
			TransactionError::Transaction(c) => ApiError::from(c),
		}
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
pub fn not_found() -> Json<ErrorResponse> {
	Json(ErrorResponse {
		status_code: Status::NotFound,
		message: "Route not found.".to_owned(),
	})
}

#[catch(422)]
pub fn unprocessable_entity() -> Json<ErrorResponse> {
	Json(ErrorResponse {
		status_code: Status::UnprocessableEntity,
		message: "Unprocessable Entity. A form is probably semantically wrong.".to_owned(),
	})
}
