use api;
use once_cell::sync::OnceCell;
use rocket::local::blocking::{Client, LocalResponse};
use serde_json::Value;
use std::sync::Mutex;
// SRC https://github.com/TatriX/realworld-rust-rocket/blob/master/tests/common.rs

/// Helper function for converting response to json value.
pub fn response_json_value<'a>(response: LocalResponse<'a>) -> Value {
	let body = response.into_string().unwrap();
	serde_json::from_str(&body).expect("can't parse value")
}

pub fn test_client() -> &'static Mutex<Client> {
	static INSTANCE: OnceCell<Mutex<Client>> = OnceCell::new();
	INSTANCE.get_or_init(|| {
		let rocket = api::rocket();
		Mutex::from(Client::tracked(rocket).expect("valid rocket instance"))
	})
}
