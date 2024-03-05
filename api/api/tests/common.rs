use api;
use diesel::{sql_query, RunQueryDsl};
use infrastructure::Database;
use once_cell::sync::OnceCell;
use rocket::{
	fairing::AdHoc,
	local::blocking::{Client, LocalResponse},
	Build, Rocket,
};
use serde_json::Value;
use std::{fs, sync::Mutex};
// SRC https://github.com/TatriX/realworld-rust-rocket/blob/master/tests/common.rs

/// Helper function for converting response to json value.
pub fn response_json_value<'a>(response: LocalResponse<'a>) -> Value {
	let body = response.into_string().unwrap();
	serde_json::from_str(&body).expect("can't parse value")
}

pub fn test_client() -> &'static Mutex<Client> {
	static INSTANCE: OnceCell<Mutex<Client>> = OnceCell::new();
	INSTANCE.get_or_init(|| {
		let rocket = api::rocket().attach(AdHoc::on_ignite("Apply Seed Data", seed_data));
		Mutex::from(Client::tracked(rocket).expect("valid rocket instance"))
	})
}

pub async fn seed_data(rocket: Rocket<Build>) -> Rocket<Build> {
	let _ = Database::get_one(&rocket)
		.await
		.expect("Get DB Handle from Rocket")
		.run(|conn| {
			for line in fs::read_to_string("./tests/seed.sql").unwrap().lines() {
				let _ = sql_query(line).execute(conn).or_else(|_| Err(()));
			}
		})
		.await;

	rocket
}
