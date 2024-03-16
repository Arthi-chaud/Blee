use api::{self, database::Db};
use entity::{artist, chapter, extra, file, image, movie, package};
use once_cell::sync::OnceCell;
use rocket::{
	fairing::AdHoc,
	local::blocking::{Client, LocalResponse},
	Build, Rocket,
};
use sea_orm::{EntityTrait, Set};
use sea_orm_rocket::Database;
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
		let rocket = api::rocket().attach(AdHoc::on_ignite("Apply Seed Data", seed_data));
		Mutex::from(Client::tracked(rocket).expect("valid rocket instance"))
	})
}

pub async fn seed_data(rocket: Rocket<Build>) -> Rocket<Build> {
	let conn = &Db::fetch(&rocket).unwrap().conn;

	let _ = extra::Entity::delete_many().exec(conn).await.unwrap();
	let _ = chapter::Entity::delete_many().exec(conn).await.unwrap();
	let _ = movie::Entity::delete_many().exec(conn).await.unwrap();
	let _ = package::Entity::delete_many().exec(conn).await.unwrap();
	let _ = artist::Entity::delete_many().exec(conn).await.unwrap();
	let _ = file::Entity::delete_many().exec(conn).await.unwrap();
	let _ = image::Entity::delete_many().exec(conn).await.unwrap();
	let _ = artist::Entity::insert(artist::ActiveModel {
		name: Set("Aretha Franklin".to_string()),
		slug: Set("aretha-franklin".to_string()),
		..Default::default()
	})
	.exec_with_returning(conn)
	.await
	.unwrap();

	rocket
}
