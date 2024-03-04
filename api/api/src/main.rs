use crate::config::Config;
use crate::controllers::index::index;
use crate::error_handling::not_found;
use crate::swagger::custom_openapi_spec;
use infrastructure::{apply_migrations, Database};
use rocket::fairing::AdHoc;
use rocket::figment::providers::Serialized;
use rocket::figment::Figment;
use rocket::{Build, Rocket};
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{mount_endpoints_and_merged_docs, swagger_ui::*};
use std::env;

mod config;
mod controllers;
mod dto;
mod error_handling;
mod responses;
mod services;
mod swagger;
mod swagger_examples;
mod utils;

#[macro_use]
extern crate rocket;

#[launch]
fn rocket() -> Rocket<Build> {
	create_server()
}

async fn run_migrations(rocket: Rocket<Build>) -> Rocket<Build> {
	let _ = Database::get_one(&rocket)
		.await
		.expect("Get DB Handle from Rocket")
		.run(|conn| apply_migrations(conn))
		.await;

	rocket
}

pub fn create_server() -> Rocket<Build> {
	let data_dir = env::var("CONFIG_DIR").expect("env variable `CONFIG_DIR` not set.");
	let figment = Figment::from(rocket::Config::figment()).merge(Serialized::defaults(Config {
		data_folder: data_dir,
	}));
	let mut building_rocket = rocket::custom(figment)
		.attach(Database::fairing())
		.attach(AdHoc::config::<Config>())
		.attach(AdHoc::on_ignite("Run migrations", run_migrations))
		.register("/", catchers![not_found])
		.mount(
			"/swagger",
			make_swagger_ui(&SwaggerUIConfig {
				url: "./openapi.json".to_owned(),
				..Default::default()
			}),
		)
		.mount("/", routes![index]);

	let openapi_settings = OpenApiSettings {
		json_path: "/swagger/openapi.json".to_owned(),
		..OpenApiSettings::default()
	};
	mount_endpoints_and_merged_docs! {
		building_rocket, "/".to_owned(), openapi_settings,
		"/artists" => controllers::artists::get_routes_and_docs(&openapi_settings),
		"/extras" => controllers::extras::get_routes_and_docs(&openapi_settings),
		"/images" => controllers::images::get_routes_and_docs(&openapi_settings),
		"/movies" => controllers::movies::get_routes_and_docs(&openapi_settings),
		"/swagger" => (vec![], custom_openapi_spec()),
	};

	building_rocket
}
