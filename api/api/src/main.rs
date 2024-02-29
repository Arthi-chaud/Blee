use crate::swagger::custom_openapi_spec;
use infrastructure::Database;
use rocket::{Build, Rocket};
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{mount_endpoints_and_merged_docs, swagger_ui::*};

mod controllers;
mod dto;
mod error_handling;
mod responses;
mod services;
mod swagger;
mod swagger_examples;

#[macro_use]
extern crate rocket;

#[launch]
fn rocket() -> Rocket<Build> {
	create_server()
}

pub fn create_server() -> Rocket<Build> {
	let mut building_rocket = rocket::build().attach(Database::fairing()).mount(
		"/swagger",
		make_swagger_ui(&SwaggerUIConfig {
			url: "./openapi.json".to_owned(),
			..Default::default()
		}),
	);

	let openapi_settings = OpenApiSettings {
		json_path: "/swagger/openapi.json".to_owned(),
		..OpenApiSettings::default()
	};
	mount_endpoints_and_merged_docs! {
		building_rocket, "/".to_owned(), openapi_settings,
		"/swagger" => (vec![], custom_openapi_spec()),
		"/index" => controllers::index::get_routes_and_docs(&openapi_settings),
		"/extras" => controllers::extras::get_routes_and_docs(&openapi_settings),
		"/movies" => controllers::movies::get_routes_and_docs(&openapi_settings),
	};

	building_rocket
}
