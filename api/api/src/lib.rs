use crate::config::Config;
use crate::controllers::index::index;
use crate::database::Db;
use crate::error_handling::{not_found, unprocessable_entity};
use crate::swagger::custom_openapi_spec;
use amqprs::connection::OpenConnectionArguments;
use migration::MigratorTrait;
use rocket::fairing::{self, AdHoc};
use rocket::figment::providers::Serialized;
use rocket::figment::Figment;
use rocket::http::Method;
use rocket::{Build, Rocket};
use rocket_cors::*;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{mount_endpoints_and_merged_docs, swagger_ui::*};
use sea_orm_rocket::Database;
use std::env;

pub mod config;
mod controllers;
pub mod database;
pub mod dto;
mod error_handling;
mod events;
pub(crate) mod guards;
pub mod services;
mod swagger;
mod swagger_examples;
pub mod utils;

#[macro_use]
extern crate rocket;

#[launch]
pub fn rocket() -> Rocket<Build> {
	create_server()
}

async fn run_migrations(rocket: Rocket<Build>) -> fairing::Result {
	let conn = &Db::fetch(&rocket).unwrap().conn;
	let _ = migration::Migrator::up(conn, None).await;
	Ok(rocket)
}

fn create_server() -> Rocket<Build> {
	let data_dir = env::var("CONFIG_DIR").expect("env variable `CONFIG_DIR` not set.");
	let scanner_api_key =
		env::var("SCANNER_API_KEY").expect("env variable `SCANNER_API_KEY` not set.");
	if scanner_api_key.is_empty() {
		panic!("env variable `SCANNER_API_KEY` is empty.")
	}
	let matcher_api_key =
		env::var("MATCHER_API_KEY").expect("env variable `MATCHER_API_KEY` not set.");
	if matcher_api_key.is_empty() {
		panic!("env variable `MATCHER_API_KEY` is empty.")
	}
	let figment = Figment::from(rocket::Config::figment()).merge(Serialized::defaults(Config {
		data_folder: data_dir,
		scanner_api_key,
		matcher_api_key,
	}));
	let rabbit_config = deadpool_amqprs::Config::new(
		OpenConnectionArguments::new(
			&env::var("RABBIT_HOST").unwrap_or("localhost".to_string()),
			env::var("RABBIT_PORT")
				.unwrap_or("5672".to_string())
				.parse::<u16>()
				.unwrap(),
			&env::var("RABBIT_USER").unwrap(),
			&env::var("RABBIT_PASS").unwrap(),
		),
		None,
	);
	let rabbit_pool = rabbit_config.create_pool();

	let mut building_rocket = rocket::custom(figment)
		.attach(Db::init())
		.attach(AdHoc::config::<Config>())
		.attach(AdHoc::try_on_ignite("Run migrations", run_migrations))
		.attach(AdHoc::try_on_ignite("Hook Database Events", |r| {
			events::hook_psql_events(r, rabbit_pool)
		}))
		.register("/", catchers![not_found, unprocessable_entity])
		.mount(
			"/swagger",
			make_swagger_ui(&SwaggerUIConfig {
				url: "./openapi.json".to_owned(),
				..Default::default()
			}),
		)
		.mount("/", routes![index]);

	if cfg!(debug_assertions) {
		let cors = CorsOptions::default()
			.allowed_origins(AllowedOrigins::all())
			.allowed_methods(
				vec![Method::Get, Method::Post, Method::Patch]
					.into_iter()
					.map(From::from)
					.collect(),
			)
			.allow_credentials(true)
			.to_cors()
			.unwrap();
		building_rocket = building_rocket.attach(cors);
	}

	let openapi_settings = OpenApiSettings {
		json_path: "/swagger/openapi.json".to_owned(),
		..OpenApiSettings::default()
	};
	mount_endpoints_and_merged_docs! {
		building_rocket, "/".to_owned(), openapi_settings,
		"/artists" => controllers::artists::get_routes_and_docs(&openapi_settings),
		"/chapters" => controllers::chapters::get_routes_and_docs(&openapi_settings),
		"/external_ids" => controllers::external_ids::get_routes_and_docs(&openapi_settings),
		"/extras" => controllers::extras::get_routes_and_docs(&openapi_settings),
		"/files" => controllers::files::get_routes_and_docs(&openapi_settings),
		"/images" => controllers::images::get_routes_and_docs(&openapi_settings),
		"/movies" => controllers::movies::get_routes_and_docs(&openapi_settings),
		"/packages" => controllers::packages::get_routes_and_docs(&openapi_settings),
		"/swagger" => (vec![], custom_openapi_spec()),
	};

	building_rocket
}
