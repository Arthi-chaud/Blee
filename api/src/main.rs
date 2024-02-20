use rocket::{Build, Rocket};
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{mount_endpoints_and_merged_docs, swagger_ui::*};


#[macro_use]
extern crate rocket;

mod index;

#[launch]
fn rocket() -> Rocket<Build> {
    create_server()
}

pub fn create_server() -> Rocket<Build> {
    let mut building_rocket = rocket::build()
        .mount(
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
        "/index" => index::get_routes_and_docs(&openapi_settings),
    };

    building_rocket
}

fn custom_openapi_spec() -> OpenApi {
    use rocket_okapi::okapi::openapi3::*;
    OpenApi {
        openapi: OpenApi::default_version(),
        info: Info {
            title: "Blee API".to_owned(),
            description: Some("Documentation of the available endpoints of the API".to_owned()),
            terms_of_service: None,
            contact: None,
            license: Some(License {
                name: "GPL License".to_owned(),
                url: Some("https://github.com/Arthi-chaud/Blee/blob/main/LICENSE".to_owned()),
                ..Default::default()
            }),
            ..Default::default()
        },
        servers: vec![
            Server {
                url: "/".to_owned(),
                description: Some("Localhost".to_owned()),
                ..Default::default()
            },
            Server {
                url: "/api".to_owned(),
                description: Some("Deployed App".to_owned()),
                ..Default::default()
            },
        ],
        ..Default::default()
    }
}
