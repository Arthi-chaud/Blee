use rocket::{get, serde::json::Json};
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};
use serde::{Deserialize, Serialize};

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
    openapi_get_routes_spec![settings: index]
}

#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
struct IndexResponse {
    message: &'static str,
}

#[openapi(tag = "Index")]
#[get("/")]
fn index() -> Json<IndexResponse> {
    Json(IndexResponse {
        message: "Hello, world!",
    })
}
