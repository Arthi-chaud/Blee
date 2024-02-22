use crate::database::Database;
use crate::models::artist::*;
use rocket::{get, serde::json::Json};
use rocket_okapi::okapi::openapi3::OpenApi;
use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use rocket_okapi::settings::OpenApiSettings;
use rocket_okapi::{openapi, openapi_get_routes_spec};
use serde::{Deserialize, Serialize};
use diesel::prelude::*;

pub fn get_routes_and_docs(settings: &OpenApiSettings) -> (Vec<rocket::Route>, OpenApi) {
    openapi_get_routes_spec![settings: index]
}

#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "camelCase")]
struct IndexResponse {
    message: String,
}

#[openapi(tag = "Index")]
#[get("/")]
async fn index(db: Database) -> Json<IndexResponse> {
    let _x = db
        .run(|conn| -> _ {
            use crate::schema::artists::dsl::*;

            artists
                .select(Artist::as_select())
                .first(conn)
                .expect("")
        }).await;
    Json(IndexResponse { message: _x.slug })
}
