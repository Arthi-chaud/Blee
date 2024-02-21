use rocket_sync_db_pools::{database, diesel};
use rocket_okapi::request::{OpenApiFromRequest, RequestHeaderInput};
use rocket_okapi::gen::OpenApiGenerator;

#[database("db")]
pub struct Database(diesel::PgConnection);

// From https://github.com/GREsau/okapi/tree/master?tab=readme-ov-file#q-my-diesel-database-does-not-implement-openapifromrequest
impl<'r> OpenApiFromRequest<'r> for Database {
    fn from_request_input(
        _gen: &mut OpenApiGenerator,
        _name: String,
        _required: bool,
    ) -> rocket_okapi::Result<RequestHeaderInput> {
        Ok(RequestHeaderInput::None)
    }
}