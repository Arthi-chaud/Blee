use diesel_migrations::{embed_migrations, EmbeddedMigrations, MigrationHarness};
use rocket_okapi::{gen::OpenApiGenerator, request::*};
use rocket_sync_db_pools::{database, diesel};

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

const MIGRATIONS: EmbeddedMigrations = embed_migrations!("./migrations");

pub fn apply_migrations(connection: &mut impl MigrationHarness<diesel::pg::Pg>) {
	match connection.run_pending_migrations(MIGRATIONS) {
		Ok(_) => {
			println!("Migrations successfully completed");
		}
		Err(e) => {
			panic!("error running pending migrations {}", e)
		}
	};
}
