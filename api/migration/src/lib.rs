pub use sea_orm_migration::prelude::*;

mod m20240311_000001_init_enums;
mod m20240311_073406_init_tables;

pub struct Migrator;

#[async_trait::async_trait]
impl MigratorTrait for Migrator {
	fn migrations() -> Vec<Box<dyn MigrationTrait>> {
		vec![
			Box::new(m20240311_000001_init_enums::Migration),
			Box::new(m20240311_073406_init_tables::Migration),
		]
	}
}
