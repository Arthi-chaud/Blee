use sea_orm_migration::prelude::*;

use crate::m20240311_073406_init_tables::{Image, Package};

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
	async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
		manager
			.alter_table(
				Table::alter()
					.table(Package::Table)
					.add_column(ColumnDef::new(Package::BannerId).uuid())
					.add_foreign_key(
						TableForeignKey::new()
							.name("fk-package-banner_id")
							.from_col(Package::BannerId)
							.to_col(Image::Id)
							.to_tbl(Image::Table)
							.on_delete(ForeignKeyAction::SetNull),
					)
					.to_owned(),
			)
			.await?;
		Ok(())
	}

	async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
		manager
			.alter_table(
				Table::alter()
					.table(Package::Table)
					.drop_column(Package::BannerId)
					.to_owned(),
			)
			.await?;
		Ok(())
	}
}
