use crate::m20240311_073406_init_tables::{File, Image};
use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
	async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
		manager
			.alter_table(
				Table::alter()
					.table(File::Table)
					.drop_column(File::ScrubberId)
					.to_owned(),
			)
			.await?;
		Ok(())
	}

	async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
		manager
			.alter_table(
				Table::alter()
					.table(File::Table)
					.add_column(ColumnDef::new(File::ScrubberId).uuid())
					.add_foreign_key(
						TableForeignKey::new()
							.name("fk-file-scrubber_id")
							.from_col(File::ScrubberId)
							.to_col(Image::Id)
							.to_tbl(Image::Table)
							.on_delete(ForeignKeyAction::SetNull),
					)
					.to_owned(),
			)
			.await?;
		Ok(())
	}
}
