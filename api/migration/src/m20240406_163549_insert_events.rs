use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
	async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
		let db = manager.get_connection();

		// src: https://github.com/grv07/pglistener_poc/blob/5841e828cb35a720846c439d9d62efc95f3ed858/src/listener_setup.sql#L2
		db.execute_unprepared(
			"
CREATE OR REPLACE FUNCTION table_update_notify() RETURNS trigger AS $$
DECLARE
  id varchar;
  name varchar;
BEGIN
  IF TG_OP = 'INSERT' OR TG_OP = 'UPDATE' THEN
    id = NEW.id;
    name = NEW.name;
  ELSE
    id = OLD.id;
    name = OLD.name;
  END IF;
  PERFORM pg_notify('table_update', json_build_object('table', TG_TABLE_NAME, 'id', id, 'name', name, 'action_type', TG_OP)::text);
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;
",
		)
		.await?;
		for trigger in vec!["INSERT", "DELETE", "UPDATE"] {
			for table in vec!["package", "artist", "movie"] {
				db.execute_unprepared(
					&format!(
						"CREATE TRIGGER {}_notify_{} AFTER {} ON {} FOR EACH ROW EXECUTE PROCEDURE table_update_notify();",
						table, trigger, trigger, table
					)
					.to_string(),
				)
				.await?;
			}
		}

		Ok(())
	}

	async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
		let db = manager.get_connection();
		for trigger in vec!["INSERT", "DELETE", "UPDATE"] {
			for table in vec!["package", "artist", "movie"] {
				db.execute_unprepared(
					&format!("DROP TRIGGER {}_notify_{} ON {};", table, trigger, table).to_string(),
				)
				.await?;
			}
		}
		Ok(())
	}
}
