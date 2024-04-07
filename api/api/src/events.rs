use serde::Deserialize;
use sqlx::postgres::PgNotification;

#[derive(Deserialize, Debug)]
pub enum ActionType {
    INSERT,
    UPDATE,
    DELETE,
}

#[derive(Deserialize, Debug)]
pub struct EventPayload {
    pub table: String,
    pub action_type: ActionType,
    pub id: String,
    pub name: String,
}

pub fn handle_database_event(event: &PgNotification) {
	let strr = event.payload().to_owned();
	let payload = serde_json::from_str::<EventPayload>(&strr).unwrap();
	println!("{}", payload.id);
}