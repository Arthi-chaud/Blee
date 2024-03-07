use rocket_okapi::okapi::schemars;
use rocket_okapi::okapi::schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(
	diesel_derive_enum::DbEnum, Debug, PartialEq, Deserialize, Serialize, JsonSchema, Clone, Copy,
)]
#[ExistingTypePath = "crate::schema::sql_types::VideoQualities"]
pub enum VideoQuality {
	#[db_rename = "8k"]
	#[serde(rename(deserialize = "8k", serialize = "8k"))]
	K8,
	#[serde(rename(deserialize = "4k", serialize = "4k"))]
	#[db_rename = "4k"]
	K4,
	#[serde(rename(deserialize = "2k", serialize = "2k"))]
	#[db_rename = "2k"]
	K2,
	#[serde(rename(deserialize = "1080p", serialize = "1080p"))]
	#[db_rename = "1080p"]
	P1080,
	#[serde(rename(deserialize = "720p", serialize = "720p"))]
	#[db_rename = "720p"]
	P720,
	#[serde(rename(deserialize = "480p", serialize = "480p"))]
	#[db_rename = "480p"]
	P420,
	#[serde(rename(deserialize = "360p", serialize = "360p"))]
	#[db_rename = "360p"]
	P360,
	#[serde(rename(deserialize = "240p", serialize = "240p"))]
	#[db_rename = "240p"]
	P240,
	Other,
}
