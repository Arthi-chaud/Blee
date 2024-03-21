use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize, Clone)]
#[serde(crate = "rocket::serde")]
pub struct Config {
	pub data_folder: String,
	pub scanner_api_key: String,
}
