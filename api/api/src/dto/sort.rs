use rocket::form::FromFormField;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, JsonSchema, FromFormField, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum SortOrder {
	Asc,
	Desc,
}

impl From<SortOrder> for sea_orm::Order {
	fn from(value: SortOrder) -> Self {
		match value {
			SortOrder::Asc => sea_orm::Order::Asc,
			SortOrder::Desc => sea_orm::Order::Desc,
		}
	}
}

/// Parameters for Sort
pub struct Sort<T> {
	/// The maximum number of items per page
	pub sort_by: T,
	/// Sorting order
	pub order: SortOrder,
}

pub fn build_sort<T>(sort: Option<T>, order: Option<SortOrder>) -> Option<Sort<T>> {
	sort.and_then(|s| {
		Some(Sort {
			sort_by: s,
			order: order.unwrap_or(SortOrder::Asc),
		})
	})
}
