//! `SeaORM` Entity. Generated by sea-orm-codegen 0.12.14

use super::sea_orm_active_enums::VideoQualityEnum;
use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, DeriveEntityModel, Eq, Serialize, Deserialize)]
#[sea_orm(table_name = "file")]
pub struct Model {
	#[sea_orm(primary_key, auto_increment = false)]
	pub id: Uuid,
	#[sea_orm(unique)]
	pub path: String,
	pub size: i64,
	pub duration: i64,
	pub quality: VideoQualityEnum,
	pub registered_at: Date,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
	#[sea_orm(has_many = "super::extra::Entity")]
	Extra,
	#[sea_orm(has_many = "super::movie::Entity")]
	Movie,
}

impl Related<super::extra::Entity> for Entity {
	fn to() -> RelationDef {
		Relation::Extra.def()
	}
}

impl Related<super::movie::Entity> for Entity {
	fn to() -> RelationDef {
		Relation::Movie.def()
	}
}

impl ActiveModelBehavior for ActiveModel {}
