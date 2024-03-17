use crate::swagger_examples::*;
use entity::{image, sea_orm_active_enums::ImageTypeEnum};
use rocket::serde::uuid::Uuid;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

#[derive(Serialize, JsonSchema)]
pub struct ImageResponse {
	#[schemars(example = "example_uuid")]
	pub id: Uuid,
	#[schemars(example = "example_image_blurhash")]
	pub blurhash: String,
	#[schemars(example = "example_image_ratio")]
	pub aspect_ratio: f32,
	pub r#type: ImageType,
	#[schemars(example = "example_image_colors")]
	pub colors: Vec<String>,
}

impl From<image::Model> for ImageResponse {
	fn from(value: image::Model) -> Self {
		ImageResponse {
			id: value.id,
			blurhash: value.blurhash,
			aspect_ratio: value.aspect_ratio,
			r#type: ImageType::from(value.r#type),
			colors: value.colors,
		}
	}
}

#[derive(Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum ImageType {
	Banner,
	Poster,
	Thumbnail,
}

impl From<ImageTypeEnum> for ImageType {
	fn from(value: ImageTypeEnum) -> Self {
		match value {
			ImageTypeEnum::Banner => ImageType::Banner,
			ImageTypeEnum::Poster => ImageType::Poster,
			ImageTypeEnum::Thumbnail => ImageType::Thumbnail,
		}
	}
}

impl From<ImageType> for ImageTypeEnum {
	fn from(value: ImageType) -> Self {
		match value {
			ImageType::Banner => ImageTypeEnum::Banner,
			ImageType::Poster => ImageTypeEnum::Poster,
			ImageType::Thumbnail => ImageTypeEnum::Thumbnail,
		}
	}
}
