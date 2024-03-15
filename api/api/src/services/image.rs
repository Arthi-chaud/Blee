use ::blurhash::encode;
use ::image::{io::Reader as ImageReader, EncodableLayout, GenericImageView};
use colors_transform::Rgb;
use entity::image;
use entity::sea_orm_active_enums::ImageTypeEnum;
use rocket::serde::uuid::Uuid;
use sea_orm::{ConnectionTrait, DbErr, EntityTrait, Set};
use std::fs;
use std::io::Cursor;
use std::io::Write;
use std::path::Path;
use webp::Encoder;

use crate::config::Config;
use crate::error_handling::ApiError;

pub async fn create<'s, 'a, C>(
	image_bytes: &Vec<u8>,
	image_type: ImageTypeEnum,
	connection: &'a C,
	config: &Config,
) -> Result<image::Model, ApiError>
where
	C: ConnectionTrait,
{
	let res: Result<(image::ActiveModel, Vec<u8>), ApiError> = {
		let img = ImageReader::new(Cursor::new(image_bytes))
			.with_guessed_format()
			.map_err(|_| ApiError::ImageProcessingError)?
			.decode()
			.map_err(|_| ApiError::ImageProcessingError)?;
		let (width, height) = img.dimensions();

		let webp_image = Encoder::from_image(&img)
			.map_err(|_| ApiError::ImageProcessingError)?
			.encode(100f32);
		let decoded_bytes = img.to_rgba8();

		// Source: https://github.com/RazrFalcon/color-thief-rs/blob/4cc0b1bbf1b725e8241f90ce548a22ce06a84f94/tests/test.rs#L8
		let color_format = match img {
			::image::DynamicImage::ImageRgb8(_) => color_thief::ColorFormat::Rgb,
			::image::DynamicImage::ImageRgba8(_) => color_thief::ColorFormat::Rgba,
			_ => unreachable!(),
		};
		let top_colors_hex = color_thief::get_palette(&decoded_bytes, color_format, 4, 5)
			.map_err(|_| ApiError::ImageProcessingError)?
			.iter()
			.take(5)
			.map(|color| {
				Rgb::from(color.r as f32, color.g as f32, color.b as f32).to_css_hex_string()
			})
			.collect();
		let blurhash_value = encode(3, 4, width, height, &decoded_bytes)
			.map_err(|_| ApiError::ImageProcessingError)?;
		Ok((
			image::ActiveModel {
				blurhash: Set(blurhash_value),
				colors: Set(top_colors_hex),
				aspect_ratio: Set((width as f32) / (height as f32)),
				r#type: Set(image_type),
				..Default::default()
			},
			webp_image.as_bytes().to_vec(),
		))
	};

	let (new_image_row, webp_image) = res?;
	let saved_image = image::Entity::insert(new_image_row)
		.exec_with_returning(connection)
		.await?;

	let image_dir = Path::new(&config.data_folder).join(saved_image.id.to_string());
	fs::create_dir_all(image_dir.clone()).map_err(|_| ApiError::ImageProcessingError)?;
	let mut file = fs::OpenOptions::new()
		.create(true)
		.write(true)
		.open(image_dir.join("image.webp"))
		.map_err(|_| ApiError::ImageProcessingError)?;

	file.write_all(&webp_image)
		.map_err(|_| ApiError::ImageProcessingError)?;

	Ok(saved_image)
}

pub async fn delete<'s, 'a, C>(
	image_uuid: &Uuid,
	connection: &'a C,
	config: &Config,
) -> Result<(), DbErr>
where
	C: ConnectionTrait,
{
	use entity::prelude::Image;
	Image::delete_by_id(image_uuid.to_owned())
		.exec(connection)
		.await?;

	let image_dir = Path::new(&config.data_folder).join(image_uuid.to_string());
	let _ = fs::remove_dir_all(image_dir);
	Ok(())
}

pub async fn get<'a, C>(image_uuid: &Uuid, connection: &'a C) -> Result<image::Model, DbErr>
where
	C: ConnectionTrait,
{
	image::Entity::find_by_id(image_uuid.clone())
		.one(connection)
		.await?
		.map_or(Err(DbErr::RecordNotFound("Image".to_string())), |r| Ok(r))
}
