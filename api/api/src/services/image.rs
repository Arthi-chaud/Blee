use ::blurhash::encode;
use colors_transform::Rgb;
use diesel::insert_into;
use diesel::prelude::*;
use diesel::PgConnection;
use domain::models::image::Image;
use domain::models::image::ImageType;
use image::io::Reader as ImageReader;
use image::EncodableLayout;
use image::GenericImageView;
use rocket::serde::uuid::Uuid;
use std::fs;
use std::io::Cursor;
use std::io::Write;
use std::path::MAIN_SEPARATOR;
use webp::Encoder;

use crate::config::Config;
use crate::error_handling::ApiError;

#[derive(Insertable)]
#[diesel(table_name = domain::schema::images)]
struct CreateImage {
	blurhash: String,
	colors: Vec<Option<String>>,
	aspect_ratio: f64,
	type_: ImageType,
}

pub fn create<'s>(
	image_bytes: &Vec<u8>,
	image_type: ImageType,
	connection: &mut PgConnection,
	config: &Config,
) -> Result<Image, ApiError> {
	let webp_image;
	let image: Result<CreateImage, ApiError> = {
		let img = ImageReader::new(Cursor::new(image_bytes))
			.with_guessed_format()
			.map_err(|_| ApiError::ImageProcessingError)?
			.decode()
			.map_err(|_| ApiError::ImageProcessingError)?;
		let (width, height) = img.dimensions();

		webp_image = Encoder::from_image(&img)
			.map_err(|_| ApiError::ImageProcessingError)?
			.encode(100f32);
		let decoded_bytes = img.to_rgba8();

		// Source: https://github.com/RazrFalcon/color-thief-rs/blob/4cc0b1bbf1b725e8241f90ce548a22ce06a84f94/tests/test.rs#L8
		let color_format = match img {
			image::DynamicImage::ImageRgb8(_) => color_thief::ColorFormat::Rgb,
			image::DynamicImage::ImageRgba8(_) => color_thief::ColorFormat::Rgba,
			_ => unreachable!(),
		};
		let top_colors_hex = color_thief::get_palette(&decoded_bytes, color_format, 4, 5)
			.map_err(|_| ApiError::ImageProcessingError)?
			.iter()
			.take(5)
			.map(|color| {
				Rgb::from(color.r as f32, color.g as f32, color.b as f32).to_css_hex_string()
			})
			.map(|c| Some(c))
			.collect();
		let blurhash_value = encode(3, 4, width, height, &decoded_bytes)
			.map_err(|_| ApiError::ImageProcessingError)?;
		Ok(CreateImage {
			blurhash: blurhash_value,
			colors: top_colors_hex,
			aspect_ratio: (width as f64) / (height as f64),
			type_: image_type,
		})
	};

	use domain::schema::images::dsl::*;

	let image_row = insert_into(images)
		.values(image?)
		.get_result::<Image>(connection)
		.map_err(|e| ApiError::DieselError(e))?;

	let image_dir = format!(
		"{}{}{}",
		config.data_folder,
		MAIN_SEPARATOR,
		image_row.id.to_string()
	);
	fs::create_dir_all(image_dir.clone()).map_err(|_| ApiError::ImageProcessingError)?;
	let mut file = fs::OpenOptions::new()
		.create(true)
		.write(true)
		.open(format!(
			"{}{}{}",
			image_dir.clone(),
			MAIN_SEPARATOR,
			"image.webp"
		))
		.map_err(|_| ApiError::ImageProcessingError)?;

	file.write_all(&webp_image.as_bytes())
		.map_err(|_| ApiError::ImageProcessingError)?;

	Ok(image_row)
}

pub fn delete<'s>(
	image_uuid: &Uuid,
	connection: &mut PgConnection,
	config: &Config,
) -> Result<(), ApiError> {
	use domain::schema::images::dsl::*;

	diesel::delete(images)
		.filter(id.eq(image_uuid))
		.execute(connection)?;

	let image_dir = format!(
		"{}{}{}",
		config.data_folder,
		MAIN_SEPARATOR,
		image_uuid.to_string()
	);
	fs::remove_dir_all(image_dir).map_err(|_| ApiError::ImageProcessingError)?;
	Ok(())
}
