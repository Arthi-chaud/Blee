use sea_orm_migration::{
	prelude::*,
	sea_orm::{EnumIter, Iterable},
	sea_query::extension::postgres::Type,
};

#[derive(DeriveMigrationName)]
pub struct Migration;

#[derive(DeriveIden)]
struct ChapterTypeEnum;
#[derive(DeriveIden, EnumIter)]
pub enum ChapterType {
	Performance,
	Interview,
	NonMusicalInterview,
	Other,
}

#[derive(DeriveIden)]
struct ExtraTypeEnum;
#[derive(DeriveIden, EnumIter)]
pub enum ExtraType {
	Trailer,
	Interview,
	BehindTheScenes,
	MusicVideo,
	AlternateView,
	Backdrops,
	Performance,
	Other,
}

#[derive(DeriveIden)]
struct MovieTypeEnum;
#[derive(DeriveIden, EnumIter)]
pub enum MovieType {
	Concert,
	Documentary,
}

#[derive(DeriveIden)]
struct ImageTypeEnum;
#[derive(DeriveIden, EnumIter)]
pub enum ImageType {
	Poster,
	Banner,
	Thumbnail,
}

#[derive(DeriveIden)]
struct VideoQualityEnum;
#[derive(Iden, EnumIter)]
pub enum VideoQuality {
	#[iden = "8k"]
	K8,
	#[iden = "4k"]
	K4,
	#[iden = "2k"]
	K2,
	#[iden = "1080p"]
	P1080,
	#[iden = "720p"]
	P720,
	#[iden = "480p"]
	P480,
	#[iden = "360p"]
	P360,
	#[iden = "240p"]
	P240,
	#[iden = "Other"]
	Other,
}

#[async_trait::async_trait]
impl MigrationTrait for Migration {
	async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
		manager
			.create_type(
				Type::create()
					.as_enum(ChapterTypeEnum)
					.values(ChapterType::iter())
					.to_owned(),
			)
			.await?;

		manager
			.create_type(
				Type::create()
					.as_enum(MovieTypeEnum)
					.values(MovieType::iter())
					.to_owned(),
			)
			.await?;

		manager
			.create_type(
				Type::create()
					.as_enum(ExtraTypeEnum)
					.values(ExtraType::iter())
					.to_owned(),
			)
			.await?;

		manager
			.create_type(
				Type::create()
					.as_enum(ImageTypeEnum)
					.values(ImageType::iter())
					.to_owned(),
			)
			.await?;

		manager
			.create_type(
				Type::create()
					.as_enum(VideoQualityEnum)
					.values(VideoQuality::iter())
					.to_owned(),
			)
			.await?;

		Ok(())
	}

	async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
		todo!();
	}
}
