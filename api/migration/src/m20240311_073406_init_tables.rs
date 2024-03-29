use sea_orm_migration::{prelude::*, sea_orm::Iterable};

use crate::m20240311_000001_init_enums::{
	ChapterType, ChapterTypeEnum, ExtraType, ExtraTypeEnum, ImageType, MovieType, VideoQuality,
};

#[derive(DeriveMigrationName)]
pub struct Migration;

#[derive(DeriveIden)]
enum Image {
	Table,
	Id,
	Blurhash,
	Colors,
	AspectRatio,
	Type,
}

#[derive(DeriveIden)]
enum Artist {
	Table,
	Id,
	Name,
	Description,
	Slug,
	RegisteredAt,
	PosterId,
}

#[derive(DeriveIden)]
enum File {
	Table,
	Id,
	Size,
	Path,
	Quality,
	ScrubberId,
}

#[derive(DeriveIden)]
enum Package {
	Table,
	Id,
	Name,
	Slug,
	Description,
	ReleaseYear,
	RegisteredAt,
	ArtistId,
	PosterId,
}

#[derive(DeriveIden)]
enum Extra {
	Table,
	Id,
	Name,
	Slug,
	ThumbnailId,
	RegisteredAt,
	PackageId,
	ArtistId,
	FileId,
	DiscIndex,
	TrackIndex,
	Type,
}

#[derive(DeriveIden)]
enum Movie {
	Table,
	Id,
	Name,
	Slug,
	PosterId,
	RegisteredAt,
	PackageId,
	ArtistId,
	FileId,
	Type,
}

#[derive(DeriveIden)]
enum Chapter {
	Table,
	Id,
	Name,
	ThumbnailId,
	MovieId,
	StartTime,
	EndTime,
	Type,
}

#[async_trait::async_trait]
impl MigrationTrait for Migration {
	async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
		manager
			.create_table(
				Table::create()
					.table(Image::Table)
					.if_not_exists()
					.col(
						ColumnDef::new(Image::Id)
							.uuid()
							.not_null()
							.primary_key()
							.extra("DEFAULT gen_random_uuid()"),
					)
					.col(ColumnDef::new(Image::Blurhash).string().not_null())
					.col(ColumnDef::new(Image::AspectRatio).float().not_null())
					.col(
						ColumnDef::new(Image::Type)
							.enumeration(Alias::new("image_type_enum"), ImageType::iter())
							.not_null(),
					)
					.col(
						ColumnDef::new(Image::Colors)
							.array(ColumnType::String(Some(7)))
							.not_null(),
					)
					.to_owned(),
			)
			.await?;
		manager
			.create_table(
				Table::create()
					.table(Artist::Table)
					.if_not_exists()
					.col(
						ColumnDef::new(Artist::Id)
							.uuid()
							.not_null()
							.primary_key()
							.extra("DEFAULT gen_random_uuid()"),
					)
					.col(ColumnDef::new(Artist::Name).string().not_null())
					.col(
						ColumnDef::new(Artist::Slug)
							.string()
							.not_null()
							.unique_key(),
					)
					.col(ColumnDef::new(Artist::Description).text())
					.col(
						ColumnDef::new(Artist::RegisteredAt)
							.date()
							.not_null()
							.default(Expr::current_date()),
					)
					.col(ColumnDef::new(Artist::PosterId).uuid())
					.foreign_key(
						ForeignKey::create()
							.name("fk-artist-poster_id")
							.from(Artist::Table, Artist::PosterId)
							.to(Image::Table, Image::Id)
							.on_delete(ForeignKeyAction::SetNull),
					)
					.to_owned(),
			)
			.await?;
		manager
			.create_table(
				Table::create()
					.table(File::Table)
					.if_not_exists()
					.col(
						ColumnDef::new(File::Id)
							.uuid()
							.not_null()
							.primary_key()
							.extra("DEFAULT gen_random_uuid()"),
					)
					.col(ColumnDef::new(File::Path).string().not_null().unique_key())
					.col(ColumnDef::new(File::Size).big_unsigned().not_null())
					.col(
						ColumnDef::new(File::Quality)
							.enumeration(Alias::new("video_quality_enum"), VideoQuality::iter())
							.not_null(),
					)
					.col(ColumnDef::new(File::ScrubberId).uuid())
					.foreign_key(
						ForeignKey::create()
							.name("fk-file-scrubber_id")
							.from(File::Table, File::ScrubberId)
							.to(Image::Table, Image::Id)
							.on_delete(ForeignKeyAction::SetNull),
					)
					.to_owned(),
			)
			.await?;
		manager
			.create_table(
				Table::create()
					.table(Package::Table)
					.if_not_exists()
					.col(
						ColumnDef::new(Package::Id)
							.uuid()
							.not_null()
							.primary_key()
							.extra("DEFAULT gen_random_uuid()"),
					)
					.col(ColumnDef::new(Package::Name).string().not_null())
					.col(
						ColumnDef::new(Package::Slug)
							.string()
							.not_null()
							.unique_key(),
					)
					.col(ColumnDef::new(Package::ReleaseYear).date())
					.col(ColumnDef::new(Package::Description).text())
					.col(
						ColumnDef::new(Package::RegisteredAt)
							.date()
							.not_null()
							.default(Expr::current_date()),
					)
					.col(ColumnDef::new(Package::ArtistId).uuid())
					.foreign_key(
						ForeignKey::create()
							.name("fk-package-artist_id")
							.from(Package::Table, Package::ArtistId)
							.to(Artist::Table, Artist::Id),
					)
					.col(ColumnDef::new(Package::PosterId).uuid())
					.foreign_key(
						ForeignKey::create()
							.name("fk-package-poster_id")
							.from(Package::Table, Package::PosterId)
							.to(Image::Table, Image::Id)
							.on_delete(ForeignKeyAction::SetNull),
					)
					.to_owned(),
			)
			.await?;

		manager
			.create_table(
				Table::create()
					.table(Extra::Table)
					.if_not_exists()
					.col(
						ColumnDef::new(Extra::Id)
							.uuid()
							.not_null()
							.primary_key()
							.extra("DEFAULT gen_random_uuid()"),
					)
					.col(ColumnDef::new(Extra::Name).string().not_null())
					.col(ColumnDef::new(Extra::Slug).string().not_null())
					.col(ColumnDef::new(Extra::ThumbnailId).uuid())
					.foreign_key(
						ForeignKey::create()
							.name("fk-extra-thumbnail_id")
							.from(Extra::Table, Extra::ThumbnailId)
							.to(Image::Table, Image::Id)
							.on_delete(ForeignKeyAction::SetNull),
					)
					.col(
						ColumnDef::new(Extra::RegisteredAt)
							.date()
							.not_null()
							.default(Expr::current_date()),
					)
					.col(ColumnDef::new(Extra::PackageId).uuid().not_null())
					.foreign_key(
						ForeignKey::create()
							.name("fk-extra-package_id")
							.from(Extra::Table, Extra::PackageId)
							.to(Package::Table, Package::Id),
					)
					.col(ColumnDef::new(Extra::ArtistId).uuid().not_null())
					.foreign_key(
						ForeignKey::create()
							.name("fk-extra-artist_id")
							.from(Extra::Table, Extra::ArtistId)
							.to(Artist::Table, Artist::Id),
					)
					.col(ColumnDef::new(Extra::FileId).uuid().not_null())
					.foreign_key(
						ForeignKey::create()
							.name("fk-extra-file_id")
							.from(Extra::Table, Extra::FileId)
							.to(File::Table, File::Id),
					)
					.col(ColumnDef::new(Extra::DiscIndex).unsigned())
					.col(ColumnDef::new(Extra::TrackIndex).unsigned())
					.col(
						ColumnDef::new(Extra::Type)
							.array(ColumnType::Enum {
								name: ExtraTypeEnum.into_iden(),
								variants: ExtraType::iter().map(|e| e.into_iden()).collect(),
							})
							.not_null(),
					)
					.to_owned(),
			)
			.await?;
		manager
			.create_table(
				Table::create()
					.table(Movie::Table)
					.if_not_exists()
					.col(
						ColumnDef::new(Movie::Id)
							.uuid()
							.not_null()
							.primary_key()
							.extra("DEFAULT gen_random_uuid()"),
					)
					.col(ColumnDef::new(Movie::Name).string().not_null())
					.col(ColumnDef::new(Movie::Slug).string().not_null().unique_key())
					.col(ColumnDef::new(Movie::PosterId).uuid())
					.foreign_key(
						ForeignKey::create()
							.name("fk-movie-poster_id")
							.from(Movie::Table, Movie::PosterId)
							.to(Image::Table, Image::Id)
							.on_delete(ForeignKeyAction::SetNull),
					)
					.col(ColumnDef::new(Movie::PackageId).uuid().not_null())
					.foreign_key(
						ForeignKey::create()
							.name("fk-movie-package_id")
							.from(Movie::Table, Movie::PackageId)
							.to(Package::Table, Package::Id),
					)
					.col(ColumnDef::new(Movie::ArtistId).uuid().not_null())
					.foreign_key(
						ForeignKey::create()
							.name("fk-movie-artist_id")
							.from(Movie::Table, Movie::ArtistId)
							.to(Artist::Table, Artist::Id),
					)
					.col(ColumnDef::new(Movie::FileId).uuid().not_null())
					.foreign_key(
						ForeignKey::create()
							.name("fk-movie-file_id")
							.from(Movie::Table, Movie::FileId)
							.to(File::Table, File::Id),
					)
					.col(
						ColumnDef::new(Movie::RegisteredAt)
							.date()
							.not_null()
							.default(Expr::current_date()),
					)
					.col(
						ColumnDef::new(Movie::Type)
							.enumeration(Alias::new("movie_type_enum"), MovieType::iter())
							.not_null(),
					)
					.to_owned(),
			)
			.await?;

		manager
			.create_table(
				Table::create()
					.table(Chapter::Table)
					.if_not_exists()
					.col(
						ColumnDef::new(Chapter::Id)
							.uuid()
							.not_null()
							.primary_key()
							.extra("DEFAULT gen_random_uuid()"),
					)
					.col(ColumnDef::new(Chapter::Name).string().not_null())
					.col(ColumnDef::new(Chapter::ThumbnailId).uuid())
					.foreign_key(
						ForeignKey::create()
							.name("fk-chapter-thumbnail_id")
							.from(Chapter::Table, Chapter::ThumbnailId)
							.to(Image::Table, Image::Id)
							.on_delete(ForeignKeyAction::SetNull),
					)
					.col(ColumnDef::new(Chapter::MovieId).uuid().not_null())
					.foreign_key(
						ForeignKey::create()
							.name("fk-chapter-movie_id")
							.from(Chapter::Table, Chapter::MovieId)
							.to(Movie::Table, Movie::Id),
					)
					.col(ColumnDef::new(Chapter::StartTime).unsigned().not_null())
					.col(ColumnDef::new(Chapter::EndTime).unsigned().not_null())
					.col(
						ColumnDef::new(Chapter::Type)
							.array(ColumnType::Enum {
								name: ChapterTypeEnum.into_iden(),
								variants: ChapterType::iter().map(|e| e.into_iden()).collect(),
							})
							.not_null(),
					)
					.to_owned(),
			)
			.await?;

		Ok(())
	}

	async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
		manager
			.drop_table(Table::drop().table(Chapter::Table).to_owned())
			.await?;
		manager
			.drop_table(Table::drop().table(Movie::Table).to_owned())
			.await?;
		manager
			.drop_table(Table::drop().table(Extra::Table).to_owned())
			.await?;
		manager
			.drop_table(Table::drop().table(Package::Table).to_owned())
			.await?;
		manager
			.drop_table(Table::drop().table(File::Table).to_owned())
			.await?;
		manager
			.drop_table(Table::drop().table(Artist::Table).to_owned())
			.await?;
		manager
			.drop_table(Table::drop().table(Image::Table).to_owned())
			.await?;
		Ok(())
	}
}
