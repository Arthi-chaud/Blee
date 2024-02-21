use diesel::prelude::*;
use rocket::serde::uuid::Uuid;

#[derive(Queryable, Identifiable, Selectable, Debug, PartialEq)]
#[diesel(table_name = crate::schema::artists)]
#[diesel(check_for_backend(diesel::pg::Pg))]
/// An Artist
pub struct Artist {
    pub id: Uuid,
    pub name: String,
    pub slug: String,
}

// /// An Image
// pub struct Image {
//     pub id: Uuid,
//     pub image_type: ImageType,
//     pub width: u16,
//     pub height: u16,
//     pub blurhash: String,
//     pub colors: [String],
// }

// /// A Media File
// pub struct File {
//     pub id: Uuid,
//     pub path: String,
//     pub quality: VideoQuality,
// }

// /// A Video Image
// pub struct Extra {
//     pub id: Uuid,
//     pub file: File,
//     pub artist: Artist,
//     pub name: String,
//     pub types: [ExtraType],
//     pub thumbnail: Image,
// }


// Enumeration of possible video qualities
// pub enum VideoQuality {
//     FourK,
//     TwoK,
//     FullHD,
//     HD,
//     SD,
// }

// Enumeration of possible types of extras
// pub enum ExtraType {
//     Trailer,
//     BehindTheScenes,
//     Interview,
//     MusicVideo,
//     AlternateAngle,
//     Backdrops,
//     BonusPerformance,
//     Other,
// }

// Enumeration of possible types of movies
// pub enum MovieType {
//     Concert,
//     Documentary,
// }

// Enumeration of possible image natures
// pub enum ImageType {
//     Poster,
//     Thumbnail,
//     Banner,
//     Scrubber,
// }
