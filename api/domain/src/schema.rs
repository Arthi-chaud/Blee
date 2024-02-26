// @generated automatically by Diesel CLI.

pub mod sql_types {
    #[derive(diesel::query_builder::QueryId, std::fmt::Debug, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "chapter-types"))]
    pub struct ChapterTypes;

    #[derive(diesel::query_builder::QueryId, std::fmt::Debug, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "extra-types"))]
    pub struct ExtraTypes;

    #[derive(diesel::query_builder::QueryId, std::fmt::Debug, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "image-types"))]
    pub struct ImageTypes;

    #[derive(diesel::query_builder::QueryId, std::fmt::Debug, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "movie-types"))]
    pub struct MovieTypes;

    #[derive(diesel::query_builder::QueryId, std::fmt::Debug, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "video-qualities"))]
    pub struct VideoQualities;
}

diesel::table! {
    artists (id) {
        id -> Uuid,
        name -> Varchar,
        description -> Nullable<Text>,
        slug -> Varchar,
        registered_at -> Timestamp,
        poster_id -> Nullable<Uuid>,
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::ChapterTypes;

    chapters (id) {
        id -> Uuid,
        name -> Text,
        thumbnail_id -> Nullable<Uuid>,
        movie_id -> Uuid,
        index -> Int2,
        start_time -> Int2,
        end_time -> Int2,
        #[sql_name = "type"]
        type_ -> Array<Nullable<ChapterTypes>>,
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::ExtraTypes;

    extras (id) {
        id -> Uuid,
        name -> Varchar,
        slug -> Varchar,
        thumbnail_id -> Nullable<Uuid>,
        registered_at -> Timestamp,
        package_id -> Uuid,
        artist_id -> Uuid,
        file_id -> Uuid,
        disc_index -> Nullable<Int2>,
        track_index -> Nullable<Int2>,
        #[sql_name = "type"]
        type_ -> Array<Nullable<ExtraTypes>>,
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::VideoQualities;

    files (id) {
        id -> Uuid,
        size -> Int8,
        path -> Text,
        quality -> VideoQualities,
        scrubber_id -> Uuid,
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::ImageTypes;

    images (id) {
        id -> Uuid,
        blurhash -> Varchar,
        colors -> Array<Nullable<Text>>,
        aspect_ratio -> Float8,
        #[sql_name = "type"]
        type_ -> ImageTypes,
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::MovieTypes;

    movies (id) {
        id -> Uuid,
        name -> Text,
        slug -> Varchar,
        poster_id -> Nullable<Uuid>,
        registered_at -> Timestamp,
        package_id -> Uuid,
        artist_id -> Uuid,
        file_id -> Uuid,
        disc_index -> Int2,
        track_index -> Int2,
        #[sql_name = "type"]
        type_ -> Array<Nullable<MovieTypes>>,
    }
}

diesel::table! {
    packages (id) {
        id -> Uuid,
        name -> Text,
        slug -> Varchar,
        description -> Nullable<Text>,
        #[sql_name = "release-year"]
        release_year -> Nullable<Date>,
        registered_at -> Timestamp,
        artist_id -> Nullable<Uuid>,
        poster_id -> Nullable<Uuid>,
        banner_id -> Nullable<Uuid>,
    }
}

diesel::joinable!(artists -> images (poster_id));
diesel::joinable!(chapters -> images (thumbnail_id));
diesel::joinable!(chapters -> movies (movie_id));
diesel::joinable!(extras -> artists (artist_id));
diesel::joinable!(extras -> files (file_id));
diesel::joinable!(extras -> images (thumbnail_id));
diesel::joinable!(extras -> packages (package_id));
diesel::joinable!(files -> images (scrubber_id));
diesel::joinable!(movies -> artists (artist_id));
diesel::joinable!(movies -> files (file_id));
diesel::joinable!(movies -> images (poster_id));
diesel::joinable!(movies -> packages (package_id));
diesel::joinable!(packages -> artists (artist_id));

diesel::allow_tables_to_appear_in_same_query!(
    artists,
    chapters,
    extras,
    files,
    images,
    movies,
    packages,
);
