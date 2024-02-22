// @generated automatically by Diesel CLI.

pub mod sql_types {
    #[derive(diesel::query_builder::QueryId, diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "image-types"))]
    pub struct ImageTypes;
}

diesel::table! {
    artists (id) {
        id -> Uuid,
        name -> Varchar,
        aspect_ratio -> Float8,
        description -> Nullable<Text>,
        slug -> Varchar,
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::ImageTypes;

    images (id) {
        id -> Uuid,
        blurhash -> Varchar,
        colors -> Array<Nullable<Varchar>>,
        #[sql_name = "type"]
        type_ -> ImageTypes,
    }
}

diesel::allow_tables_to_appear_in_same_query!(
    artists,
    images,
);
