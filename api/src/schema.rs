// @generated automatically by Diesel CLI.

diesel::table! {
    artists (id) {
        id -> Uuid,
        name -> Varchar,
        description -> Nullable<Text>,
        slug -> Varchar,
    }
}
