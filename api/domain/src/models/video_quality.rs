#[derive(diesel_derive_enum::DbEnum, Debug, PartialEq)]
#[ExistingTypePath = "crate::schema::sql_types::VideoQualities"]
pub enum VideoQuality {
    #[db_rename = "8k"]
    K8,
    #[db_rename = "4k"]
    K4,
    #[db_rename = "2k"]
    K2,
    #[db_rename = "1080p"]
    P1080,
    #[db_rename = "720p"]
    P720,
    #[db_rename = "480p"]
    P420,
    #[db_rename = "360p"]
    P360,
    #[db_rename = "240p"]
    P240,
    Other,
}
