use api::{self, database::Db};
use chrono::NaiveDate;
use entity::{
	artist, chapter, extra, file, image, movie, package,
	sea_orm_active_enums::{ExtraTypeEnum, MovieTypeEnum, VideoQualityEnum},
};
use once_cell::sync::OnceCell;
use rocket::{
	fairing::AdHoc,
	futures::future::join_all,
	local::blocking::{Client, LocalResponse},
	Build, Rocket,
};
use sea_orm::{DatabaseTransaction, DbErr, EntityTrait, Set, TransactionTrait};
use sea_orm_rocket::Database;
use serde_json::Value;
use slug::slugify;
use std::{sync::Mutex, vec};

pub struct MovieDummy(pub movie::Model, pub Vec<chapter::Model>, pub file::Model);
pub struct ExtraDummy(pub extra::Model, pub file::Model);

pub struct PackageDummy {
	pub package: package::Model,
	pub movies: Vec<MovieDummy>,
	pub extras: Vec<ExtraDummy>,
	pub artist: Option<artist::Model>,
}

pub struct DummyData {
	pub package_a: PackageDummy,
	pub package_b: PackageDummy,
	pub package_c: PackageDummy,
}

macro_rules! aw {
	($e:expr) => {
		tokio_test::block_on($e)
	};
}

pub static GLOBAL_DATA: Mutex<Option<DummyData>> = Mutex::new(None);

// SRC https://github.com/TatriX/realworld-rust-rocket/blob/master/tests/common.rs

/// Helper function for converting response to json value.
pub fn response_json_value<'a>(response: LocalResponse<'a>) -> Value {
	let body = response.into_string().unwrap();
	serde_json::from_str(&body).expect("can't parse value")
}

pub fn test_client() -> &'static Mutex<Client> {
	static INSTANCE: OnceCell<Mutex<Client>> = OnceCell::new();
	INSTANCE.get_or_init(|| {
		let rocket = api::rocket().attach(AdHoc::on_ignite("Apply Seed Data", setup_test_database));
		Mutex::from(Client::tracked(rocket).expect("valid rocket instance"))
	})
}

pub async fn setup_test_database(rocket: Rocket<Build>) -> Rocket<Build> {
	let conn = &Db::fetch(&rocket).unwrap().conn;

	let _ = extra::Entity::delete_many().exec(conn).await.unwrap();
	let _ = chapter::Entity::delete_many().exec(conn).await.unwrap();
	let _ = movie::Entity::delete_many().exec(conn).await.unwrap();
	let _ = package::Entity::delete_many().exec(conn).await.unwrap();
	let _ = artist::Entity::delete_many().exec(conn).await.unwrap();
	let _ = file::Entity::delete_many().exec(conn).await.unwrap();
	let _ = image::Entity::delete_many().exec(conn).await.unwrap();

	let seed = conn
		.transaction::<_, DummyData, DbErr>(|txn| Box::pin(async move { seed_data(txn).await }))
		.await
		.unwrap();
	let _ = GLOBAL_DATA.lock().unwrap().insert(seed);
	rocket
}

pub async fn seed_data(db: &DatabaseTransaction) -> Result<DummyData, DbErr> {
	let min_to_sec = |min: i64, sec: i64| -> i64 { min * 60 + sec };
	let artist_a = artist::Entity::insert(artist::ActiveModel {
		name: Set("Madonna".to_string()),
		slug: Set("madonna".to_string()),
		..Default::default()
	})
	.exec_with_returning(db)
	.await?;

	let package_a1 = package::Entity::insert(package::ActiveModel {
		name: Set("The Video Collection 93:99".to_string()),
		slug: Set("madonna-the-video-collection-93-99".to_string()),
		release_year: Set(NaiveDate::from_ymd_opt(1999, 1, 1)),
		artist_id: Set(Some(artist_a.id)),
		..Default::default()
	})
	.exec_with_returning(db)
	.await?;

	let package_tracks = vec![
		("Bad Girl", min_to_sec(6, 10)),
		("Fever", min_to_sec(4, 7)),
		("Rain", min_to_sec(4, 33)),
		("Secret", min_to_sec(4, 22)),
		("Take A Bow", min_to_sec(4, 33)),
		("Bedtime Story", min_to_sec(4, 25)),
		("Human Nature", min_to_sec(4, 33)),
		("Love Don't Live Here Anymore", min_to_sec(4, 39)),
		("Frozen", min_to_sec(5, 21)),
		("Ray Of Light", min_to_sec(5, 05)),
		("Drowned World", min_to_sec(4, 57)),
		("The Power Of Good-Bye", min_to_sec(4, 10)),
		("Nothing Really Matters", min_to_sec(4, 25)),
		("Beautiful Stranger", min_to_sec(4, 34)),
	];

	let package_a1_extra = join_all(package_tracks.iter().map(|(track_name, duration)| async {
		let new_file = file::Entity::insert(file::ActiveModel {
			path: Set(format!(
				"/videos/Madonna/The Video Collection 93:99/01 {}.mp4",
				track_name.to_string()
			)),
			size: Set(*duration * 1000), // 1kb per second
			quality: Set(VideoQualityEnum::_480p),
			..Default::default()
		})
		.exec_with_returning(db)
		.await
		.unwrap();

		let new_extra = extra::Entity::insert(extra::ActiveModel {
			name: Set(track_name.to_string()),
			slug: Set(slugify(track_name.to_string()).to_string()),
			package_id: Set(package_a1.id),
			artist_id: Set(artist_a.id),
			disc_index: Set(Some(1)),
			track_index: Set(Some(1)),
			r#type: Set(vec![ExtraTypeEnum::MusicVideo]),
			file_id: Set(new_file.id),
			..Default::default()
		})
		.exec_with_returning(db)
		.await
		.unwrap();
		ExtraDummy(new_extra, new_file)
	}))
	.await;

	let artist_b = artist::Entity::insert(artist::ActiveModel {
		name: Set("MIKA".to_string()),
		slug: Set("mika".to_string()),
		..Default::default()
	})
	.exec_with_returning(db)
	.await?;

	let package_b1 = package::Entity::insert(package::ActiveModel {
		name: Set("Live in Cartoon Motion".to_string()),
		slug: Set("mika-live-in-cartoon-motion".to_string()),
		release_year: Set(NaiveDate::from_ymd_opt(2006, 1, 1)),
		artist_id: Set(Some(artist_b.id)),
		..Default::default()
	})
	.exec_with_returning(db)
	.await?;

	let movie_b1 = {
		let new_file = file::Entity::insert(file::ActiveModel {
			path: Set("/videos/MIKA/Live in Cartoon Motion/Live in Cartoon Motion.mkv".to_string()),
			size: Set(min_to_sec(120, 0) * 1000), // 1kb per second
			quality: Set(VideoQualityEnum::_720p),
			..Default::default()
		})
		.exec_with_returning(db)
		.await
		.unwrap();

		let new_movie = movie::Entity::insert(movie::ActiveModel {
			name: Set("Live in Cartoon Motion".to_string()),
			slug: Set(slugify("mika-live-in-cartoon-motion".to_string()).to_string()),
			package_id: Set(package_b1.id),
			artist_id: Set(artist_b.id),
			file_id: Set(new_file.id),
			r#type: Set(MovieTypeEnum::Concert),
			..Default::default()
		})
		.exec_with_returning(db)
		.await?;
		MovieDummy(new_movie, vec![], new_file)
	};

	let package_a2 = package::Entity::insert(package::ActiveModel {
		name: Set("I'm Going to Tell You a Secret".to_string()),
		slug: Set("im-going-to-tell-you-a-secret".to_string()),
		release_year: Set(NaiveDate::from_ymd_opt(2005, 1, 1)),
		artist_id: Set(Some(artist_a.id)),
		..Default::default()
	})
	.exec_with_returning(db)
	.await?;

	let movie_a2 = {
		let new_file = file::Entity::insert(file::ActiveModel {
			path: Set("/videos/Madonna/I'm Going to Tell You a Secret.mp4".to_string()),
			size: Set(min_to_sec(150, 0) * 1000), // 1kb per second
			quality: Set(VideoQualityEnum::_480p),
			..Default::default()
		})
		.exec_with_returning(db)
		.await
		.unwrap();

		let new_movie = movie::Entity::insert(movie::ActiveModel {
			name: Set("I'm Going to Tell You a Secret".to_string()),
			slug: Set(slugify("madonna-im-going-to-tell-you-a-secret".to_string()).to_string()),
			package_id: Set(package_a2.id),
			artist_id: Set(artist_a.id),
			file_id: Set(new_file.id),
			r#type: Set(MovieTypeEnum::Documentary),
			..Default::default()
		})
		.exec_with_returning(db)
		.await?;
		MovieDummy(new_movie, vec![], new_file)
	};

	Ok(DummyData {
		package_a: PackageDummy {
			movies: vec![],
			package: package_a1,
			artist: Some(artist_a.clone()),
			extras: package_a1_extra,
		},
		package_b: PackageDummy {
			movies: vec![movie_b1],
			package: package_b1,
			artist: Some(artist_b),
			extras: vec![],
		},
		package_c: PackageDummy {
			movies: vec![movie_a2],
			package: package_a2,
			artist: Some(artist_a),
			extras: vec![],
		},
	})
}
