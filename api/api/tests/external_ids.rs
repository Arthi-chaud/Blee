#[macro_use]
mod common;

#[cfg(test)]
mod test_external_id {
	use std::env;

	use api::dto::external_id::NewExternalId;
	use rocket::{
		http::{ContentType, Header, Status},
		serde::uuid,
	};

	use crate::common::*;

	#[test]
	/// Test POST `/external_ids/`
	fn test_external_id() {
		let client = test_client().lock().unwrap();
		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let artist_id = &data.as_ref().unwrap().package_a.package.artist_id.unwrap();
				let dto = NewExternalId {
					url: "a.b/c".to_string(),
					value: "c".to_string(),
					description: Some("This is an artist description".to_string()),
					rating: Some(50),
					provider_name: "TMDB".to_string(),
					artist_id: Some(artist_id.clone()),
					package_id: None,
				};
				let response = client
					.post("/external_ids")
					.header(ContentType::JSON)
					.header(Header::new(
						"X-API-Key",
						env::var("MATCHER_API_KEY").unwrap(),
					))
					.body(serde_json::to_value(dto.clone()).unwrap().to_string())
					.dispatch();
				assert_eq!(response.status(), Status::Created);
				let value_response = response_json_value(response);

				let url = value_response.get("url").unwrap().as_str().unwrap();
				assert_eq!(url, dto.url);
				let provider_name = value_response
					.get("provider_name")
					.unwrap()
					.as_str()
					.unwrap();
				assert_eq!(provider_name, dto.provider_name);
				let value = value_response.get("value").unwrap().as_str().unwrap();
				assert_eq!(value, dto.value);
				let rating = value_response.get("rating").unwrap().as_i64().unwrap() as i16;
				assert_eq!(rating, dto.rating.unwrap());
				let description = value_response.get("description").unwrap().as_str().unwrap();
				assert_eq!(description, dto.description.unwrap());
				assert_eq!(
					value_response.get("artist_id").unwrap().as_str().unwrap(),
					artist_id.to_string()
				);
				assert_eq!(value_response.get("package_id"), None);
			})
			.is_ok());
	}

	#[test]
	/// Test POST `/external_ids/` (Artist Already has external ID from
	/// provider)
	fn test_external_id_duplicate() {
		let client = test_client().lock().unwrap();
		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let artist_id = &data.as_ref().unwrap().package_a.package.artist_id.unwrap();
				let dto = NewExternalId {
					url: "a.b/c".to_string(),
					value: "c".to_string(),
					description: Some("This is a description".to_string()),
					rating: Some(50),
					provider_name: "TMDB".to_string(),
					artist_id: Some(artist_id.clone()),
					package_id: None,
				};
				let response = client
					.post("/external_ids")
					.header(ContentType::JSON)
					.header(Header::new(
						"X-API-Key",
						env::var("MATCHER_API_KEY").unwrap(),
					))
					.body(serde_json::to_value(dto.clone()).unwrap().to_string())
					.dispatch();
				assert_eq!(response.status(), Status::Conflict);
			})
			.is_ok());
	}

	#[test]
	/// Test POST `/external_ids/` (Invalid Rating)
	fn test_external_id_invalid_rating() {
		let client = test_client().lock().unwrap();
		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let artist_id = &data.as_ref().unwrap().package_a.package.artist_id.unwrap();
				let dto = NewExternalId {
					url: "a.b/c".to_string(),
					value: "c".to_string(),
					description: Some("This is a description".to_string()),
					rating: Some(101),
					provider_name: "TMDB".to_string(),
					artist_id: Some(artist_id.clone()),
					package_id: None,
				};
				let response = client
					.post("/external_ids")
					.header(Header::new(
						"X-API-Key",
						env::var("MATCHER_API_KEY").unwrap(),
					))
					.header(ContentType::JSON)
					.body(serde_json::to_value(dto.clone()).unwrap().to_string())
					.dispatch();
				assert_eq!(response.status(), Status::BadRequest);
			})
			.is_ok());
	}

	#[test]
	/// Test POST `/external_ids/` (both package_id and artist_id are given)
	fn test_external_id_both_parent_given() {
		let client = test_client().lock().unwrap();
		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let artist_id = &data.as_ref().unwrap().package_a.package.artist_id.unwrap();
				let dto = NewExternalId {
					url: "a.b/c".to_string(),
					value: "c".to_string(),
					description: Some("This is a description".to_string()),
					rating: Some(50),
					provider_name: "TMDB".to_string(),
					artist_id: Some(artist_id.clone()),
					package_id: Some(artist_id.clone()),
				};
				let response = client
					.post("/external_ids")
					.header(Header::new(
						"X-API-Key",
						env::var("MATCHER_API_KEY").unwrap(),
					))
					.header(ContentType::JSON)
					.body(serde_json::to_value(dto.clone()).unwrap().to_string())
					.dispatch();
				assert_eq!(response.status(), Status::BadRequest);
			})
			.is_ok());
	}

	#[test]
	/// Test POST `/external_ids/` (Package)
	fn test_external_id_package() {
		let client = test_client().lock().unwrap();
		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let package_id = &data.as_ref().unwrap().package_b.package.id;
				let dto = NewExternalId {
					url: "a.b/c".to_string(),
					value: "c".to_string(),
					description: Some("This is a package description".to_string()),
					rating: Some(50),
					provider_name: "TMDB".to_string(),
					artist_id: None,
					package_id: Some(package_id.clone()),
				};
				let response = client
					.post("/external_ids")
					.header(ContentType::JSON)
					.header(Header::new(
						"X-API-Key",
						env::var("MATCHER_API_KEY").unwrap(),
					))
					.body(serde_json::to_value(dto.clone()).unwrap().to_string())
					.dispatch();
				assert_eq!(response.status(), Status::Created);
				let value_response = response_json_value(response);

				let url = value_response.get("url").unwrap().as_str().unwrap();
				assert_eq!(url, dto.url);
				let provider_name = value_response
					.get("provider_name")
					.unwrap()
					.as_str()
					.unwrap();
				assert_eq!(provider_name, dto.provider_name);
				let value = value_response.get("value").unwrap().as_str().unwrap();
				assert_eq!(value, dto.value);
				let rating = value_response.get("rating").unwrap().as_i64().unwrap() as i16;
				assert_eq!(rating, dto.rating.unwrap());
				let description = value_response.get("description").unwrap().as_str().unwrap();
				assert_eq!(description, dto.description.unwrap());
				assert_eq!(
					value_response.get("package_id").unwrap().as_str().unwrap(),
					package_id.to_string()
				);
				assert_eq!(value_response.get("artist_id"), None);
			})
			.is_ok());
	}

	#[test]
	/// Test POST `/external_ids/` (Resource not found)
	fn test_external_id_parent_not_found() {
		let client = test_client().lock().unwrap();
		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let package_id = &data.as_ref().unwrap().package_a.package.artist_id.unwrap();
				let dto = NewExternalId {
					url: "a.b/c".to_string(),
					value: "c".to_string(),
					description: Some("This is a description".to_string()),
					rating: Some(50),
					provider_name: "TMDB".to_string(),
					artist_id: None,
					package_id: Some(package_id.clone()),
				};
				let response = client
					.post("/external_ids")
					.header(Header::new(
						"X-API-Key",
						env::var("MATCHER_API_KEY").unwrap(),
					))
					.header(ContentType::JSON)
					.body(serde_json::to_value(dto.clone()).unwrap().to_string())
					.dispatch();
				assert_eq!(response.status(), Status::NotFound);
			})
			.is_ok());
	}

	#[test]
	/// Test GET `/external_ids?artist=`
	fn test_get_external_id_artist() {
		let client = test_client().lock().unwrap();
		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let artist_id = &data.as_ref().unwrap().package_a.package.artist_id.unwrap();
				let response = client
					.get(format!("/external_ids?artist={}", artist_id))
					.header(ContentType::JSON)
					.dispatch();
				assert_eq!(response.status(), Status::Ok);
				let value = response_json_value(response);
				let items = value
					.as_object()
					.unwrap()
					.get("items")
					.unwrap()
					.as_array()
					.unwrap();
				assert_eq!(items.len(), 1);
				assert_eq!(
					items
						.get(0)
						.unwrap()
						.as_object()
						.unwrap()
						.get("description")
						.unwrap()
						.as_str()
						.unwrap(),
					"This is an artist description"
				);
			})
			.is_ok());
	}

	#[test]
	/// Test GET `/external_ids?package=`
	fn test_get_external_id_package() {
		let client = test_client().lock().unwrap();
		assert!(GLOBAL_DATA
			.lock()
			.inspect(|data| {
				let package_id = &data.as_ref().unwrap().package_b.package.id;
				let response = client
					.get(format!("/external_ids?package={}", package_id))
					.header(ContentType::JSON)
					.dispatch();
				assert_eq!(response.status(), Status::Ok);
				let value = response_json_value(response);
				let items = value
					.as_object()
					.unwrap()
					.get("items")
					.unwrap()
					.as_array()
					.unwrap();
				assert_eq!(items.len(), 1);
				assert_eq!(
					items
						.get(0)
						.unwrap()
						.as_object()
						.unwrap()
						.get("description")
						.unwrap()
						.as_str()
						.unwrap(),
					"This is a package description"
				);
			})
			.is_ok());
	}

	#[test]
	/// Test GET `/external_ids?package=&artist=`
	fn test_get_external_id_package_and_artist() {
		let client = test_client().lock().unwrap();
		let response = client
			.get(format!(
				"/external_ids?artist={}&package={}",
				uuid::Uuid::max(),
				uuid::Uuid::max()
			))
			.header(ContentType::JSON)
			.dispatch();
		assert_eq!(response.status(), Status::BadRequest);
	}

	#[test]
	/// Test GET `/external_ids?package=`
	fn test_get_external_id_parent_not_found() {
		let client = test_client().lock().unwrap();
		let response = client
			.get(format!("/external_ids?package={}", uuid::Uuid::max()))
			.header(ContentType::JSON)
			.dispatch();
		assert_eq!(response.status(), Status::Ok);
		let value = response_json_value(response);
		let items = value
			.as_object()
			.unwrap()
			.get("items")
			.unwrap()
			.as_array()
			.unwrap();
		assert_eq!(items.len(), 0);
	}

	#[test]
	/// Test GET `/external_ids?skip=`
	fn test_get_external_id_pagination() {
		let client = test_client().lock().unwrap();
		let response = client
			.get("/external_ids?skip=1")
			.header(ContentType::JSON)
			.dispatch();
		assert_eq!(response.status(), Status::Ok);
		let value = response_json_value(response);
		let items = value
			.as_object()
			.unwrap()
			.get("items")
			.unwrap()
			.as_array()
			.unwrap();
		assert_eq!(items.len(), 1);
	}
}
