use rocket_okapi::okapi::openapi3::OpenApi;

pub fn custom_openapi_spec() -> OpenApi {
	use rocket_okapi::okapi::openapi3::*;
	OpenApi {
		openapi: OpenApi::default_version(),
		info: Info {
			title: "Blee API".to_owned(),
			description: Some("Documentation of the available endpoints of the API".to_owned()),
			terms_of_service: None,
			contact: None,
			license: Some(License {
				name: "GPL License".to_owned(),
				url: Some("https://github.com/Arthi-chaud/Blee/blob/main/LICENSE".to_owned()),
				..Default::default()
			}),
			..Default::default()
		},
		servers: vec![
			Server {
				url: "/".to_owned(),
				description: Some("Localhost".to_owned()),
				..Default::default()
			},
			Server {
				url: "/api".to_owned(),
				description: Some("Deployed App".to_owned()),
				..Default::default()
			},
		],
		..Default::default()
	}
}
