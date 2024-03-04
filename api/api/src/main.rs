use api;
use rocket;

#[rocket::main]
async fn main() -> Result<(), rocket::Error> {
    let _ = api::rocket().launch().await;

	Ok(())
}