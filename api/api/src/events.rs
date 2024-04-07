use amqprs::callbacks::{DefaultChannelCallback, DefaultConnectionCallback};
use amqprs::channel::{BasicPublishArguments, QueueBindArguments, QueueDeclareArguments};
use amqprs::BasicProperties;
use deadpool_amqprs::Pool;
use migration::EVENT_CHANNEL;
use rocket::tokio::spawn;
use rocket::{fairing, Build, Rocket};
use sea_orm_rocket::Database;
use serde::Deserialize;
use sqlx::postgres::PgListener;

use crate::database::Db;

#[derive(Deserialize, Debug)]
pub enum ActionType {
	INSERT,
	UPDATE,
	DELETE,
}

#[derive(Deserialize, Debug)]
pub struct EventPayload {
	pub table: String,
	pub action_type: ActionType,
	pub id: String,
	pub name: String,
}

pub async fn hook_psql_events(rocket: Rocket<Build>, rabbit_pool: Pool) -> fairing::Result {
	let pool = Db::fetch(&rocket)
		.unwrap()
		.conn
		.get_postgres_connection_pool();
	let mut listener = PgListener::connect_with(&pool).await.unwrap();

	let _ = listener.listen(EVENT_CHANNEL).await;
	spawn(async move {
		let rabbit_connection = rabbit_pool.get().await.unwrap();
		rabbit_connection
			.register_callback(DefaultConnectionCallback)
			.await
			.unwrap();

		let rabbit_channel = rabbit_connection.open_channel(None).await.unwrap();
		rabbit_channel
			.register_callback(DefaultChannelCallback)
			.await
			.unwrap();
		let (queue_name, _, _) = rabbit_channel
			.queue_declare(QueueDeclareArguments::durable_client_named(
				"amqprs.examples.basic",
			))
			.await
			.unwrap()
			.unwrap();
		let routing_key = "amqprs.example";
		let exchange_name = "amq.topic";
		rabbit_channel
			.queue_bind(QueueBindArguments::new(
				&queue_name,
				exchange_name,
				routing_key,
			))
			.await
			.unwrap();
		loop {
			while let Some(notification) = listener.try_recv().await.unwrap() {
				let args = BasicPublishArguments::new(exchange_name, routing_key);
				let strr = notification.payload().to_owned();
				let _ = serde_json::from_str::<EventPayload>(&strr).unwrap();
				rabbit_channel
					.basic_publish(BasicProperties::default(), strr.into(), args)
					.await
					.unwrap();
			}
		}
	});
	Ok(rocket)
}
