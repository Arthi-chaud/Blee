-- Your SQL goes here

CREATE TABLE images (
	"id" uuid DEFAULT gen_random_uuid(),
    "blurhash" VARCHAR NOT NULL,
	"colors" TEXT[] NOT NULL,
	"type" "image-types" NOT NULL,
	PRIMARY KEY (id)
)