-- Your SQL goes here

-- Slug Function
CREATE EXTENSION IF NOT EXISTS "unaccent";

CREATE OR REPLACE FUNCTION slugify("value" TEXT)
RETURNS TEXT AS $$
  -- removes accents (diacritic signs) from a given string --
  WITH "unaccented" AS (
    SELECT unaccent("value") AS "value"
  ),
  -- lowercases the string
  "lowercase" AS (
    SELECT lower("value") AS "value"
    FROM "unaccented"
  ),
  -- remove single and double quotes
  "removed_quotes" AS (
    SELECT regexp_replace("value", '[''"]+', '', 'gi') AS "value"
    FROM "lowercase"
  ),
  -- replaces anything that's not a letter, number, hyphen('-'), or underscore('_') with a hyphen('-')
  "hyphenated" AS (
    SELECT regexp_replace("value", '[^a-z0-9\\-_]+', '-', 'gi') AS "value"
    FROM "removed_quotes"
  ),
  -- trims hyphens('-') if they exist on the head or tail of the string
  "trimmed" AS (
    SELECT regexp_replace(regexp_replace("value", '\-+$', ''), '^\-', '') AS "value"
    FROM "hyphenated"
  )
  SELECT "value" FROM "trimmed";
$$ LANGUAGE SQL STRICT IMMUTABLE;

CREATE TYPE "chapter-types" AS ENUM (
	'Performance',
	'Interview',
	'NonMusicalInterview',
	'Other'
);

CREATE TYPE "movie-types" AS ENUM ('Concert', 'Documentary');

CREATE TYPE "extra-types" AS ENUM (
	'Trailer',
	'Interview',
	'BehindTheScenes',
	'MusicVideo',
	'AlternateView',
	'Backdrops',
	'Performance',
	'Other'
);

CREATE TYPE "image-types" AS ENUM ('Poster', 'Banner', 'Thumbnail');

CREATE TYPE "video-qualities" AS ENUM (
	'8k',
	'4k',
	'2k',
	'1080p',
	'720p',
	'480p',
	'360p',
	'240p',
	'Other'
);


CREATE TABLE artists (
	"id" uuid DEFAULT gen_random_uuid(),
	"name" VARCHAR NOT NULL,
	"description" TEXT,
	"slug" VARCHAR GENERATED ALWAYS AS (slugify(name)) STORED UNIQUE NOT NULL,
	PRIMARY KEY (id)
);

CREATE TABLE images (
	"id" uuid DEFAULT gen_random_uuid(),
	"blurhash" VARCHAR NOT NULL,
	"colors" TEXT [] NOT NULL,
	"aspect_ratio" FLOAT NOT NULL,
	"type" "image-types" NOT NULL,
	PRIMARY KEY (id)
);

CREATE TABLE files (
	"id" uuid DEFAULT gen_random_uuid(),
	"size" bigserial NOT NULL,
	"path" TEXT NOT NULL,
	"quality" "video-qualities" NOT NULL,
	"scrubber_id" uuid NOT NULL REFERENCES images (id),
	PRIMARY KEY (id)
);

CREATE TABLE packages (
	"id" uuid DEFAULT gen_random_uuid(),
	"description" TEXT,
	"release-year" DATE,
	"artist_id" uuid REFERENCES artists (id),
	"poster_id" uuid REFERENCES images (id),
	"banner_id" uuid REFERENCES images (id),
	PRIMARY KEY (id)
);

CREATE TABLE extras (
	"id" uuid DEFAULT gen_random_uuid(),
	"name" TEXT NOT NULL,
	"thumbnail_id" uuid REFERENCES images (id),
	"package_id" uuid NOT NULL REFERENCES packages (id),
	"artist_id" uuid NOT NULL REFERENCES artists (id),
	"file_id" uuid NOT NULL REFERENCES files (id),
	"disc_index" smallserial,
	"track_index" smallserial,
	"type" "extra-types" [] NOT NULL,
	PRIMARY KEY (id)
);

CREATE TABLE movies (
	"id" uuid DEFAULT gen_random_uuid(),
	"name" TEXT NOT NULL,
	"poster_id" uuid REFERENCES images (id),
	"package_id" uuid NOT NULL REFERENCES packages (id),
	"artist_id" uuid NOT NULL REFERENCES artists (id),
	"file_id" uuid NOT NULL REFERENCES files (id),
	"disc_index" smallserial,
	"track_index" smallserial,
	"type" "movie-types" [] NOT NULL,
	PRIMARY KEY (id)
);

CREATE TABLE chapters (
	"id" uuid DEFAULT gen_random_uuid(),
	"name" TEXT NOT NULL,
	"thumbnail_id" uuid REFERENCES images (id),
	"movie_id" uuid NOT NULL REFERENCES movies (id),
	"index" smallserial NOT NULL,
	"start_time" smallserial NOT NULL,
	"end_time" smallserial NOT NULL,
	"type" "chapter-types" [] NOT NULL,
	PRIMARY KEY (id)
);