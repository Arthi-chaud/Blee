-- This file should undo anything in `up.sql`
DROP EXTENSION "unaccent";

DROP TABLE chapters;

DROP TABLE extras;
DROP TABLE movies;
DROP TABLE packages;

DROP TABLE files;
DROP TABLE artists;
DROP TABLE images;

DROP TYPE "chapter-types";
DROP TYPE "movie-types";
DROP TYPE "extra-types";
DROP TYPE "image-types";
DROP TYPE "video-qualities";

DROP FUNCTION slugify;