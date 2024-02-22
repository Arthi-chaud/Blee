-- Your SQL goes here

CREATE TYPE "chapter-types" AS ENUM ('Performance', 'Interview', 'NonMusicalInterview', 'Other');

CREATE TYPE "movie-types" AS ENUM ('Concert', 'Documentary');

CREATE TYPE "extra-types" AS ENUM ('Trailer', 'Interview', 'BehindTheScenes', 'MusicVideo', 'AlternateView', 'Backdrops', 'Performance', 'Other');

CREATE TYPE "image-types" AS ENUM ('Poster', 'Banner', 'Thumbnail');

CREATE TYPE "video-qualities" AS ENUM ('8k', '4k', '2k', '1080p', '720p', '480p', '360p', '240p', 'Other');
