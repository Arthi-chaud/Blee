package models

import "time"

type NewFileDto struct {
	Path     string `validate:"required" json:"path"`
	Size     uint64 `validate:"required" json:"size"`
	Duration uint64 `validate:"required" json:"duration"`
	Quality  string `validate:"required" json:"quality"`
}

type NewMovieDto struct {
	ArtistName         string          `validate:"required" json:"artist_name"`
	MovieName          string          `validate:"required" json:"movie_name"`
	MovieType          string          `validate:"required" json:"movie_type"`
	PackageArtistName  string          `json:"package_artist_name"`
	PackageName        string          `validate:"required" json:"package_name"`
	PackageReleaseDate time.Time       `json:"package_release_date"`
	Chapters           []NewChapterDto `validate:"required,dive,required" json:"chapters"`
	File               NewFileDto      `validate:"required" json:"file"`
}

type NewChapterDto struct {
	Name           string   `validate:"required" json:"name"`
	StartTimestamp uint64   `json:"start_timestamp"`
	EndTimestamp   uint64   `validate:"required" json:"end_timestamp"`
	Types          []string `validate:"required" json:"types"`
}

type NewExtraDto struct {
	ArtistName         string     `validate:"required" json:"artist_name"`
	ExtraName          string     `validate:"required" json:"extra_name"`
	Types              []string   `validate:"required" json:"types"`
	PackageArtistName  string     `json:"package_artist_name"`
	PackageName        string     `validate:"required" json:"package_name"`
	DiscIndex          int        `json:"disc_index"`
	TrackIndex         int        `json:"track_index"`
	PackageReleaseDate time.Time  `json:"package_release_date"`
	File               NewFileDto `validate:"required" json:"file"`
}
