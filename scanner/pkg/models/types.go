package models

type ExtraType int64

const (
	AlternateView ExtraType = iota
	Backdrops
	BehindTheScenes
	Interview
	MusicVideo
	Other
	Performance
	Trailer
)

type MovieType int64

const (
	Concert MovieType = iota
	Documentary
)
