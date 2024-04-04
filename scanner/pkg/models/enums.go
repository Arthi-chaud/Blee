package models

type ChapterType string

const (
	CInterview           = "interview"
	CNonMusicalInterview = "non_musical_interview"
	COther               = "other"
	CPerformance         = "performance"
)

type ExtraType string

const (
	AlternateView   = "alternate_view"
	Backdrops       = "backdrops"
	BehindTheScenes = "behind_the_scenes"
	Interview       = "interview"
	MusicVideo      = "music_video"
	Other           = "other"
	Performance     = "performance"
	Trailer         = "trailer"
)

type MovieType string

const (
	Concert     = "concert"
	Documentary = "documentary"
)

type Quality string

const (
	Other_ Quality = "other"
	P240   Quality = "240p"
	P360   Quality = "360p"
	P480   Quality = "480p"
	P576   Quality = "576p"
	P720   Quality = "720p"
	P1080  Quality = "1080p"
	P2k    Quality = "2k"
	P4k    Quality = "4k"
)

func (q Quality) Height() uint64 {
	switch q {
	case P240:
		return 240
	case P360:
		return 360
	case P480:
		return 480
	case P576:
		return 576
	case P720:
		return 720
	case P1080:
		return 1080
	case P2k:
		return 1440
	case P4k:
		return 2160
	}
	panic("Invalid quality value")
}

var Qualities = []Quality{P240, P360, P480, P720, P1080, P2k, P4k, Other_}
