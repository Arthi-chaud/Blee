package parser

import (
	"github.com/Arthi-chaud/Blee/scanner/pkg"
	"github.com/Arthi-chaud/Blee/scanner/pkg/models"
	"github.com/zoriya/go-mediainfo"
)

type MediaInfo struct {
	Quality  models.Quality `validate:"required"`
	Size     uint64         `validate:"required"`
	Duration uint64         `validate:"required"`
	Chapters []MediaChapter `validate:"required,dive"`
}

type MediaChapter struct {
	Name      string               `validate:"required"`
	StartTime uint32               `validate:"required"`
	EndTime   uint32               `validate:"required"`
	Types     []models.ChapterType `validate:"required,dive,required"`
}

func GetMediaInfo(path string) (MediaInfo, error) {
	mi, err := mediainfo.Open(path)
	if err != nil {
		return MediaInfo{}, err
	}
	defer mi.Close()

	chapters_begin := pkg.ParseUint64(mi.Parameter(mediainfo.StreamMenu, 0, "Chapters_Pos_Begin"))
	chapters_end := pkg.ParseUint64(mi.Parameter(mediainfo.StreamMenu, 0, "Chapters_Pos_End"))

	// SRC: https://github.com/zoriya/Kyoo/blob/master/transcoder/src/info.go
	info := MediaInfo{
		Quality:  qualityFromHeight(pkg.ParseUint64(mi.Parameter(mediainfo.StreamVideo, 0, "Height"))),
		Size:     pkg.ParseUint64(mi.Parameter(mediainfo.StreamGeneral, 0, "FileSize")),
		Duration: uint64(pkg.ParseFloat(mi.Parameter(mediainfo.StreamGeneral, 0, "Duration")) / 1000),
		Chapters: pkg.Map(make([]MediaChapter, max(chapters_end-chapters_begin, 1)-1), func(_ MediaChapter, i int) MediaChapter {
			return MediaChapter{
				StartTime: uint32(pkg.ParseTime(mi.GetI(mediainfo.StreamMenu, 0, int(chapters_begin)+i, mediainfo.InfoName))),
				EndTime:   uint32(pkg.ParseTime(mi.GetI(mediainfo.StreamMenu, 0, int(chapters_begin)+i+1, mediainfo.InfoName))),
				Name:      mi.GetI(mediainfo.StreamMenu, 0, int(chapters_begin)+i, mediainfo.InfoText),
				//TODO
				Types: []models.ChapterType{models.COther},
			}
		}),
	}
	return info, nil
}

func qualityFromHeight(height uint64) models.Quality {
	qualities := models.Qualities
	for _, quality := range qualities {
		if quality.Height() >= height {
			return quality
		}
	}
	return models.Other_
}
