package parser

import (
	"strings"
	"unicode"

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
	duration := uint64(pkg.ParseFloat(mi.Parameter(mediainfo.StreamGeneral, 0, "Duration")) / 1000)

	// SRC: https://github.com/zoriya/Kyoo/blob/master/transcoder/src/info.go
	info := MediaInfo{
		Quality:  qualityFromHeight(pkg.ParseUint64(mi.Parameter(mediainfo.StreamVideo, 0, "Height"))),
		Size:     pkg.ParseUint64(mi.Parameter(mediainfo.StreamGeneral, 0, "FileSize")),
		Duration: duration,
		Chapters: getChapters(uint32(chapters_begin), uint32(chapters_end), mi, duration),
	}
	return info, nil
}

func parseChapterName(name string) string {
	index := strings.Index(name, ":")
	if index == -1 {
		return name
	}
	return name[index+1:]
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

func chapterTimeIsValid(chapterTime string) bool {
	return len(chapterTime) > 0 && unicode.IsDigit(rune(chapterTime[0]))
}

func getChapters(chapters_begin uint32, chapters_end uint32, mi *mediainfo.File, duration uint64) []MediaChapter {
	chapterCount := max(chapters_end-chapters_begin, 0)
	chapterIterationCount := chapterCount
	chapters := make([]MediaChapter, chapterCount)
	chapterIndex := 0

	for i := 0; i < int(chapterIterationCount); i++ {
		rawStartTime := mi.GetI(mediainfo.StreamMenu, 0, int(chapters_begin)+i, mediainfo.InfoName)
		rawEndTime := mi.GetI(mediainfo.StreamMenu, 0, int(chapters_begin)+i+1, mediainfo.InfoName)
		// If true, this "chapter" is invalid. We skip it
		if !chapterTimeIsValid(rawStartTime) {
			chapterIterationCount = chapterIterationCount + 1
			continue
		}
		var endTime uint32
		// If this fails, we probably are at the end of the video
		// Since there would be no following chapter,
		// we defacto set the end time to the end of the video (i.e. its duration)
		if chapterTimeIsValid(rawEndTime) {
			endTime = uint32(pkg.ParseTime(rawEndTime))
		} else {
			endTime = uint32(duration)
		}
		chapters[chapterIndex] = MediaChapter{
			StartTime: uint32(pkg.ParseTime(rawStartTime)),
			EndTime:   endTime,
			Name:      parseChapterName(mi.GetI(mediainfo.StreamMenu, 0, int(chapters_begin)+i, mediainfo.InfoText)),
			// TODO
			Types: []models.ChapterType{models.COther},
		}
		chapterIndex++
	}
	return chapters
}
