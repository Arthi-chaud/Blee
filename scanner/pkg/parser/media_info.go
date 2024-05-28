package parser

import (
	"context"
	"errors"

	"github.com/Arthi-chaud/Blee/scanner/pkg"
	"github.com/Arthi-chaud/Blee/scanner/pkg/models"
	"github.com/kpango/glg"
	"gopkg.in/vansante/go-ffprobe.v2"
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
	ctx, cancelFn := context.WithCancel(context.Background())
	defer cancelFn()

	probe, err := ffprobe.ProbeURL(ctx, path)
	if err != nil {
		return MediaInfo{}, err
	}
	glg.Print(probe, probe.Format, probe.Streams, probe.Chapters)

	videoStream := probe.FirstVideoStream()
	if videoStream == nil {
		return MediaInfo{}, errors.New("no video stream found")
	}
	info := MediaInfo{
		Quality:  qualityFromHeight(uint64(videoStream.Height)),
		Size:     pkg.ParseUint64(probe.Format.Size),
		Duration: uint64(probe.Format.DurationSeconds),
		Chapters: formatChapters(probe),
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

func formatChapters(probe *ffprobe.ProbeData) []MediaChapter {
	if probe == nil {
		return []MediaChapter{}
	}
	chapterCount := len(probe.Chapters)
	chapters := make([]MediaChapter, len(probe.Chapters))

	for i := 0; i < chapterCount; i++ {
		currentChapter := probe.Chapters[i]
		chapters[i] = MediaChapter{
			Name:      currentChapter.Tags.Title,
			StartTime: uint32(currentChapter.StartTimeSeconds),
			EndTime:   uint32(currentChapter.EndTimeSeconds),
			// TODO
			Types: []models.ChapterType{models.COther},
		}
	}
	return chapters
}
