package actions

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"

	"github.com/Arthi-chaud/Blee/scanner/pkg"
	"github.com/Arthi-chaud/Blee/scanner/pkg/api"
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/Arthi-chaud/Blee/scanner/pkg/models"
	"github.com/Arthi-chaud/Blee/scanner/pkg/parser"
	"github.com/go-playground/validator/v10"
	"github.com/kpango/glg"
)

// Register File to API
func RegisterFile(path string, c *config.Config) error {
	glg.Logf("File to scan %s", path)
	parsedPath, err := parser.ParseMetadataFromPath(path, &c.UserConfig)
	if err != nil {
		glg.Failf("Parsing Path of '%s' failed:", filepath.Base(path))
		glg.Fail(err)
	}
	mediainfo, err := parser.GetMediaInfo(path)
	if err != nil {
		glg.Failf("Getting MediaInfo on '%s' failed:", filepath.Base(path))
		glg.Fail(err)
		return err
	}
	var resourceUuid = ""
	var packageUuid = ""
	if parsedPath.Movie != nil {
		dto, err := buildMovieDto(path, parsedPath.Movie, &mediainfo)
		if err != nil {
			glg.Failf(err.Error())
			return err
		}
		res, err := api.SaveMovie(&dto, *c)
		resourceUuid = res.MovieId
		packageUuid = res.PackageId
		if err != nil {
			glg.Failf(err.Error())
			return err
		}
		for i, chapterId := range res.ChaptersId {
			thumbnailBytes, err := pkg.GetFrame(path, int64(dto.Chapters[i].StartTimestamp)+int64(dto.Chapters[i].EndTimestamp-dto.Chapters[i].StartTimestamp)/2)
			api.SaveChapterThumbnail(chapterId, thumbnailBytes, *c)
			if err != nil {
				glg.Fail(err)
			}
		}
	} else if parsedPath.Extra != nil {
		dto, err := buildExtraDto(path, parsedPath.Extra, &mediainfo)

		if err != nil {
			glg.Failf(err.Error())
			return err
		}
		res, err := api.SaveExtra(&dto, *c)
		resourceUuid = res.ExtraId
		packageUuid = res.PackageId
		if err != nil {
			glg.Failf(err.Error())
			return err
		}
	}
	thumbnailBytes, err := pkg.GetFrame(path, int64(mediainfo.Duration)/2)
	if err != nil {
		glg.Failf(err.Error())
		return err
	}
	if parsedPath.Extra != nil {
		err = api.SaveExtraThumbnail(resourceUuid, thumbnailBytes, *c)
	} else if parsedPath.Movie != nil {
		err = api.SaveMovieThumbnail(resourceUuid, thumbnailBytes, *c)
	}
	posterPath := pkg.GetPosterPathInFolder(filepath.Dir(path))
	if len(posterPath) == 0 {
		return err
	}
	parentPackage, err := api.GetPackage(packageUuid, *c)
	if err != nil {
		glg.Failf(err.Error())
		return err
	}
	if len(parentPackage.PosterId) != 0 {
		return err
	}
	glg.Log("Found a Poster to save!")
	bs, err := os.ReadFile(posterPath)
	if err != nil {
		glg.Fail("Could not open Poster filer")
		return err
	}
	if e := api.SavePackagePoster(packageUuid, bytes.NewReader(bs), *c); e != nil {
		glg.Fail("Could not POST poster.")
		glg.Fail(e.Error())
		return e
	}
	return err
}

func buildMovieDto(path string, parsedPath *parser.MovieMetadataFromPath, mediainfo *parser.MediaInfo) (models.NewMovieDto, error) {
	validate := validator.New(validator.WithRequiredStructEnabled())
	var stringYear string = ""
	year := parsedPath.Package_.ReleaseYear
	if year.Year() != 1 {
		stringYear = fmt.Sprintf("%d-%02d-%02d", year.Year(), year.Month(), year.Day())
	}
	dto := models.NewMovieDto{
		ArtistName:         parsedPath.ArtistName,
		MovieName:          parsedPath.Name,
		MovieType:          string(parsedPath.Type_),
		PackageArtistName:  parsedPath.Package_.ArtistName,
		PackageName:        parsedPath.Package_.Name,
		PackageReleaseDate: stringYear,
		Chapters: pkg.Map(make([]models.NewChapterDto, len(mediainfo.Chapters)), func(_ models.NewChapterDto, i int) models.NewChapterDto {
			return models.NewChapterDto{
				Name:           mediainfo.Chapters[i].Name,
				StartTimestamp: uint64(mediainfo.Chapters[i].StartTime),
				EndTimestamp:   uint64(mediainfo.Chapters[i].EndTime),
				Types: pkg.Map(make([]string, len(mediainfo.Chapters[i].Types)), func(_ string, y int) string {
					return string(mediainfo.Chapters[i].Types[y])
				}),
			}
		}),
		File: buildFileDto(path, mediainfo),
	}
	if err := validate.Struct(dto); err != nil {
		return models.NewMovieDto{}, err.(validator.ValidationErrors)
	}
	return dto, nil
}

func buildExtraDto(path string, parsedPath *parser.ExtraMetadataFromPath, mediainfo *parser.MediaInfo) (models.NewExtraDto, error) {
	validate := validator.New(validator.WithRequiredStructEnabled())
	var stringYear string = ""
	year := parsedPath.Package_.ReleaseYear
	if year.Year() != 1 {
		stringYear = fmt.Sprintf("%d-%02d-%02d", year.Year(), year.Month(), year.Day())
	}
	dto := models.NewExtraDto{
		ArtistName: parsedPath.ArtistName,
		ExtraName:  parsedPath.Name,
		Types: pkg.Map(make([]string, len(parsedPath.Types)), func(_ string, i int) string {
			return string(parsedPath.Types[i])
		}),
		PackageArtistName:  parsedPath.Package_.ArtistName,
		PackageName:        parsedPath.Package_.Name,
		PackageReleaseDate: stringYear,
		DiscIndex:          parsedPath.DiscIndex,
		TrackIndex:         parsedPath.TrackIndex,
		File:               buildFileDto(path, mediainfo),
	}
	if err := validate.Struct(dto); err != nil {
		return models.NewExtraDto{}, err.(validator.ValidationErrors)
	}
	return dto, nil
}

func buildFileDto(path string, mediainfo *parser.MediaInfo) models.NewFileDto {
	return models.NewFileDto{
		Path:     path,
		Size:     mediainfo.Size,
		Duration: mediainfo.Duration,
		Quality:  string(mediainfo.Quality),
	}
}
