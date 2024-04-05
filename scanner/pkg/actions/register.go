package actions

import (
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
	}
	var resourceUuid = ""
	if parsedPath.Movie != nil {
		dto, err := buildMovieDto(path, parsedPath.Movie, mediainfo)

		if err != nil {
			glg.Failf(err.Error())
			return err
		}
		res, err := api.SaveMovie(&dto, *c)
		resourceUuid = res.MovieId
		if err != nil {
			glg.Failf(err.Error())
			return err
		}
	} else if parsedPath.Extra != nil {
		dto, err := buildExtraDto(path, parsedPath.Extra, mediainfo)

		if err != nil {
			glg.Failf(err.Error())
			return err
		}
		res, err := api.SaveExtra(&dto, *c)
		resourceUuid = res.ExtraId
		if err != nil {
			glg.Failf(err.Error())
			return err
		}
	}
	thumbnailBytes, err := pkg.GetFrame(path, int64(mediainfo.Duration) / 2)
	if (err != nil) {
		glg.Failf(err.Error())
		return err
	}
	if (parsedPath.Extra != nil) {
		err = api.SaveExtraThumbnail(resourceUuid, thumbnailBytes, *c)
	} else if (parsedPath.Movie != nil) {
		err = api.SaveMovieThumbnail(resourceUuid, thumbnailBytes, *c)
	}
	return err
}

func buildMovieDto(path string, parsedPath *parser.MovieMetadataFromPath, mediainfo *parser.MediaInfo) (models.NewMovieDto, error) {
	validate := validator.New(validator.WithRequiredStructEnabled())
	dto := models.NewMovieDto{
		ArtistName:         parsedPath.ArtistName,
		MovieName:          parsedPath.Name,
		MovieType:          string(parsedPath.Type_),
		PackageArtistName:  parsedPath.Package_.ArtistName,
		PackageName:        parsedPath.Package_.Name,
		PackageReleaseDate: parsedPath.Package_.ReleaseYear,
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
	dto := models.NewExtraDto{
		ArtistName: parsedPath.ArtistName,
		ExtraName:  parsedPath.Name,
		Types: pkg.Map(make([]string, len(parsedPath.Types)), func(_ string, i int) string {
			return string(parsedPath.Types[i])
		}),
		PackageArtistName:  parsedPath.Package_.ArtistName,
		PackageName:        parsedPath.Package_.Name,
		PackageReleaseDate: parsedPath.Package_.ReleaseYear,
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
