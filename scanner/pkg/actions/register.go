package actions

import (
	"path/filepath"

	"github.com/Arthi-chaud/Blee/scanner/pkg"
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
	if parsedPath.Movie != nil {
		dto, err := buildMovieDto(path, parsedPath.Movie, mediainfo)

		if err != nil {
			glg.Failf(err.Error())
		} else {
			glg.Fail(dto)
		}
	} else if parsedPath.Extra != nil {

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
		File: models.NewFileDto{
			Path:     path,
			Size:     mediainfo.Size,
			Duration: mediainfo.Duration,
			Quality:  string(mediainfo.Quality),
		},
	}
	if err := validate.Struct(dto); err != nil {
		return models.NewMovieDto{}, err.(validator.ValidationErrors)
	}
	return dto, nil
}
