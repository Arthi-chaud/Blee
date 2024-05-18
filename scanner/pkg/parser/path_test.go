package parser

import (
	"testing"
	"time"

	config "github.com/Arthi-chaud/Blee/scanner/pkg/config"
	models "github.com/Arthi-chaud/Blee/scanner/pkg/models"
	"github.com/stretchr/testify/assert"
)

func getTestConfig() *config.UserConfiguration {
	var config config.UserConfiguration

	config.Regexes.Extra = []string{
		"[\\/\\\\]videos[\\/\\\\]((?P<PackageArtist>[^\\/\\\\]+)((\\s+-\\s+)|([\\/\\\\]+))(([^\\/\\\\]+)\\s+-\\s+)?((?P<Package>[^\\/\\\\]+?)))(\\s+\\((?P<PackageYear>\\d+)\\))?[\\/\\\\]+(Extra(s)?[\\/\\\\]+)(((?P<Disc>\\d+)-)?(?P<Index>\\d+)\\s+)?(?P<Extra>[^\\/\\\\]+?)(?P<PlexExtraType>-[a-z]+)?\\..+",
		"[\\/\\\\]videos[\\/\\\\]((?P<PackageArtist>[^\\/\\\\]+)((\\s+-\\s+)|([\\/\\\\]+))(([^\\/\\\\]+)\\s+-\\s+)?((?P<Package>[^\\/\\\\]+?)))(\\s+\\((?P<PackageYear>\\d+)\\))?[\\/\\\\]+(Extra(s)?[\\/\\\\]+)?(((?P<Disc>\\d+)-)?(?P<Index>\\d+)\\s+)?(?P<Extra>[^\\/\\\\]+?)(?P<PlexExtraType>-[a-z]+)\\..+",
		"[\\/\\\\]videos[\\/\\\\]((?P<PackageArtist>[^\\/\\\\]+)((\\s+-\\s+)|([\\/\\\\]+))(([^\\/\\\\]+)\\s+-\\s+)?((?P<Package>[^\\/\\\\]+?)))(\\s+\\((?P<PackageYear>\\d+)\\))?[\\/\\\\]+(Extra(s)?[\\/\\\\]+)?(((?P<Disc>\\d+)-)?(?P<Index>\\d+)\\s+)(?P<Extra>[^\\/\\\\]+?)(?P<PlexExtraType>-[a-z]+)?\\..+",
	}
	config.Regexes.Movie = []string{
		"[\\/\\\\]videos[\\/\\\\](?P<PackageArtist>[^\\/\\\\]+)([\\/\\\\]+|(\\s+-\\s+))(?P<Package>[^\\/\\\\]+?)(\\s+\\((?P<PackageYear>\\d+)\\))?[\\/\\\\]+((?P<Artist>[^\\/\\\\]+)\\s+-\\s+)?(?P<Movie>[^\\/\\\\]+?)(\\s+\\((?P<Year>\\d+)\\))?\\..+",
		"[\\/\\\\]videos[\\/\\\\](?P<PackageArtist>(?P<Artist>[^\\/\\\\]+))(\\s+-\\s+)(?P<Package>(?P<Movie>[^\\/\\\\]+?))(\\s+\\((?P<PackageYear>(?P<Year>\\d+))\\))?\\..+",
	}
	return &config
}

func TestMovieParsingWithoutPackageFolderWithYear(t *testing.T) {
	config := getTestConfig()
	path := "/videos/The Corrs - Unplugged (1999).mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Nil(t, err)
	assert.Nil(t, res.Extra)
	assert.Equal(t, "The Corrs", res.Movie.ArtistName)
	assert.Equal(t, "Unplugged", res.Movie.Name)
	assert.Equal(t, models.MovieType(models.Concert), res.Movie.Type_)
	assert.Equal(t, "The Corrs", res.Movie.Package_.ArtistName)
	assert.Equal(t, "Unplugged", res.Movie.Package_.Name)
	assert.Equal(t, 1999, res.Movie.Package_.ReleaseYear.Year())
}

func TestMovieParsingWithoutPackageFolderWithFestival(t *testing.T) {
	config := getTestConfig()
	path := "/videos/Lady Gaga - iTunes Festival.mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Nil(t, err)
	assert.Nil(t, res.Extra)
	assert.Equal(t, "Lady Gaga", res.Movie.ArtistName)
	assert.Equal(t, "iTunes Festival", res.Movie.Name)
	assert.Equal(t, models.MovieType(models.Concert), res.Movie.Type_)
	assert.Equal(t, "Lady Gaga", res.Movie.Package_.ArtistName)
	assert.Equal(t, "iTunes Festival", res.Movie.Package_.Name)
}

func TestMovieParsingWithPackageFolderWithoutYear(t *testing.T) {
	config := getTestConfig()
	path := "/videos/The Corrs - Unplugged/The Corrs - Unplugged.mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Nil(t, err)
	assert.Nil(t, res.Extra)
	assert.Equal(t, res.Movie.ArtistName, "The Corrs")
	assert.Equal(t, res.Movie.Name, "Unplugged")
	assert.Equal(t, res.Movie.Type_, models.MovieType(models.Concert))
	assert.Equal(t, res.Movie.Package_.ArtistName, "The Corrs")
	assert.Equal(t, res.Movie.Package_.Name, "Unplugged")
	assert.Equal(t, res.Movie.Package_.ReleaseYear, time.Time{})
}

func TestMovieParsingWithPackageFolderWithArtistFolderWithMovieYear(t *testing.T) {
	config := getTestConfig()
	path := "/videos/The Corrs/Unplugged (1999)/The Corrs - Unplugged.mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Nil(t, err)
	assert.Nil(t, res.Extra)
	assert.Equal(t, res.Movie.ArtistName, "The Corrs")
	assert.Equal(t, res.Movie.Name, "Unplugged")
	assert.Equal(t, res.Movie.Type_, models.MovieType(models.Concert))
	assert.Equal(t, res.Movie.Package_.ArtistName, "The Corrs")
	assert.Equal(t, res.Movie.Package_.Name, "Unplugged")
	assert.Equal(t, res.Movie.Package_.ReleaseYear.Year(), 1999)
}

func TestExtraParsingWithPackageFolderWithoutArtistFolder(t *testing.T) {
	config := getTestConfig()
	path := "/videos/Olivia Ruiz - Miss Météores Live (2008)/Autour D'Olivia (Documentaire)-interview.mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Nil(t, err)
	assert.Nil(t, res.Movie)
	assert.Equal(t, res.Extra.ArtistName, "Olivia Ruiz")
	assert.Equal(t, res.Extra.DiscIndex, 0)
	assert.Equal(t, res.Extra.TrackIndex, 0)
	assert.Equal(t, res.Extra.Name, "Autour D'Olivia (Documentaire)")
	assert.Equal(t, res.Extra.Types, []models.ExtraType{models.ExtraType(models.BehindTheScenes)})
	assert.Equal(t, res.Extra.Package_.ArtistName, "Olivia Ruiz")
	assert.Equal(t, res.Extra.Package_.Name, "Miss Météores Live")
	assert.Equal(t, res.Extra.Package_.ReleaseYear.Year(), 2008)
}

func TestExtraParsingWithPackageFolderWithArtistFolder(t *testing.T) {
	config := getTestConfig()
	path := "/videos/Olivia Ruiz/Chocolat Show/Interview-interview.mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Nil(t, err)
	assert.Nil(t, res.Movie)
	assert.Equal(t, res.Extra.ArtistName, "Olivia Ruiz")
	assert.Equal(t, res.Extra.DiscIndex, 0)
	assert.Equal(t, res.Extra.TrackIndex, 0)
	assert.Equal(t, res.Extra.Name, "Interview")
	assert.Equal(t, res.Extra.Types, []models.ExtraType{models.Interview})
	assert.Equal(t, res.Extra.Package_.ArtistName, "Olivia Ruiz")
	assert.Equal(t, res.Extra.Package_.Name, "Chocolat Show")
	assert.Equal(t, res.Extra.Package_.ReleaseYear, time.Time{})
}

func TestExtraParsingWithPackageFolderWithArtistFolderWithExtraFolder(t *testing.T) {
	config := getTestConfig()
	path := "/videos/Olivia Ruiz/Chocolat Show/Extras/Interview-interview.mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Nil(t, err)
	assert.Nil(t, res.Movie)
	assert.Equal(t, res.Extra.ArtistName, "Olivia Ruiz")
	assert.Equal(t, res.Extra.DiscIndex, 0)
	assert.Equal(t, res.Extra.TrackIndex, 0)
	assert.Equal(t, res.Extra.Name, "Interview")
	assert.Equal(t, res.Extra.Types, []models.ExtraType{models.Interview})
	assert.Equal(t, res.Extra.Package_.ArtistName, "Olivia Ruiz")
	assert.Equal(t, res.Extra.Package_.Name, "Chocolat Show")
	assert.Equal(t, res.Extra.Package_.ReleaseYear, time.Time{})
}

func TestExtraParsingWithPackageFolderWithoutArtistFolderWithIndexesWithoutTypes(t *testing.T) {
	config := getTestConfig()
	path := "/videos/Artist - Package (2000)/1-02 Extra1.mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Nil(t, err)
	assert.Nil(t, res.Movie)
	assert.Equal(t, res.Extra.ArtistName, "Artist")
	assert.Equal(t, res.Extra.DiscIndex, 1)
	assert.Equal(t, res.Extra.TrackIndex, 2)
	assert.Equal(t, res.Extra.Name, "Extra1")
	assert.Equal(t, res.Extra.Types, []models.ExtraType{models.Other})
	assert.Equal(t, res.Extra.Package_.ArtistName, "Artist")
	assert.Equal(t, res.Extra.Package_.Name, "Package")
	assert.Equal(t, res.Extra.Package_.ReleaseYear.Year(), 2000)
}

func TestExtraWithoutDisc(t *testing.T) {
	config := getTestConfig()
	path := "/videos/Artist - Package (2000)/02 Extra1.mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Nil(t, err)
	assert.Nil(t, res.Movie)
	assert.Equal(t, res.Extra.ArtistName, "Artist")
	assert.Equal(t, res.Extra.DiscIndex, 0)
	assert.Equal(t, res.Extra.TrackIndex, 2)
	assert.Equal(t, res.Extra.Name, "Extra1")
	assert.Equal(t, res.Extra.Types, []models.ExtraType{models.Other})
	assert.Equal(t, res.Extra.Package_.ArtistName, "Artist")
	assert.Equal(t, res.Extra.Package_.Name, "Package")
	assert.Equal(t, res.Extra.Package_.ReleaseYear.Year(), 2000)
}
