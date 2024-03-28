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
	assert.Nil(t, res.extra)
	assert.Equal(t, "The Corrs", res.movie.artist_name)
	assert.Equal(t, "Unplugged", res.movie.name)
	assert.Equal(t, models.Concert, res.movie.type_)
	assert.Equal(t, "The Corrs", res.movie.package_.artist_name)
	assert.Equal(t, "Unplugged", res.movie.package_.name)
	assert.Equal(t, 1999, res.movie.package_.release_year.Year())
}

func TestMovieParsingWithPackageFolderWithoutYear(t *testing.T) {
	config := getTestConfig()
	path := "/videos/The Corrs - Unplugged/The Corrs - Unplugged.mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Nil(t, err)
	assert.Nil(t, res.extra)
	assert.Equal(t, res.movie.artist_name, "The Corrs")
	assert.Equal(t, res.movie.name, "Unplugged")
	assert.Equal(t, res.movie.type_, models.Concert)
	assert.Equal(t, res.movie.package_.artist_name, "The Corrs")
	assert.Equal(t, res.movie.package_.name, "Unplugged")
	assert.Equal(t, res.movie.package_.release_year, time.Time{})
}

func TestMovieParsingWithPackageFolderWithArtistFolderWithMovieYear(t *testing.T) {
	config := getTestConfig()
	path := "/videos/The Corrs/Unplugged (1999)/The Corrs - Unplugged.mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Nil(t, err)
	assert.Nil(t, res.extra)
	assert.Equal(t, res.movie.artist_name, "The Corrs")
	assert.Equal(t, res.movie.name, "Unplugged")
	assert.Equal(t, res.movie.type_, models.Concert)
	assert.Equal(t, res.movie.package_.artist_name, "The Corrs")
	assert.Equal(t, res.movie.package_.name, "Unplugged")
	assert.Equal(t, res.movie.package_.release_year.Year(), 1999)
}

func TestExtraParsingWithPackageFolderWithoutArtistFolder(t *testing.T) {
	config := getTestConfig()
	path := "/videos/Olivia Ruiz - Miss Météores Live (2008)/Autour D'Olivia (Documentaire)-interview.mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Nil(t, err)
	assert.Nil(t, res.movie)
	assert.Equal(t, res.extra.artist_name, "Olivia Ruiz")
	assert.Equal(t, res.extra.disc_index, 0)
	assert.Equal(t, res.extra.track_index, 0)
	assert.Equal(t, res.extra.name, "Autour D'Olivia (Documentaire)")
	assert.Equal(t, res.extra.types, []models.ExtraType{models.ExtraType(models.Documentary)})
	assert.Equal(t, res.extra.package_.artist_name, "Olivia Ruiz")
	assert.Equal(t, res.extra.package_.name, "Miss Météores Live")
	assert.Equal(t, res.extra.package_.release_year.Year(), 2008)
}

func TestExtraParsingWithPackageFolderWithArtistFolder(t *testing.T) {
	config := getTestConfig()
	path := "/videos/Olivia Ruiz/Chocolat Show/Interview-interview.mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Nil(t, err)
	assert.Nil(t, res.movie)
	assert.Equal(t, res.extra.artist_name, "Olivia Ruiz")
	assert.Equal(t, res.extra.disc_index, 0)
	assert.Equal(t, res.extra.track_index, 0)
	assert.Equal(t, res.extra.name, "Interview")
	assert.Equal(t, res.extra.types, []models.ExtraType{models.Interview})
	assert.Equal(t, res.extra.package_.artist_name, "Olivia Ruiz")
	assert.Equal(t, res.extra.package_.name, "Chocolat Show")
	assert.Equal(t, res.extra.package_.release_year, time.Time{})
}

func TestExtraParsingWithPackageFolderWithArtistFolderWithExtraFolder(t *testing.T) {
	config := getTestConfig()
	path := "/videos/Olivia Ruiz/Chocolat Show/Extras/Interview-interview.mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Nil(t, err)
	assert.Nil(t, res.movie)
	assert.Equal(t, res.extra.artist_name, "Olivia Ruiz")
	assert.Equal(t, res.extra.disc_index, 0)
	assert.Equal(t, res.extra.track_index, 0)
	assert.Equal(t, res.extra.name, "Interview")
	assert.Equal(t, res.extra.types, []models.ExtraType{models.Interview})
	assert.Equal(t, res.extra.package_.artist_name, "Olivia Ruiz")
	assert.Equal(t, res.extra.package_.name, "Chocolat Show")
	assert.Equal(t, res.extra.package_.release_year, time.Time{})
}

func TestExtraParsingWithPackageFolderWithoutArtistFolderWithIndexesWithoutTypes(t *testing.T) {
	config := getTestConfig()
	path := "/videos/Artist - Package (2000)/1-02 Extra1.mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Nil(t, err)
	assert.Nil(t, res.movie)
	assert.Equal(t, res.extra.artist_name, "Artist")
	assert.Equal(t, res.extra.disc_index, 1)
	assert.Equal(t, res.extra.track_index, 2)
	assert.Equal(t, res.extra.name, "Extra1")
	assert.Equal(t, res.extra.types, []models.ExtraType{models.Other})
	assert.Equal(t, res.extra.package_.artist_name, "Artist")
	assert.Equal(t, res.extra.package_.name, "Package")
	assert.Equal(t, res.extra.package_.release_year.Year(), 2000)
}

func TestExtraWithoutDisc(t *testing.T) {
	config := getTestConfig()
	path := "/videos/Artist - Package (2000)/02 Extra1.mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Nil(t, err)
	assert.Nil(t, res.movie)
	assert.Equal(t, res.extra.artist_name, "Artist")
	assert.Equal(t, res.extra.disc_index, 0)
	assert.Equal(t, res.extra.track_index, 2)
	assert.Equal(t, res.extra.name, "Extra1")
	assert.Equal(t, res.extra.types, []models.ExtraType{models.Other})
	assert.Equal(t, res.extra.package_.artist_name, "Artist")
	assert.Equal(t, res.extra.package_.name, "Package")
	assert.Equal(t, res.extra.package_.release_year.Year(), 2000)
}
