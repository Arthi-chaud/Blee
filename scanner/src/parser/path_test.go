package src

import (
	"testing"
	"time"

	src "github.com/Arthi-chaud/Blee/scanner/src"
	models "github.com/Arthi-chaud/Blee/scanner/src/models"
	"github.com/stretchr/testify/assert"
)

func getTestConfig() *src.UserConfiguration {
	var config src.UserConfiguration

	config.Regexes.Extra = []string{
		"(?!^)[\\/\\\\]((?P<PackageArtist>[^\\/\\\\]+)((\\s+-\\s+)|([\\/\\\\]+))(([^\\/\\\\]+)\\s+-\\s+)?((?P<Package>[^\\/\\\\]+?)))(\\s+\\((?P<PackageYear>\\d+)\\))?[\\/\\\\]+(Extra(s)?[\\/\\\\]+)?(((?P<Disc>\\d+)-)?(?P<Index>\\d+))?(?P<Extra>(?!\\1)(?!\\8)[^\\/\\\\]+?)(?P<PlexExtraType>-[a-z]+)?\\..+",
	}
	config.Regexes.Movie = []string{
		"(?!^)[\\/\\\\](?P<PackageArtist>[^\\/\\\\]+)((\\s+-\\s+)|([\\/\\\\]+))(([^\\/\\\\]+)\\s+-\\s+)?(?P<Package>[^\\/\\\\]+?)(\\s+\\((?P<PackageYear>\\d+)\\))?[\\/\\\\]+((?P<Artist>[^\\/\\\\]+)\\s+-\\s+)?(?P<Movie>[^\\/\\\\]+?)(\\s+\\((?P<Year>\\d+)\\))?\\..+",
		"(?!^)[\\/\\\\](?P<PackageArtist>(?P<Artist>[^\\/\\\\]+))((\\s+-\\s+)|([\\/\\\\]+))(?P<Package>(?P<Movie>[^\\/\\\\]+?))(\\s+\\((?P<PackageYear>(?P<Year>\\d+))\\))?\\..+",
	}
	return &config
}

func TestMovieParsingWithoutPackageFolderWithYear(t *testing.T) {
	config := getTestConfig()
	path := "/videos/The Corrs - Unplugged (1999).mkv"
	res, err := ParseMetadataFromPath(path, config)

	assert.Equal(t, err, nil, "Parsing Failed")
	assert.Equal(t, res.extra, nil, "Expected Movie, got extra")
	assert.Equal(t, res.movie.artist_name, "The Corrs", "Wrong Artist Name")
	assert.Equal(t, res.movie.name, "Unplugged", "Wrong Movie Name")
	assert.Equal(t, res.movie.type_, models.Concert, "Wrong Movie Type")
	assert.Equal(t, res.movie.package_.artist_name, "The Corrs")
	assert.Equal(t, res.movie.package_.name, "Unplugged")
	assert.Equal(t, res.movie.package_.release_year.Year(), 1999)
}


