package src

import (
	"errors"
	"time"

	"github.com/Arthi-chaud/Blee/scanner/src"
	models "github.com/Arthi-chaud/Blee/scanner/src/models"
)

// Metadata Extracted from the path of a file
type PackageMetadataFromPath struct {
	// Name of the package
	// Required
	name string
	// Name of the artist of the package
	// Optional
	artist_name string
	// Release Year of the package
	// Required
	release_year time.Time
}

// Metadata Extracted from the path of an Extra file
type ExtraMetadataFromPath struct {
	// Name of the extra
	// Required
	name string
	// Name of the artist of the extra
	// Required
	artist_name string
	// Index of the disc of the extra
	// Optional
	disc_index int32
	// Index of the track of the extra
	// Optional
	track_index int32
	// Types of the extra
	// Cannot be empty
	types    [](models.ExtraType)
	package_ PackageMetadataFromPath
}

// Metadata Extracted from the path of a Movie file
type MovieMetadataFromPath struct {
	// Name of the movie
	// Required
	name string
	// Name of the artist of the movie
	// Required
	artist_name string
	// Type of movie
	type_    models.MovieType
	package_ PackageMetadataFromPath
}

type PathMetadataParsingResult struct {
	movie *MovieMetadataFromPath
	extra *ExtraMetadataFromPath
}

func ParseMetadataFromPath(filePath string, userConfig *src.UserConfiguration) (PathMetadataParsingResult, error) {
	return PathMetadataParsingResult{}, errors.New("Not Implemented")
}
