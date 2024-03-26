package src

import (
	"errors"
	"github.com/Arthi-chaud/Blee/scanner/src"
	models "github.com/Arthi-chaud/Blee/scanner/src/models"
	"regexp"
	"strconv"
	"strings"
	"time"
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
	disc_index int
	// Index of the track of the extra
	// Optional
	track_index int
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
	for _, trackRegex := range userConfig.Regexes.Movie {
		regex := regexp.MustCompile(trackRegex)
		matches := regex.FindStringSubmatch(filePath)
		if len(matches) == 0 {
			continue
		}
		return PathMetadataParsingResult{movie: parseMovieMetadataFromMatches(matches, regex)}, nil
	}
	for _, trackRegex := range userConfig.Regexes.Extra {
		regex := regexp.MustCompile(trackRegex)
		matches := regex.FindStringSubmatch(filePath)
		if len(matches) == 0 {
			continue
		}
		return PathMetadataParsingResult{extra: parseExtraMetadataFromMatches(matches, regex)}, nil
	}
	return PathMetadataParsingResult{}, errors.New("No match")
}

func parseMovieMetadataFromMatches(matches []string, regex *regexp.Regexp) *MovieMetadataFromPath {
	res := MovieMetadataFromPath{}

	res.artist_name = matches[regex.SubexpIndex("Artist")]
	if len(res.artist_name) == 0 {
		res.artist_name = matches[regex.SubexpIndex("PackageArtist")]
	}
	res.name = matches[regex.SubexpIndex("Movie")]
	res.type_ = parseMovieTypeFromName(res.name)
	res.package_.name = matches[regex.SubexpIndex("Package")]
	parsed_date, err := time.Parse(time.RFC3339, matches[regex.SubexpIndex("Package")])
	if err == nil {
		res.package_.release_year = parsed_date
	}
	res.package_.artist_name = matches[regex.SubexpIndex("PackageArtist")]
	if len(res.package_.artist_name) == 0 {
		res.package_.artist_name = res.artist_name
	}
	return &res
}

func parseMovieTypeFromName(movieName string) models.MovieType {
	movieName = strings.ToLower(movieName)
	if strings.Contains(movieName, "live") || strings.Contains(movieName, "concert") || strings.Contains(movieName, "tour") || strings.Contains(movieName, "show") {
		return models.Concert
	}
	return models.Documentary
}

func parseExtraMetadataFromMatches(matches []string, regex *regexp.Regexp) *ExtraMetadataFromPath {
	res := ExtraMetadataFromPath{}

	res.artist_name = matches[regex.SubexpIndex("Artist")]
	if len(res.artist_name) == 0 {
		res.artist_name = matches[regex.SubexpIndex("PackageArtist")]
	}
	disc_index, err := strconv.Atoi(matches[regex.SubexpIndex("Disc")])
	if err == nil {
		res.disc_index = disc_index
	}
	index, err := strconv.Atoi(matches[regex.SubexpIndex("Index")])
	if err == nil {
		res.track_index = index
	}
	res.name = matches[regex.SubexpIndex("Extra")]
	res.types = []models.ExtraType{parseExtraTypeFromPlexRegexGroup(matches[regex.SubexpIndex("PlexExtraType")])}

	res.package_.name = matches[regex.SubexpIndex("Package")]
	parsed_date, err := time.Parse(time.RFC3339, matches[regex.SubexpIndex("Package")])
	if err == nil {
		res.package_.release_year = parsed_date
	}
	res.package_.artist_name = matches[regex.SubexpIndex("PackageArtist")]
	if len(res.package_.artist_name) == 0 {
		res.package_.artist_name = res.artist_name
	}
	return &res
}

func parseExtraTypeFromPlexRegexGroup(group string) models.ExtraType {
	//src: https://support.plex.tv/articles/local-files-for-trailers-and-extras/
	group = strings.ToLower(group)
	switch {
	case group == "behindthescenes":
		return models.BehindTheScenes
	case group == "deleted":
		return models.Other
	case group == "featurette":
		return models.Other
	case group == "interview":
		return models.Interview
	case group == "scene":
		return models.Performance
	case group == "trailer":
		return models.Trailer
	}
	return models.Other
}
