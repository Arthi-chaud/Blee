package parser

import (
	"errors"
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/Arthi-chaud/Blee/scanner/pkg/models"
	validator "github.com/go-playground/validator/v10"
	"regexp"
	"strconv"
	"strings"
	"time"
)

// Metadata Extracted from the path of a file
type PackageMetadataFromPath struct {
	// Name of the package
	name string `validate:"required"`
	// Name of the artist of the package
	// Optional
	artist_name string
	// Release Year of the package
	release_year time.Time
}

// Metadata Extracted from the path of an Extra file
type ExtraMetadataFromPath struct {
	// Name of the extra
	name string `validate:"required"`
	// Name of the artist of the extra
	artist_name string `validate:"required"`
	// Index of the disc of the extra
	disc_index int `validate:"gte=0"`
	// Index of the track of the extra
	track_index int `validate:"gte=0"`
	// Types of the extra
	types    [](models.ExtraType)    `validate:"min=1"`
	package_ PackageMetadataFromPath `validate:"required,dive,required"`
}

// Metadata Extracted from the path of a Movie file
type MovieMetadataFromPath struct {
	// Name of the movie
	name string `validate:"required"`
	// Name of the artist of the movie
	artist_name string `validate:"required"`
	// Type of movie
	type_    models.MovieType        `validate:"required"`
	package_ PackageMetadataFromPath `validate:"required,dive,required"`
}

type PathMetadataParsingResult struct {
	movie *MovieMetadataFromPath
	extra *ExtraMetadataFromPath
}

func ParseMetadataFromPath(filePath string, userConfig *config.UserConfiguration) (PathMetadataParsingResult, error) {
	for _, trackRegex := range userConfig.Regexes.Extra {
		regex := regexp.MustCompile(trackRegex)
		matches := regex.FindStringSubmatch(filePath)
		if len(matches) == 0 {
			continue
		}
		extra, err := parseExtraMetadataFromMatches(matches, regex)
		return PathMetadataParsingResult{extra: extra}, err
	}
	for _, movieRegex := range userConfig.Regexes.Movie {
		regex := regexp.MustCompile(movieRegex)
		matches := regex.FindStringSubmatch(filePath)
		if len(matches) == 0 {
			continue
		}
		movie, err := parseMovieMetadataFromMatches(matches, regex)
		return PathMetadataParsingResult{movie: movie}, err
	}
	return PathMetadataParsingResult{}, errors.New("No match")
}

func parseMovieMetadataFromMatches(matches []string, regex *regexp.Regexp) (*MovieMetadataFromPath, error) {
	res := MovieMetadataFromPath{}

	if artist_index := regex.SubexpIndex("Artist"); artist_index != -1 {
		res.artist_name = matches[artist_index]
		res.package_.artist_name = res.artist_name
	}
	if package_artist := regex.SubexpIndex("PackageArtist"); package_artist != -1 {
		res.package_.artist_name = matches[package_artist]
		if len(res.artist_name) == 0 {
			res.artist_name = res.package_.artist_name
		}
	}
	if name_index := regex.SubexpIndex("Movie"); name_index != -1 {
		res.name = matches[name_index]
	}
	if package_index := regex.SubexpIndex("Package"); package_index != -1 {
		res.package_.name = matches[package_index]
	}
	res.type_ = parseMovieTypeFromName(res.name)
	parseYearFromRegex(matches, regex, &res.package_)
	validate := validator.New(validator.WithRequiredStructEnabled())
	err := validate.Struct(res)
	return &res, err
}

func parseYearFromRegex(matches []string, regex *regexp.Regexp, p *PackageMetadataFromPath) {
	for _, group_name := range []string{"PackageYear", "Year"} {
		if group_index := regex.SubexpIndex(group_name); group_index != -1 {
			if parsed_year, err := strconv.Atoi(matches[group_index]); err == nil {
				p.release_year = time.Date(parsed_year, 1, 1, 1, 1, 1, 1, time.UTC)
				return
			}
		}
	}
}

func parseMovieTypeFromName(movieName string) models.MovieType {
	movieName = strings.ToLower(movieName)
	if strings.Contains(movieName, "live") ||
		strings.Contains(movieName, "concert") ||
		strings.Contains(movieName, "tour") ||
		strings.Contains(movieName, "show") ||
		strings.Contains(movieName, "unplugged") {
		return models.Concert
	}
	return models.Documentary
}

func parseExtraMetadataFromMatches(matches []string, regex *regexp.Regexp) (*ExtraMetadataFromPath, error) {
	res := ExtraMetadataFromPath{}

	if artist_index := regex.SubexpIndex("Artist"); artist_index != -1 {
		res.artist_name = matches[artist_index]
		res.package_.artist_name = res.artist_name
	}
	if package_artist := regex.SubexpIndex("PackageArtist"); package_artist != -1 {
		res.package_.artist_name = matches[package_artist]
		if len(res.artist_name) == 0 {
			res.artist_name = res.package_.artist_name
		}
	}
	if disc_index, err := strconv.Atoi(matches[regex.SubexpIndex("Disc")]); err == nil {
		res.disc_index = disc_index
	}
	if index, err := strconv.Atoi(matches[regex.SubexpIndex("Index")]); err == nil {
		res.track_index = index
	}
	if name_index := regex.SubexpIndex("Extra"); name_index != -1 {
		res.name = matches[name_index]
	}
	if plexExtraType := parseExtraTypeFromPlexRegexGroup(matches[regex.SubexpIndex("PlexExtraType")]); plexExtraType != models.Other {
		res.types = []models.ExtraType{plexExtraType}
	}
	if extraType := parseExtraTypeFromName(res.name); extraType != models.Other {
		res.types = append(res.types, extraType)
	}
	if len(res.types) == 0 {
		res.types = append(res.types, models.Other)
	}
	parseYearFromRegex(matches, regex, &res.package_)
	if package_index := regex.SubexpIndex("Package"); package_index != -1 {
		res.package_.name = matches[package_index]
	}
	validate := validator.New(validator.WithRequiredStructEnabled())
	err := validate.Struct(res)
	return &res, err
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

func parseExtraTypeFromName(extraName string) models.ExtraType {
	extraName = strings.ToLower(extraName)
	if strings.Contains(extraName, "document") {
		return models.ExtraType(models.Documentary)
	}
	if strings.Contains(extraName, "music video") {
		return models.ExtraType(models.MusicVideo)
	}
	if strings.Contains(extraName, " angle") {
		return models.ExtraType(models.AlternateView)
	}
	if strings.Contains(extraName, "backdrop") ||
		strings.Contains(extraName, "visuals") {
		return models.ExtraType(models.Backdrops)
	}
	if strings.Contains(extraName, "trailer") ||
		strings.Contains(extraName, "teaser") {
		return models.ExtraType(models.Trailer)
	}
	if strings.Contains(extraName, "making of") ||
		strings.Contains(extraName, "behind the") ||
		strings.Contains(extraName, "making the") {
		return models.ExtraType(models.BehindTheScenes)
	}
	if strings.Contains(extraName, "interview") ||
		strings.Contains(extraName, "chat ") ||
		strings.HasSuffix(extraName, " chat") {
		return models.ExtraType(models.Interview)
	}
	if strings.Contains(extraName, "interview") ||
		strings.Contains(extraName, "chat ") ||
		strings.HasSuffix(extraName, " chat") {
		return models.ExtraType(models.Interview)
	}
	if strings.Contains(extraName, "video") {
		return models.ExtraType(models.MusicVideo)
	}
	if strings.Contains(extraName, "live") {
		return models.ExtraType(models.Performance)
	}
	return models.Other
}
