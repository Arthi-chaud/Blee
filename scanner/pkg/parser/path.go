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
	Name string `validate:"required"`
	// Name of the artist of the package
	// Optional
	ArtistName string
	// Release Year of the package
	ReleaseYear time.Time
}

// Metadata Extracted from the path of an Extra file
type ExtraMetadataFromPath struct {
	// Name of the extra
	Name string `validate:"required"`
	// Name of the artist of the extra
	ArtistName string `validate:"required"`
	// Index of the disc of the extra
	DiscIndex int `validate:"gte=0"`
	// Index of the track of the extra
	TrackIndex int `validate:"gte=0"`
	// Types of the extra
	Types    [](models.ExtraType)    `validate:"min=1"`
	Package_ PackageMetadataFromPath `validate:"required"`
}

// Metadata Extracted from the path of a Movie file
type MovieMetadataFromPath struct {
	// Name of the movie
	Name string `validate:"required"`
	// Name of the artist of the movie
	ArtistName string `validate:"required"`
	// Type of movie
	Type_    models.MovieType        `validate:"required"`
	Package_ PackageMetadataFromPath `validate:"required"`
}

type PathMetadataParsingResult struct {
	Movie *MovieMetadataFromPath
	Extra *ExtraMetadataFromPath
}

func ParseMetadataFromPath(filePath string, userConfig *config.UserConfiguration) (PathMetadataParsingResult, error) {
	for _, trackRegex := range userConfig.Regexes.Extra {
		regex := regexp.MustCompile(trackRegex)
		matches := regex.FindStringSubmatch(filePath)
		if len(matches) == 0 {
			continue
		}
		extra, err := parseExtraMetadataFromMatches(matches, regex)
		return PathMetadataParsingResult{Movie: nil, Extra: extra}, err
	}
	for _, movieRegex := range userConfig.Regexes.Movie {
		regex := regexp.MustCompile(movieRegex)
		matches := regex.FindStringSubmatch(filePath)
		if len(matches) == 0 {
			continue
		}
		movie, err := parseMovieMetadataFromMatches(matches, regex)
		return PathMetadataParsingResult{Movie: movie, Extra: nil}, err
	}
	return PathMetadataParsingResult{}, errors.New("No match")
}

func parseMovieMetadataFromMatches(matches []string, regex *regexp.Regexp) (*MovieMetadataFromPath, error) {
	res := MovieMetadataFromPath{}

	if artist_index := regex.SubexpIndex("Artist"); artist_index != -1 {
		res.ArtistName = matches[artist_index]
		res.Package_.ArtistName = res.ArtistName
	}
	if package_artist := regex.SubexpIndex("PackageArtist"); package_artist != -1 {
		res.Package_.ArtistName = matches[package_artist]
		if len(res.ArtistName) == 0 {
			res.ArtistName = res.Package_.ArtistName
		}
	}
	if name_index := regex.SubexpIndex("Movie"); name_index != -1 {
		res.Name = matches[name_index]
	}
	if package_index := regex.SubexpIndex("Package"); package_index != -1 {
		res.Package_.Name = matches[package_index]
	}
	res.Type_ = parseMovieTypeFromName(res.Name)
	parseYearFromRegex(matches, regex, &res.Package_)
	validate := validator.New(validator.WithRequiredStructEnabled())
	err := validate.Struct(res)
	return &res, err
}

func parseYearFromRegex(matches []string, regex *regexp.Regexp, p *PackageMetadataFromPath) {
	for _, group_name := range []string{"PackageYear", "Year"} {
		if group_index := regex.SubexpIndex(group_name); group_index != -1 {
			if parsed_year, err := strconv.Atoi(matches[group_index]); err == nil {
				p.ReleaseYear = time.Date(parsed_year, 1, 1, 1, 1, 1, 1, time.UTC)
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
		res.ArtistName = matches[artist_index]
		res.Package_.ArtistName = res.ArtistName
	}
	if package_artist := regex.SubexpIndex("PackageArtist"); package_artist != -1 {
		res.Package_.ArtistName = matches[package_artist]
		if len(res.ArtistName) == 0 {
			res.ArtistName = res.Package_.ArtistName
		}
	}
	if disc_index, err := strconv.Atoi(matches[regex.SubexpIndex("Disc")]); err == nil {
		res.DiscIndex = disc_index
	}
	if index, err := strconv.Atoi(matches[regex.SubexpIndex("Index")]); err == nil {
		res.TrackIndex = index
	}
	if name_index := regex.SubexpIndex("Extra"); name_index != -1 {
		res.Name = matches[name_index]
	}
	if plexExtraType := parseExtraTypeFromPlexRegexGroup(matches[regex.SubexpIndex("PlexExtraType")]); plexExtraType != models.Other {
		res.Types = []models.ExtraType{plexExtraType}
	}
	if extraType := parseExtraTypeFromName(res.Name); extraType != models.Other {
		res.Types = append(res.Types, extraType)
	}
	if len(res.Types) == 0 {
		res.Types = append(res.Types, models.Other)
	}
	parseYearFromRegex(matches, regex, &res.Package_)
	if package_index := regex.SubexpIndex("Package"); package_index != -1 {
		res.Package_.Name = matches[package_index]
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
