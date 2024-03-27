package src

import (
	"errors"
	"regexp"
	"strconv"
	"strings"
	"time"

	"github.com/Arthi-chaud/Blee/scanner/src"
	models "github.com/Arthi-chaud/Blee/scanner/src/models"
	"github.com/kpango/glg"
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
	for _, trackRegex := range userConfig.Regexes.Extra {
		regex := regexp.MustCompile(trackRegex)
		matches := regex.FindStringSubmatch(filePath)
		if len(matches) == 0 {
			continue
		}
		return PathMetadataParsingResult{extra: parseExtraMetadataFromMatches(matches, regex)}, nil
	}
	for _, movieRegex := range userConfig.Regexes.Movie {
		regex := regexp.MustCompile(movieRegex)
		matches := regex.FindStringSubmatch(filePath)
		if len(matches) == 0 {
			continue
		}
		glg.Debugf("%s %s", movieRegex, filePath)
		return PathMetadataParsingResult{movie: parseMovieMetadataFromMatches(matches, regex)}, nil
	}
	return PathMetadataParsingResult{}, errors.New("No match")
}

func parseMovieMetadataFromMatches(matches []string, regex *regexp.Regexp) *MovieMetadataFromPath {
	res := MovieMetadataFromPath{}

	if artist_index := regex.SubexpIndex("Artist"); artist_index != -1 {
		res.artist_name = matches[artist_index]
	} else if package_artist := regex.SubexpIndex("PackageArtist"); package_artist != -1 {
		res.artist_name = matches[package_artist]
	}
	res.name = matches[regex.SubexpIndex("Movie")]
	res.type_ = parseMovieTypeFromName(res.name)
	res.package_.name = matches[regex.SubexpIndex("Package")]
	parseYearFromRegex(matches, regex, &res.package_)
	res.package_.artist_name = matches[regex.SubexpIndex("PackageArtist")]
	if len(res.package_.artist_name) == 0 {
		res.package_.artist_name = res.artist_name
	}
	return &res
}

func parseYearFromRegex(matches []string, regex *regexp.Regexp, p *PackageMetadataFromPath) {
	for _, group_name := range []string {"PackageYear", "Year"} {
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

func parseExtraMetadataFromMatches(matches []string, regex *regexp.Regexp) *ExtraMetadataFromPath {
	res := ExtraMetadataFromPath{}

	if artist_index := regex.SubexpIndex("Artist"); artist_index != -1 {
		res.artist_name = matches[artist_index]
	} else if package_artist := regex.SubexpIndex("PackageArtist"); package_artist != -1 {
		res.artist_name = matches[package_artist]
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
