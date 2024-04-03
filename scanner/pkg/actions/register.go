package actions

import (
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/Arthi-chaud/Blee/scanner/pkg/parser"
	"github.com/kpango/glg"
)

// Register File to API
func RegisterFile(path string, c *config.Config) error {
	glg.Logf("File to scan %s", path)
	parsedPath, err := parser.ParseMetadataFromPath(path, &c.UserConfig)
	if parsedPath.Movie != nil {
		glg.Logf("Parsed path: %s", parsedPath.Movie.Name)
	} else if parsedPath.Extra != nil {
		glg.Logf("Parsed path: %s", parsedPath.Extra.Name)
	}
	res, err := parser.GetMediaInfo(path)
	glg.Logf("Mediainfo: %s", res)

	return err
}
