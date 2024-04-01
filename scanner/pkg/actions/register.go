package actions

import (
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/kpango/glg"
)

// Register File to API
func RegisterFile(path string, c* config.Config) {
	glg.Logf("File to scan %s", path)
}