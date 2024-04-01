package actions

import (
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/kpango/glg"
)

// Delete File from API
func DeleteFile(path string, c* config.Config) {
	glg.Logf("File to delete %s", path)
}