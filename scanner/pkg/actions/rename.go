package actions

import (
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/kpango/glg"
)

// Rename File in API
func RenameFile(oldPath string, newPath string, c* config.Config) {
	glg.Logf("File to rename %s (new path: %s)", oldPath, newPath)
}