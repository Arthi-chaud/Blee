package pkg

import (
	"github.com/gabriel-vasile/mimetype"
	"strings"
)

func FileIsVideo(path string) bool {
	mime, err := mimetype.DetectFile(path)
	if err != nil {
		return false
	}
	return strings.HasPrefix(mime.String(), "video/")
}