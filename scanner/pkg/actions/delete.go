package actions

import (
	"path/filepath"

	"github.com/Arthi-chaud/Blee/scanner/pkg/api"
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/kpango/glg"
)

// Delete File from API
func DeleteFile(path string, c *config.Config) {
	file, err := api.GetFileByPath(path, *c)
	if err != nil {
		glg.Fail("Getting File entry from API failed.")
		glg.Fail(err.Error())
		return
	}
	if err = api.DeleteFile(file.Id, *c); err != nil {
		glg.Fail("Deleting File entry from API failed.")
		glg.Fail(err.Error())
		return
	}
	glg.Successf("File '%s' deleted", filepath.Base(path))
}
