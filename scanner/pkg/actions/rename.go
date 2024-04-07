package actions

import (
	"github.com/Arthi-chaud/Blee/scanner/pkg/api"
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/Arthi-chaud/Blee/scanner/pkg/models"
	"github.com/kpango/glg"
)

// Rename File in API
func RenameFile(oldPath string, newPath string, c *config.Config) {
	file, err := api.GetFileByPath(oldPath, *c)
	if err != nil {
		glg.Fail("Could not find File from API.")
		glg.Fail(err.Error())
		return
	}
	err = api.UpdateFile(file.Id, &models.UpdateFileDto{ Path: newPath }, *c)
	if err != nil {
		glg.Fail("Could not update File.")
		glg.Fail(err.Error())
	}
	
}
