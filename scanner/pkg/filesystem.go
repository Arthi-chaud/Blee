// Get All Files in watched directory
package pkg

import (
	"os"
	"path"

	"github.com/kpango/glg"
)

func GetWatchedFiles(rootDir string) []string {
	watchedFiles := []string{}
	entries, err := os.ReadDir(rootDir)

	if err != nil {
		glg.Fatal(err)
	}

	for _, e := range entries {
		entryName := path.Join(rootDir, e.Name())
		i, err := os.Stat(entryName)
		if err != nil {
			glg.Fail(err)
			return []string{}
		}
		if i.IsDir() {
			watchedFiles = append(watchedFiles, GetWatchedFiles(entryName)...)
		} else {
			watchedFiles = append(watchedFiles, entryName)
		}
	}
	return watchedFiles
}
