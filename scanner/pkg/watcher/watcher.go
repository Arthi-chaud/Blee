package watcher

import (
	"os"
	"path"

	"github.com/fsnotify/fsnotify"
	"github.com/kpango/glg"
)

// Configure the watcher so that it watches all the sub/directories
// Returns all the watched files
func SetupWatcher(w *fsnotify.Watcher, rootDir string) []string {
	watchedFiles := []string{}
	if err := w.Add(rootDir); err != nil {
		glg.Fatalf("Could not watch: %s", err)
		os.Exit(1)
	}
	entries, _ := os.ReadDir(rootDir)

	for _, e := range entries {
		entryName := path.Join(rootDir, e.Name())
		if e.IsDir() {
			glg.Logf("Watching directory %s", entryName)
			watchedFiles = append(watchedFiles, SetupWatcher(w, entryName)...)
		} else {
			watchedFiles = append(watchedFiles, entryName)
		}
	}
	return watchedFiles
}
