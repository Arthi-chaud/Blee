package watcher

import (
	"github.com/Arthi-chaud/Blee/scanner/pkg"
	"github.com/Arthi-chaud/Blee/scanner/pkg/actions"
	"os"

	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	fsnotify "github.com/fsnotify/fsnotify"
	"github.com/kpango/glg"
)

func HandleWatcherEvent(event *fsnotify.Event, c *config.Config) {
	file, err := os.Open(event.Name)
	if err != nil {
		glg.Failf("Error opening file: %s", err)
		return
	}
	defer file.Close()
	fstat, err := file.Stat()

	if fstat.IsDir() {
		//TODO Check if every file in a deleted folder will trigger an event
		return
	}

	if event.Has(fsnotify.Create) {
		if pkg.FileIsVideo(event.Name) {
			actions.RegisterFile(event.Name, c)
		}
	} else if event.Has(fsnotify.Remove) {
		if pkg.FileIsVideo(event.Name) {
			actions.DeleteFile(event.Name, c)
		}
	} else if event.Has(fsnotify.Rename) {
		// actions.RenameFile(event.OldPath, event.Path, c)
	} else if event.Has(fsnotify.Write) {
		// TODO See #36
	}
}
