package watcher

import (
	"github.com/Arthi-chaud/Blee/scanner/pkg"
	"github.com/Arthi-chaud/Blee/scanner/pkg/actions"
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	w "github.com/radovskyb/watcher"
)

func HandleWatcherEvent(event *w.Event, c *config.Config) {
	if event.IsDir() {
		//TODO Check if every file in a deleted folder will trigger an event
		return
	}
	switch event.Op {
	case w.Create:
		if pkg.FileIsVideo(event.Path) {
			actions.RegisterFile(event.Path, c)
		}
	case w.Remove:
		actions.DeleteFile(event.Path, c)
	case w.Rename:
		actions.RenameFile(event.OldPath, event.Path, c)
	case w.Move:
		actions.RenameFile(event.OldPath, event.Path, c)
	case w.Write:
		// TODO See #36
	}
}
