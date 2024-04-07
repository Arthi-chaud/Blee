package watcher

import (
	"github.com/Arthi-chaud/Blee/scanner/pkg"
	"github.com/Arthi-chaud/Blee/scanner/pkg/actions"
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/kpango/glg"
	w "github.com/radovskyb/watcher"
)

func HandleWatcherEvent(event *w.Event, c *config.Config) {
	glg.Fail(event)
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
		if pkg.FileIsVideo(event.Path) {
			actions.DeleteFile(event.Path, c)
		}
	case w.Rename:
		actions.RenameFile(event.OldPath, event.Path, c)
	case w.Move:
		actions.RenameFile(event.OldPath, event.Path, c)
	case w.Write:
		// TODO See #36
	}
}
