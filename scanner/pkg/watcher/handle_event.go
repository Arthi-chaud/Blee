package watcher

import (
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	"github.com/kpango/glg"
	w "github.com/radovskyb/watcher"
)

func HandleWatcherEvent(event *w.Event, c *config.Config) {
	glg.Println(event)
}
