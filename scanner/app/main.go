package main

import (
	"os"
	"slices"

	"github.com/Arthi-chaud/Blee/scanner/pkg"
	"github.com/Arthi-chaud/Blee/scanner/pkg/actions"
	"github.com/Arthi-chaud/Blee/scanner/pkg/api"
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	wa "github.com/Arthi-chaud/Blee/scanner/pkg/watcher"
	"github.com/fsnotify/fsnotify"
	"github.com/kpango/glg"
)

func setupLogger() {
	glg.Get().
		SetMode(glg.STD).
		// We will be watching the logs through docker-compose
		// It already provides timestamps
		DisableTimestamp().
		SetLineTraceMode(glg.TraceLineNone)
}

func main() {
	setupLogger()
	c := config.GetConfig()
	w, err := fsnotify.NewWatcher()
	if err != nil {
		glg.Fatalf("Failed setting up API: %s", err)
		os.Exit(1)
	}
	if err := api.HealthCheck(c); err != nil {
		glg.Fatalf("Failed connecting to API: %s", err)
		os.Exit(1)
	}
	go func() {
		for {
			select {
			case event := <-w.Events:
				wa.HandleWatcherEvent(&event, &c)
			case err := <-w.Errors:
				glg.Fatalf("File System Watcher errored: %s", err)
				os.Exit(1)
			}
		}
	}()
	glg.Logf("Attempting to watch %s", c.WatchDir)

	watchedFiles := wa.SetupWatcher(w, c.WatchDir)
	knownPaths, err := api.GetAllKnownPaths(c)
	if err != nil {
		glg.Fatalf("Could not get registered files from API: %s", err)
		os.Exit(1)
	}
	for _, path := range watchedFiles {
		if !slices.Contains(knownPaths, path) && pkg.FileIsVideo(path) {
			actions.RegisterFile(path, &c)
		}
	}
	for _, path := range knownPaths {
		if !slices.Contains(watchedFiles, path) && pkg.FileIsVideo(path) {
			actions.DeleteFile(path, &c)
		}
	}
	glg.Log("Scanner started! Let's get this show on the road.")

	<-make(chan struct{})
}
