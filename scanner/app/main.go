package main

import (
	"os"
	"slices"
	"time"

	"github.com/Arthi-chaud/Blee/scanner/pkg/api"
	"github.com/Arthi-chaud/Blee/scanner/pkg/config"
	wa "github.com/Arthi-chaud/Blee/scanner/pkg/watcher"
	"github.com/kpango/glg"
	"github.com/radovskyb/watcher"
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
	w := watcher.New()
	if err := api.HealthCheck(c); err != nil {
		glg.Fatalf("Failed connecting to API: %s", err)
		os.Exit(1)
	}
	go func() {
		for {
			select {
			case event := <-w.Event:
				wa.HandleWatcherEvent(&event, &c)
			case err := <-w.Error:
				glg.Fatalf("File System Watcher errored: %s", err)
				os.Exit(1)
			case <-w.Closed:
				return
			}
		}
	}()
	glg.Logf("Attempting to watch %s", c.WatchDir)

	if err := w.AddRecursive(c.WatchDir); err != nil {
		glg.Fatalf("Could not watch: %s", err)
		os.Exit(1)
	}
	knownPaths, err := api.GetAllKnownPaths(c)
	if err != nil {
		glg.Fatalf("Could not get registered files from API: %s", err)
		os.Exit(1)
	}
	watchedFiles := []string{}
	// Handle files that are currently in the file system
	for _, watchedFile := range w.WatchedFiles() {
		if !watchedFile.IsDir() {
			watchedFiles = append(watchedFiles, watchedFile.Name())
		}
	}
	for _, path := range watchedFiles {
		if !slices.Contains(knownPaths, path) {
			glg.Logf("File to scan %s", path)
		}
	}
	for _, path := range knownPaths {
		if !slices.Contains(watchedFiles, path) {
			glg.Logf("File to delete %s", path)
		}
	}
	glg.Log("Scanner started! Let's get this show on the road.")
	// Check for changes every 10s.
	if err := w.Start(time.Second * 10); err != nil {
		glg.Fatalf("Starting Polling Cycle errored: %s", err)
		os.Exit(1)
	}
}
